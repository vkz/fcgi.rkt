#lang racket/tables

(require bitsyntax
         racket/generic
         racket/struct
         racket/hash)


(module+ test
  (require rackunit
           prelude/testing))


;;* Constants ------------------------------------------------------- *;;


(define FCGI_VERSION_1 1)

;; management records request ID is always 0
(define FCGI_NULL_REQUEST_ID     0)


;; record types
(define FCGI_BEGIN_REQUEST       1)
(define FCGI_ABORT_REQUEST       2)
(define FCGI_END_REQUEST         3)
(define FCGI_PARAMS              4)
(define FCGI_STDIN               5)
(define FCGI_STDOUT              6)
(define FCGI_STDERR              7)
(define FCGI_DATA                8)
(define FCGI_GET_VALUES          9)
(define FCGI_GET_VALUES_RESULT  10)
(define FCGI_UNKNOWN_TYPE       11)
(define FCGI_MAXTYPE FCGI_UNKNOWN_TYPE)


;; mask for flags in begin-request
(define FCGI_KEEP_CONN  1)


;; roles in begin-request
(define FCGI_RESPONDER  1)
(define FCGI_AUTHORIZER 2)
(define FCGI_FILTER     3)


;; protostatus
(define FCGI_REQUEST_COMPLETE 0)
(define FCGI_CANT_MPX_CONN    1)
(define FCGI_OVERLOADED       2)
(define FCGI_UNKNOWN_ROLE     3)


;; names allowed in FCGI_GET_VALUES / FCGI_GET_VALUES_RESULT records
(define FCGI_MAX_CONNS  1000)
(define FCGI_MAX_REQS   10000)
(define FCGI_MPXS_CONNS 1)


;;* Bit twiddling --------------------------------------------------- *;;


(define (empty-bytes? bytes)
  (and (bytes? bytes)
       (equal? #"" bytes)))


(define (pad content)
  (define clen (bytes-length content))
  (define-values (quotient remainder) (quotient/remainder clen 8))
  (define plen (if (zero? quotient) 0 (- (* (add1 quotient) 8) clen)))
  (define padding (make-bytes plen))
  (values plen padding (bytes-append content padding)))


(define (bytes-of thing)
  (cond
    ((bytes?      thing) thing)
    ((bit-string? thing) (bit-string->bytes thing))
    ((string?     thing) (string->bytes/utf-8 thing))
    ((dict?       thing) (pack-name-values thing))
    (else
     (string->bytes/utf-8
      (format "~a" thing)))))


(define (pack-length len)
    (cond
      ;; fits into 1 byte with high bit unset (i.e. 7 bits)
      ((len . < . (arithmetic-shift 1 7)) (integer->integer-bytes len 1 false true))
      ;; fits into 4 bytes with high bit set (i.e. 31 bits)
      ((len . < . (arithmetic-shift 1 31)) (bit-string->bytes (bit-string (1 :: bits 1) (len :: bits 31))))
      (else (error "too long to pack"))))


(define (pack-name-value name value)
  (set! name (bytes-of name))
  (set! value (bytes-of value))
  (define nlen (bytes-length name))
  (define vlen (bytes-length value))
  (bytes-append (pack-length nlen) (pack-length vlen) name value))


(define (pack-name-values table)
  (bytes-append*
   (for/list (((name value) (in-dict table)))
     (pack-name-value name value))))


(module+ test
  (check equal? #"\4\5namevalue"         (pack-name-value "name" "value"))
  (check equal? #"\4\0name"              (pack-name-value "name" ""))
  (check equal? #"\4\1name1"             (pack-name-value "name" 1))
  (check equal? #"\4\3name255"           (pack-name-value "name" 255))
  (check equal? #"\4\3name256"           (pack-name-value "name" 256))
  (check equal? #"\4\5name256.5"         (pack-name-value "name" 256.5))
  (check equal? #"\3\3bazduh\3\3foobar"  (pack-name-values {("foo" "bar")
                                                            ("baz" "duh")})))


(define (parse-name-values stream #:into [table {}])
  (if (empty-bytes? stream)
      table
      (bit-string-case stream
        ([(nlen  :: (fcgi-length))
          (vlen  :: (fcgi-length))
          (name  :: binary bytes nlen)
          (value :: binary bytes vlen)
          (rest  :: binary)]
         (set table
              (bytes->string/utf-8 (bit-string->bytes name))
              (bytes->string/utf-8 (bit-string->bytes value)))
         (parse-name-values rest #:into table)))))


(module+ test
  (check-equal? {("name" "value")} (parse-name-values (pack-name-value "name" "value")))
  (check-equal? {("name" "")}      (parse-name-values (pack-name-value "name" "")))
  (check-equal? {("name" "255")}   (parse-name-values (pack-name-value "name" "255")))
  (check-equal? {("name" "256.5")} (parse-name-values (pack-name-value "name" 256.5)))
  (check-equal? {("foo" "bar") ("baz" "duh")} (parse-name-values
                                                  (bytes-append
                                                   (pack-name-value "foo" "bar")
                                                   (pack-name-value "baz" "duh")))))


;;* Bitsyntax custom parsers ---------------------------------------- *;;


(define-syntax fcgi-header
  (syntax-rules ()

    ;; parse and create a record
    ((_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([version type (id :: bytes 2) (clen :: bytes 2) plen _ (rest :: binary)]
        (ks {(:version version) (:type type) (:id id) (:clen clen) (:plen plen)}
            rest))
       (else (kf))))

    ;; parse into record passed as argument
    ((_ #t input ks kf record)
     (let ((r record))
       (bit-string-case input
         #:on-short (λ (fail) (kf #t))
         ([version type (id :: bytes 2) (clen :: bytes 2) plen _ (rest :: binary)]
          (set r :version version)
          (set r :type type)
          (set r :id id)
          (set r :clen clen)
          (set r :plen plen)
          (ks r rest))
         (else (kf)))))

    ;; unparse header from record
    ((_ #f record)
     (let ((r record))
       (bit-string
        r:version r:type (r:id :: bytes 2) (r:clen :: bytes 2) r:plen #;reserved 0)))))


(module+ test
  (define header {(:version 1) (:type FCGI_BEGIN_REQUEST) (:id 1) (:clen 3) (:plen 5)})

  ;; unparse header
  (define bs (bit-string (header :: (fcgi-header))))
  (check eq? 8 (bit-string-byte-count bs))
  (check equal? '(1 1 0 1 0 3 5 0) (bytes->list (bit-string->bytes bs)))

  ;; (parse (unparse header)) == header
  (check-equal? (bit-string-case (bit-string->bytes bs)
                  ([(header :: (fcgi-header))] header))
                header))


(define-syntax fcgi-length
  (syntax-rules ()

    ;; parse
    ((_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([(= 0 :: bits 1) (len :: integer bits 7) (rest :: binary)]
        (ks len rest))

       ([(= 1 :: bits 1) (len :: integer bits 31) (rest :: binary)]
        (ks len rest))
       (else (kf))))

    ;; unparse
    ((_ #f len)
     (pack-length len))))


(module+ test
  (check eq? 1 (bytes-length (pack-length 127)))
  (check eq? 4 (bytes-length (pack-length 128)))

  (check equal? #"\177" (bit-string (127 :: (fcgi-length))))
  (check eq? 1 (bytes-length (bit-string (127 :: (fcgi-length)))))

  (check equal? #"\200\0\0\200" (bit-string (128 :: (fcgi-length))))
  (check eq? 4 (bytes-length (bit-string (128 :: (fcgi-length)))))

  (check eq? 127 (bit-string-case #"\177"         ([(len :: (fcgi-length))] len)))
  (check eq? 128 (bit-string-case #"\200\0\0\200" ([(len :: (fcgi-length))] len))))


;;* Requests ----------------------------------------------------- *;;


(define <connection> {#:check {<open> (:in (opt input-port?))
                                      (:out (opt output-port?))
                                      (:requests (opt table?))}})


(define/table (<connection>::push request)
  ;; TODO check for duplicate request ids?
  (set (or? self:requests (begin (set self :requests {<requests>}) self:requests))
       request:id
       request)
  ;; notify connection:writer thread of the new request
  (thread-send self:writer request)
  request)


(define/table (<connection>::drop request)
  (rm self:requests request:id))


(define/table (<connection>::close)
  (unless (? self:multiplexed)
    (displayln "closing connection")
    (flush-output self:out)
    (close-output-port self:out)))


(define/table (<connection>::start-reader)
  (set self :reader
       (thread
        (thunk
         (let loop ((r {<record>}))
           (unless (port-closed? self:in)
             (r::parse self:in)
             (displayln (isa r))
             (r::deliver)
             (loop {<record>})))))))


(define/table (<connection>::start-writer)
  (set self :writer
       (thread
        (thunk
         (let loop ()
           (sync

            (handle-evt (thread-receive-evt)
                        (λ (_)
                          (displayln "writer: fresh request")
                          (define mail (thread-receive))
                          (cond
                            ;; new request => (loop)
                            ((and (table? mail) (isa? mail <request>)))
                            (else (raise-argument-error 'connection:writer
                                                        "<request>" mail)))
                          (unless (port-closed? self:out)
                            (loop))))

            (handle-evt self:requests
                        (λ (request port <record>)
                          ;; TODO I could move this to <outgoing-record>::pack and
                          ;; <outgoing-record>::deliver
                          (define buffer (make-bytes 65535))
                          (define count (read-bytes-avail! buffer port))
                          (define stream (if (eof-object? count) #""
                                             (subbytes buffer 0 count)))
                          (define record {<record> (:id request:id)
                                                   (:stream stream)})
                          (record::deliver)
                          ;; TODO these really belong in :after deliver method
                          ;; combinators, but we don't yet support those
                          (cond
                            ;; flag relevant stream as closed
                            ((and (empty-bytes? stream) (isa? record <stdout>))
                             (set request :stdout-closed #t))
                            ((and (empty-bytes? stream) (isa? record <stderr>))
                             (set request :stderr-closed #t)))
                          ;; when both streams are closed consider request
                          ;; handled: send end-request and free resources
                          (when? (and? request:stdout-closed request:stderr-closed)
                            (::gc request)
                            (::deliver {<end-request> (:id request:id)}))
                          (loop)))

            (handle-evt (port-closed-evt self:out)
                        (λ (port)
                          (displayln "connection:out closed"))))

           ;; TODO ATM we rudely shutdown everything, but when multiplexing
           ;; requests we should probably notify every request and allow them time
           ;; to clean up gracefully and shutdown on reasonable timeout.
           (displayln "connection:writer closed")
           (custodian-shutdown-all self:custodian))))))


(define current-connection (make-parameter undefined))


(struct port (source sink)
  #:property prop:input-port 0
  #:property prop:output-port 1)
(define (make-port (limit #f))
  (let-values (((source sink) (make-pipe)))
    (let ((source (if limit (make-limited-input-port source limit) source)))
      (port source sink))))


(define <request> {<table/evt> #:check {<open> (:id      (opt integer?))
                                               (:records (opt list?))
                                               (:params  (opt table?))
                                               (:stdin   (opt port?))
                                               (:stdout  (opt port?))
                                               (:stderr  (opt port?))
                                               (:data    (opt port?))
                                               (:app     (opt procedure?))
                                               (:thread  (opt thread?))}})


(define/table (<request>::evt)
  ;; values: request pipe record-metatable
  (choice-evt #;stdout
              (wrap-evt
               (if? self:stdout-closed never-evt self:stdout)
               (λ (_) (values self self:stdout <stdout>)))
              #;stderr
              (wrap-evt
               (if? self:stderr-closed never-evt self:stderr)
               (λ (_) (values self self:stderr <stderr>)))))


(define/table (<request>::<proc>)
  (set self :thread
       (thread
        (thunk
         (parameterize ((current-input-port self:stdin)
                        (current-output-port self:stdout)
                        (current-error-port self:stderr))
           (define end-request {<end-request> (:id self:id)})
           (self::app)
           (close-output-port self:stdout)
           ;; TODO hm, this would result in us sending a closing <stderr>, which
           ;; may not be what we wanted. But maybe it'll be ignored?
           (close-output-port self:stderr))))))


(define/table (<request>::push record)
  (set self :records (cons record (or? self:records '()))))


(define/table (<request>::gc)
  (define request self)
  (define thread (? request:thread))
  (define connection (current-connection))
  (when thread (kill-thread thread))
  (connection::drop self)
  (printf "<request> ~a dropped\n" request:id))


;; {(request-id <request>)}
(define <requests> {<table/evt> (:evt (λ (t) (apply choice-evt (dict-values t))))})


;;* FCGI records ------------------------------------------------- *;;


(define (mt-of type)
  (cond
    ((eq? type FCGI_BEGIN_REQUEST) <begin-request>)
    ((eq? type FCGI_ABORT_REQUEST) <abort-request>)
    ((eq? type FCGI_PARAMS)        <params>)
    ((eq? type FCGI_STDIN)         <stdin>)
    ((eq? type FCGI_STDOUT)        <stdout>)
    ((eq? type FCGI_STDERR)        <stderr>)
    ((eq? type FCGI_END_REQUEST)   <end-request>)
    (else <unknown-type>)))


;;* <record> ----------------------------------------------------- *;;


(define <record> {#:check {<open> (:version (opt integer?))
                                  (:type    (opt integer?))
                                  (:id      (opt integer?))
                                  (:clen    (opt integer?))
                                  (:plen    (opt integer?))}
                  (:version FCGI_VERSION_1)
                  (:clen 0)
                  (:plen 0)})


;; NOTE <record>::parse is always the entry point for parsing. It is here that the
;; actual record type is determined and parsing continues in descendant's method:
;; 1. parse fcgi-header to get record type,
;; 2. swap record's (self) metatable to the one of that type,
;; 3. continue parsing by calling :parse on itself (delegate to new metatable).


(define/table (<record>::parse in)
  (bit-string-case (read-bytes 8 in)
    ([(r :: (fcgi-header self))]
     (set-table-meta! r (mt-of r:type))
     (r::parse in))
    (else (error "Failed to parse fcgi-header"))))


(define/table (<record>::pack)
  ;; there isn't really a reasonable default here, so lets just delegate to
  ;; <stream>. In most cases this well simply pack record with empty payload.
  (<stream>:pack self))


(define/table (<record>::request)
  (define connection (current-connection))
  (define requests (get connection :requests))
  (get requests self:id))


;; TODO any reasonable <record>::pack?


(define/table (<record>::deliver)
  (define request (self::request))
  (request::push self))


;; TODO don't forget to case-lambda its <setmeta> if I ever switch to optional 2nd
;; arg in <setmeta> metamethods just for traits
;;
;; trait
(define <outgoing> {(:<setmeta> (λ (t) (set t :deliver <outgoing>:deliver)))})


(define/table (<outgoing>::deliver)
  (define connection (current-connection))
  (define message (self::pack))
  (define count (write-bytes message connection:out))
  (flush-output connection:out)
  ;; TODO log correct record type here <stdout>, <stderr>, <data>, <end-request>
  (printf "<outgoing>::~a ~a bytes delivered\n" self:type count))


;;* <stream> ----------------------------------------------------- *;;


(define <stream> {<record> #:check {<open> (:stream (opt bytes?))}})


(define/table (<stream>::parse in)
  (define clen self:clen)
  (define plen self:plen)
  (bit-string-case (read-bytes (+ clen plen) in)
    ([(stream :: binary bytes clen) (_ :: bytes plen)]
     (set self :stream (bit-string->bytes stream)))
    (else
     (error "Failed to parse" (isa self)))))


(define/table (<stream>::assemble)
  (define request (self::request))
  (bytes-append*
   (for/list ((r (in-list request:records))
              #:when (eq? r:type self:type))
     r:stream)))


(define/table (<stream>::pack)
  (define stream (bytes-of (or (? self:stream) #"")))
  (define-values (plen padding body) (pad stream))
  (set self :clen (bytes-length stream))
  (set self :plen plen)
  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header))
    (body :: binary))))


;; delegate <stream>::deliver => <record>::deliver


;;* <management> ------------------------------------------------- *;;


(define <management> {<record> #:check {<open> (:name-values (opt table?))}
                               (:id FCGI_NULL_REQUEST_ID)})


(define/table (<management>::parse in)
  (:parse <stream> self in)
  (set self :name-values (parse-name-values self:stream)))


(define/table (<management>::pack)
  (set self :stream (bytes-of (or (? self:name-values) (? self:stream) #"")))
  (:pack <stream> self))


;;** - <begin-request> ------------------------------------------- *;;


(define <begin-request> {<record> #:check {<open> (:role (opt integer?))
                                                  (:flags (opt integer?))}
                                  (:type FCGI_BEGIN_REQUEST)
                                  (:clen 8)})


(define/table (<begin-request>::parse in)
  (define r self)
  (define body (read-bytes (+ r:clen r:plen) in))
  (bit-string-case body
    ([(role :: bytes 2) flags (_ :: bytes 5)]
     (set r :role role)
     (set r :flags flags))
    (else
     (error "Failed to parse fcgi-begin-request"))))


;; for completeness
(define/table (<begin-request>::pack)
  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header))
    (self:role :: bytes 2)
    self:flags
    (0 :: bytes 5))))


(define/table (<begin-request>::deliver)
  (define connection (current-connection))
  (define request {<request> (:id self:id) (:app app)})
  (request::push self)
  (connection::push request))


(module+ test
  (test-case "<begin-request>"
    (define/checked begin-request {<begin-request> (:id 1)
                                                   (:role FCGI_RESPONDER)
                                                   (:flags FCGI_KEEP_CONN)})

    (define/checked packed (begin-request::pack))
    (check-true (zero? (remainder (bytes-length packed) 8)))
    (define r {<record>})
    (define/checked parsed (r::parse (open-input-bytes packed)))
    (check-true (isa? parsed <begin-request>))
    (check-eq? parsed:id begin-request:id)
    (check-eq? parsed:role begin-request:role)
    (check-eq? parsed:flags begin-request:flags)))


;;** - <abort-request> ------------------------------------------- *;;


(define <abort-request> {<record> (:type FCGI_ABORT_REQUEST)})


(define/table (<abort-request>::parse in)
  ;; meant to be empty, we read to throw away to be safe
  (read-bytes (+ self:clen self:plen) in))


;; delegate <abort-request>::pack  => <record>::pack


(define/table (<abort-request>::deliver)
  (define request (self::request))
  ;; TODO if the (? request:thread) is running we ought to notify it and allow
  ;; some time for the app to clean up. We timestamp in case we decide to go with
  ;; manual periodic GC of aborted requests.
  ;;
  ;;   (set request :aborted (current-inexact-milliseconds))
  ;;   (thread-send thread :abort-request)
  (::gc request)
  (::deliver {<end-request> (:id request:id)}))


;; TODO to test this I'd probably want some long-running app, so that I can ABORT
;; in the browser. I hope Nginx sends an abort-request when that happens.


;;** - <params> -------------------------------------------------- *;;


(define <params> {<stream> (:type FCGI_PARAMS)})


(define (content-length params)
  (or (string->number (get params "CONTENT_LENGTH")) 0))


;; delegate <params>::parse => <stream>::parse
;; delegate <params>::pack  => <stream>::pack
(define/table (<params>::deliver)
  (define request (self::request))
  (cond
    ((zero? self:clen)
     (set request :params (parse-name-values (self::assemble)))
     (set request :stdin (make-port (content-length request:params)))
     (set request :stdout (make-port))
     (set request :stderr (make-port))
     ;; run the app
     (request))
    (else (request::push self))))


;;** - <stdin> --------------------------------------------------- *;;


(define <stdin> {<stream> (:type FCGI_STDIN)})


;; delegate <params>::parse => <stream>::parse
;; delegate <params>::pack => <stream>::pack


;; solution that waits for all chunks to arrive, then assembles them
(define/table (<stdin>::deliver)
  (define request (self::request))
  (cond
    ((zero? self:clen)
     (write-bytes (self::assemble) request:stdin))
    (else (request::push self))))


;; very general solution that potentially doesn't wait for <stdin> to finish
#;(define/table (<stdin>::deliver)
    (define request (self::request))
    (request::push self)
    (if (zero? self:clen)
        (close-input-port request:stdin)
        (write-bytes self:stream request:stdin)))


;;** - <stdout> -------------------------------------------------- *;;


(define <stdout> {<stream> #:direction <outgoing>
                           (:type FCGI_STDOUT)})
;; delegate <stdout>::parse   => <stream>::parse
;; delegate <stdout>::pack    => <stream>::pack
;; delegate <stdout>::deliver => <outgoing>::deliver


;;** - <stderr> -------------------------------------------------- *;;


(define <stderr> {<stream> #:direction <outgoing>
                           (:type FCGI_STDERR)})
;; delegate <stderr>::parse   => <stream>::parse
;; delegate <stderr>::pack    => <stream>::pack
;; delegate <stderr>::deliver => <outgoing>::deliver


;;** - data ------------------------------------------------------ *;;


;;** - end-request ----------------------------------------------- *;;


(define <end-request> {<record> #:direction <outgoing>
                                (:type FCGI_END_REQUEST)
                                (:clen 8)
                                (:app-status 0)
                                (:proto-status FCGI_REQUEST_COMPLETE)})


;; for completeness
(define/table (<end-request>::parse in)
  (bit-string-case (read-bytes (+ self:clen self:plen) in)
    ([(app-status :: integer bytes 4) proto-status (_ :: bytes 3)]
     (set self :app-status app-status)
     (set self :proto-status proto-status))
    (else
     (error "Failed to parse" (isa self)))))


(define/table (<end-request>::pack)
  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header))
    (self:app-status :: bytes 4)
    self:proto-status
    (0 :: bytes 3))))


(define/table (<end-request>::deliver)
  ;; outgoing will deliver
  (:deliver <outgoing> self)
  (::close (current-connection)))


(module+ test
  (test-case "<end-request>"
    (define/checked end-request {<end-request> (:id 1)})
    (define/checked packed (end-request::pack))
    (check-true (zero? (remainder (bytes-length packed) 8)))
    (define r {<record>})
    (define/checked parsed (r::parse (open-input-bytes packed)))
    (check-true (isa? parsed <end-request>))
    (check-eq? parsed:id end-request:id)
    (check-eq? parsed:app-status end-request:app-status)
    (check-eq? parsed:proto-status end-request:proto-status)))


;;** - <get-values> ---------------------------------------------- *;;


(define <get-values> {<management> (:type FCGI_GET_VALUES)})

;; delegate <get-values>::parse => <management>::parse
;; delegate <get-values>::pack  => <management>::pack
(define/table (<get-values>::deliver)
  (::deliver
   {<get-values-result>
    (:name-values
     {("FCGI_MAX_CONNS"  (and? (get self:name-values "FCGI_MAX_CONNS") FCGI_MAX_CONNS))
      ("FCGI_MAX_REQS"   (and? (get self:name-values "FCGI_MAX_REQS") FCGI_MAX_REQS))
      ("FCGI_MPXS_CONNS" (and? (get self:name-values "FCGI_MPXS_CONNS") FCGI_MPXS_CONNS))})}))


;;** - <get-values-result> --------------------------------------- *;;


(define <get-values-result> {<management> #:direction <outgoing>
                                          (:type FCGI_GET_VALUES_RESULT)})


;; delegate <get-values-result>::parse   => <management>::parse
;; delegate <get-values-result>::pack    => <management>::pack
;; delegate <get-values-result>::deliver => <outgoing>::deliver


;;** - <unknown-type> -------------------------------------------- *;;


(define <unknown-type> {<management> #:direction <outgoing>
                                     #:check {<open> (:unknown (opt integer?))}
                                     (:type FCGI_UNKNOWN_TYPE)})


(define/table (<unknown-type>::parse in)
  ;; drop the payload
  (read-bytes (+ self:clen self:plen) in)
  ;; cast this unknown record to <unknown-type>
  (set self :unknown self:type)
  (set self :type FCGI_UNKNOWN_TYPE)
  (set self :id FCGI_NULL_REQUEST_ID)
  (set self :clen 8)
  (set self :plen 0))


(define/table (<unknown-type>::pack)
  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header)) self:unknown (0 :: bytes 7))))


;; delegate <unknown>::deliver => <outgoing>::deliver


;;* app ---------------------------------------------------------- *;;


(define (app request)
  (display "Content-type: text/html\r\n\r\n")
  ;; TODO HTML table with params
  (display "<html><body>Hello world!</body></html>"))


;;* Main --------------------------------------------------------- *;;


(define (main)
  (define tcp-port 9000)
  (define tcp-max-allow-wait 1024)
  (define tcp-reuse? false)
  (define tcp-hostname "127.0.0.1")
  (define tcp-listener (tcp-listen tcp-port tcp-max-allow-wait tcp-reuse? tcp-hostname))

  (let loop ()
    (define connection-custodian (make-custodian))
    (define connection {<connection> (:requests {<requests>})
                                     (:custodian connection-custodian)})
    (parameterize ((current-custodian connection-custodian)
                   (current-connection connection))

      (let-values (((in out) (tcp-accept tcp-listener)))
        (displayln "client connected")
        (set connection :in in)
        (set connection :out out)
        (connection::start-writer)
        (connection::start-reader)))
    (loop)))


;;* Notes ----------------------------------------------------------- *;;


;; Standard and existing web server implementations suggest 3 possibly routes:
;;
;; (1) full request multiplexing: multiple parallel requests on a single channel.
;;
;; (2) serial multiplexing: multiple requests on connection but only serially.
;;
;; (3) connection multiplexing: each request gets its own connection.
;;
;; I think (1) is what standard allows and would be the most performant model, but
;; not easy to get right since I'd have to synchronize multiple workers that write
;; to the same stdout, synchronize at FastCGI record boundaries that is.
;;
;; Now, (2) would be unfortunate for performance but very easy to code.
;;
;; While (3) is a good middle ground - one I hope for. IIUC, implementing (3)
;; would automatically support (2) assuming that keep-conn flag is set and the
;; thread that handles a request doesn't shutdown after sending fcgi-end-request
;; but instead loops to receive the next begin request. This essentially makes the
;; whole process multi-threaded when there're multiple connections, and
;; single-threaded and worse serial, when there's but one connection.

;; Main loop listens for connections. Every new connection spawns a communicator
;; thread.
;;
;; Communicatorthread will spawn a new worker for every fcgi-begin-request.
;; Communicator thread delivers fcgi records to appropriate workers, packs
;; outgoing messages from them and puts them on the wire.
;;
;; Communicator thread keeps connection open unless keep-conn is unset. Then it
;; closes the connection after all workers are finished.
;;
;; Worker dies after sending its fcgi-end-request message.
;;
;; Communicator is essentially two threads: 1 - parses incomming fcgi and delivers
;; to appropriate worker or spawns a new one, 2 - choice-block-reads from all
;; workers' stdout and stderr ports, fcgi packs available output, puts it on the
;; wire, loops.
;;
;; Since for every connection only its communcator-2 thread is allowed to write to
;; that connection, I think this architecture covers all three multiplexing models
;; (1-3).

;; Some vocabulary: we call communicator thread a "connection", we call
;; communicator-1 a "reader", we call communicator-2 a "writer".

;; Every fcgi-end-request message carries both app-status and proto-status. Every
;; connection thread could simply alter (inc/dec) some global proto-status state.
;; IMO we can allow that race (number of connections maybe tricky: someone needs
;; to decrement when connection is dropped). How do we report app-status (worker
;; status effectively). How about worker alters its status then sends itself to
;; its connection's mailbox. Connection checks its mailbox, finds worker there and
;; that signals fcgi-end-request which it sends on worker's behalf.
