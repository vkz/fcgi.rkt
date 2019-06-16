#lang prelude

(require prelude/tables)

(require bitsyntax
         racket/generic
         racket/struct
         racket/hash)


(module+ test
  (require rackunit
           prelude/testing))


;;* Constants ------------------------------------------------------- *;;


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
(define FCGI_MAX_CONNS  "FCGI_MAX_CONNS")
(define FCGI_MAX_REQS   "FCGI_MAX_REQS")
(define FCGI_MPXS_CONNS "FCGI_MPXS_CONNS")


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
  (check equal? #"\3\3bazduh\3\3foobar"  (pack-name-values (ht ("foo" "bar")
                                                               ("baz" "duh")))))


(define (parse-name-values stream #:into [table (ht)])
  (if (empty-bytes? stream)
      table
      (bit-string-case stream
        ([(nlen  :: (fcgi-length))
          (vlen  :: (fcgi-length))
          (name  :: binary bytes nlen)
          (value :: binary bytes vlen)
          (rest  :: binary)]
         (set: table
               (bytes->string/utf-8 (bit-string->bytes name))
               (bytes->string/utf-8 (bit-string->bytes value)))
         (parse-name-values rest #:into table)))))


(module+ test
  (check equal? (ht ("name" "value")) (parse-name-values (pack-name-value "name" "value")))
  (check equal? (ht ("name" ""))      (parse-name-values (pack-name-value "name" "")))
  (check equal? (ht ("name" "255"))   (parse-name-values (pack-name-value "name" "255")))
  (check equal? (ht ("name" "256.5")) (parse-name-values (pack-name-value "name" 256.5)))
  (check equal? (ht ("foo" "bar") ("baz" "duh")) (parse-name-values
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
        r.version r.type (r.id :: bytes 2) (r.clen :: bytes 2) r.plen #;reserved 0)))))


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


;;* FCGI records ------------------------------------------------- *;;


(define (mt-of type)
  (cond
    ((eq? type FCGI_BEGIN_REQUEST) <begin-request>)
    (else <mock>)
    ;; (else (raise-argument-error 'mt-of "known record type" type))
    ))


(define (type-of record-num)
  (cond
    ((eq? record-num FCGI_BEGIN_REQUEST)     :BEGIN_REQUEST)
    ((eq? record-num FCGI_ABORT_REQUEST)     :ABORT_REQUEST)
    ((eq? record-num FCGI_END_REQUEST)       :END_REQUEST)
    ((eq? record-num FCGI_PARAMS)            :PARAMS)
    ((eq? record-num FCGI_STDIN)             :STDIN)
    ((eq? record-num FCGI_STDOUT)            :STDOUT)
    ((eq? record-num FCGI_STDERR)            :STDERR)
    ((eq? record-num FCGI_DATA)              :DATA)
    ((eq? record-num FCGI_GET_VALUES)        :GET_VALUES)
    ((eq? record-num FCGI_GET_VALUES_RESULT) :GET_VALUES_RESULT)
    ((eq? record-num FCGI_UNKNOWN_TYPE)      :UNKNOWN_TYPE)
    ((eq? record-num FCGI_MAXTYPE)           :MAXTYPE)))



(define <record> {#:check {<open> (:version (? integer?))
                                  (:type    (? integer?))
                                  (:id      (? integer?))
                                  (:clen    (? integer?))
                                  (:plen    (? integer?))}})


(define/table (<record>:parse in)
  (bit-string-case (read-bytes 8 in)
    ([(r :: (fcgi-header self))]
     (set-table-meta! r (mt-of r.type))
     (r:parse in))
    (else (error "Failed to parse fcgi-header"))))


;;** - <begin-request> ------------------------------------------- *;;


(define <begin-request> {<record> #:check {<open> (:role (? integer?))
                                                  (:flags (? integer?))}})


(define/table (<begin-request>:parse in)
  (define r self)
  (define body (read-bytes (+ r.clen r.plen) in))
  (bit-string-case body
    ([(role :: bytes 2) flags (_ :: bytes 5)]
     (set r :role role)
     (set r :flags flags))
    (else
     (error "Failed to parse fcgi-begin-request"))))


(define/table (<begin-request>:pack)
  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header))
    (self.role :: bytes 2)
    self.flags
    (0 :: bytes 5))))


(module+ test
  (test-case "<begin-request>"
    (define/checked begin-request {<begin-request> (:version 1)
                                                   (:type FCGI_BEGIN_REQUEST)
                                                   (:id 1)
                                                   (:clen 8)
                                                   (:plen 0)
                                                   (:role FCGI_RESPONDER)
                                                   (:flags FCGI_KEEP_CONN)})

    (define/checked packed (begin-request:pack))
    (check-true (zero? (remainder (bytes-length packed) 8)))
    (define r {<record>})
    (define/checked parsed (r:parse (open-input-bytes packed)))
    (check-true (isa? parsed <begin-request>))
    (check-equal? begin-request parsed)))


(define <mock> {<record> (:parse (λ (r in)
                                   (read-bytes (+ r.clen r.plen) in)
                                   (set r :name (type-of r.type))
                                   r))})


;;** - abort-request --------------------------------------------- *;;
;;** - end-request ----------------------------------------------- *;;
;;** - get-values ------------------------------------------------ *;;
;;** - get-values-result ----------------------------------------- *;;
;;** - unknown --------------------------------------------------- *;;
;;** - stream ---------------------------------------------------- *;;
;;** - params ---------------------------------------------------- *;;
;;** - stdin ----------------------------------------------------- *;;
;;** - stdout ---------------------------------------------------- *;;
;;** - stderr ---------------------------------------------------- *;;
;;** - data ------------------------------------------------------ *;;

;;* Connection ------------------------------------------------------ *;;


;; Standard ports for request struct
(struct stdin  (source sink) #:property prop:input-port 0 #:property prop:output-port 1)
(struct stdout (source sink) #:property prop:input-port 0 #:property prop:output-port 1)
(struct stderr (source sink) #:property prop:input-port 0 #:property prop:output-port 1)
(struct data   (source sink) #:property prop:input-port 0 #:property prop:output-port 1)


(define (main)
  (define tcp-port 9000)
  (define tcp-max-allow-wait 1024)
  (define tcp-reuse? false)
  (define tcp-hostname "127.0.0.1")
  (define connection (tcp-listen tcp-port tcp-max-allow-wait tcp-reuse? tcp-hostname))

  ;; (define connections empty)

  (let-values (((in out) (tcp-accept connection)))
    (displayln "connection established")
    (let loop ((r {<record>}))
      (displayln (r:parse in))
      (loop {<record>}))))


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
