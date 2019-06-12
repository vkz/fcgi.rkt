#lang prelude/tables


(require bitsyntax)


(module+ test
  (require rackunit
           prelude/testing))


;;* Structs --------------------------------------------------------- *;;


;; TODO for now we assume that there is a way to do tables "Racket first class"
;; that is we can e.g. implement prop:evt on tables


(table table/evt lua ()
       #:property prop:evt (λ (self)
                             (choice-evt
                              (request-out self)
                              (request-err self))))


(define <request> {#:type table/evt
                   #:spec ((:id integer?)
                           (:records list?)
                           (:params table?)
                           (:in stdin?)
                           (:out stdout?)
                           (:err stderr?)
                           (:data data?)
                           (:thread thread?))})


(define/table (<request>:push record)
  (~> self.records
      (cons record ~)
      (set self.records ~)))


;; Standard ports for request
(struct stdin  (source sink) #:property prop:input-port 0 #:property prop:output-port 1)
(struct stdout (source sink) #:property prop:input-port 0 #:property prop:output-port 1)
(struct stderr (source sink) #:property prop:input-port 0 #:property prop:output-port 1)
(struct data   (source sink) #:property prop:input-port 0 #:property prop:output-port 1)


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


;; TODO replace ht with {}
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
        (let ((<fcgi> (. fcgi-types type))
              (content {(:version version)
                        (:type type)
                        (:id id)
                        (:clen clen)
                        (:plen plen)}))
          (~> {<fcgi> (:id id)
                      (:content content)}
              (ks ~ rest))))
       (else (kf))))

    ;; parse into record passed as argument
    ((_ #t input ks kf r)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([version type (id :: bytes 2) (clen :: bytes 2) plen _ (rest :: binary)]
        (let ((content {(:version version)
                        (:type type)
                        (:id id)
                        (:clen clen)
                        (:plen plen)}))
          (~> r
              (set ~ :id id)
              ;; TODO maybe assert that r.type = type?
              (set ~ :type type)
              (set ~ :content (if (table? r.content)
                                  (:merge r.content content)
                                  content))
              (ks ~record rest))))
       (else (kf))))

    ;; unparse header from record
    ((_ #f r)
     (match-let (((table version type id clen plen) r.content))
       (bit-string (or version 1)
                   (or type r.type 0)
                   ((or id r.id 0) :: bytes 2)
                   ((or clen 0) :: bytes 2)
                   (or plen 0)
                   #;reserved 0)))))


(module+ test
  (define header (make-record
                  FCGI_BEGIN_REQUEST
                  #:id 1
                  #:content (ht ('version 1)
                                ('type FCGI_BEGIN_REQUEST)
                                ('id 1)
                                ('clen 3)
                                ('plen 5))))

  ;; unparse header
  (define bs (bit-string (header :: (fcgi-header))))
  (check eq? 8 (bit-string-byte-count bs))
  (check equal? '(1 1 0 1 0 3 5 0) (bytes->list (bit-string->bytes bs)))

  ;; (parse (unparse header)) == header
  (check equal?
         (record-content header)
         (record-content
          (bit-string-case (bit-string->bytes bs)
            ([(header :: (fcgi-header))] header)))))


(define-syntax fcgi-length
  (syntax-rules ()

    ;; parse and create a record
    ((_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([(= 0 :: bits 1) (len :: integer bits 7) (rest :: binary)]
        (ks len rest))

       ([(= 1 :: bits 1) (len :: integer bits 31) (rest :: binary)]
        (ks len rest))
       (else (kf))))

    ;; unparse header from record
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


;;* FCGI ------------------------------------------------------------ *;;


(define <fcgi> {})

(define-generics fcgi
  (parse   fcgi #;from input-port)
  (deliver fcgi #;via connection)
  (pack    fcgi content)
  (respond fcgi #;to request #;over output-port))


(define fcgi-types {(:base                  <fcgi>)
                    (FCGI_GET_VALUES        <fcgi-get-values>)
                    (FCGI_GET_VALUES_RESULT <fcgi-get-values-result>)
                    (FCGI_UNKNOWN_TYPE      <fcgi-unknown>)
                    (FCGI_BEGIN_REQUEST     <fcgi-begin-request>)
                    (FCGI_ABORT_REQUEST     <fcgi-abort-request>)
                    (FCGI_END_REQUEST       <fcgi-end-request>)
                    (FCGI_PARAMS            <fcgi-params>)
                    (FCGI_STDIN             <fcgi-stdin>)
                    (FCGI_STDOUT            <fcgi-stdout>)
                    (FCGI_STDERR            <fcgi-stderr>)
                    (FCGI_DATA              <fcgi-data>)})


(comment
 ;; NOTE There's also FCGI_MAXTYPE but spec only mentions it once without details
 (define (make-record [type 'base]
                      #:id           [id           undefined]
                      #:content      [content      (ht)]
                      #:complete?    [complete?    false]
                      #:type         [unknown-type undefined]
                      #:role         [role         undefined]
                      #:flags        [flags        undefined]
                      #:app-status   [app-status   undefined]
                      #:proto-status [proto-status undefined]
                      #:payload      [payload      undefined])
   (cond
     ((eq? type 'base)                  (record                 FCGI_NULL_REQUEST_ID type content payload))
     ((eq? type FCGI_GET_VALUES)        (fcgi-get-values        FCGI_NULL_REQUEST_ID type content payload))
     ((eq? type FCGI_GET_VALUES_RESULT) (fcgi-get-values-result FCGI_NULL_REQUEST_ID type content payload))
     ((eq? type FCGI_UNKNOWN_TYPE)      (fcgi-unknown           FCGI_NULL_REQUEST_ID type content payload unknown-type))
     ((eq? type FCGI_BEGIN_REQUEST)     (fcgi-begin-request     id type content payload role flags))
     ((eq? type FCGI_ABORT_REQUEST)     (fcgi-abort-request     id type content payload))
     ((eq? type FCGI_END_REQUEST)       (fcgi-end-request       id type content payload app-status proto-status))
     ((eq? type FCGI_PARAMS)            (fcgi-params            id type content payload complete?))
     ((eq? type FCGI_STDIN)             (fcgi-stdin             id type content payload complete?))
     ((eq? type FCGI_STDOUT)            (fcgi-stdout            id type content payload complete?))
     ((eq? type FCGI_STDERR)            (fcgi-stderr            id type content payload complete?))
     ((eq? type FCGI_DATA)              (fcgi-data              id type content payload complete?))))
 ;; comment
 )


;;** - records ------------------------------------------------------ *;;


;; TODO it would be nice to let the user specify slots either for documentation
;; purpose only or as guards or contracts? Maybe have a special define-meta that
;; lets you do that? Otherwise we are forced to do this in comments. This is kind
;; of what SPEC does in Clojure I guess.

;; id type content payload
(define <record> {#:spec
                  ((:id integer?)
                   (:type (and/c integer? (curry set-member? (keys fcgi-types))))
                   (:content table?)
                   (:payload (or/c table? bytes?)))})


(define <stream-record>     {<record> #:spec
                                      (:id (and/c integer? (not FCGI_NULL_REQUEST_ID)))})

(define/table (<record>:parse in)
  (bit-string-case (read-bytes 8 in)
    ([(record :: (fcgi-header))] (record:parse in))
    (else (error "Failed to parse fcgi-header"))))

(define/table (<record>:pack content)
  (set! content (bytes-of (or content self.payload)))
  (define clen (bytes-length content))
  (define-values (plen padding body) (pad content))

  (unless (table? self.content)
    (set self :content {}))

  (hash-union! self.content {(:version 1)
                             (:type self.type)
                             (:id self.id)
                             (:clen clen)
                             (:plen plen)})
  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header))
    (body :: binary))))


;;*** == fcgi-begin-request ----------------------------------------- *;;


(define <begin-request> {<record> #:spec ((:role (or/c FCGI_RESPONDER
                                                       FCGI_AUTHORIZER
                                                       FCGI_FILTER))
                                          (:flags FCGI_KEEP_CONN))})


(define/table (<begin-request>:parse in)
  (define body (read-bytes (+ self.clen self.plen) in))
  (bit-string-case body
    ([(role :: bytes 2) flags (_ :: bytes 5)]
     ;; NOTE {} are mutable ~> is redundant, could splice these in body. As it is
     ;; now ~> is nothing more than a (begin ...) form.
     (~> (if (table? self.content) self.content {})
         (set ~content :body body)
         (set ~content :role role)
         (set ~content :flags flags)
         (set self :content ~)
         (set ~self :role role)
         (set ~self :flags flags)))
    (else
     (error "Failed to parse fcgi-begin-request"))))


(define/table (<begin-request>:pack content)
  (set! content (or content self.payload))
  (define role (or self.role content.role))
  (define flags (or self.flags content.flags))
  (define clen 8)
  ;; delegate to <record>
  (<record>:pack self (bit-string->bytes
                       (bit-string (role :: bytes 2) flags (0 :: bytes 5)))))

;; NOTE how we "document" self by using <mt>.meth instead of <mt>:meth
(define/table (<begin-request>.deliver record connection)
  ;; TODO check for duplicate id in requests
  (~> {<request>
       (:id record.id)
       (:records record)
       (:params {})
       (:stdin  (let-values (((source sink) (make-pipe 65535))) (stdin source sink)))
       (:stdout (let-values (((source sink) (make-pipe 65535))) (stdout source sink)))
       (:stderr (let-values (((source sink) (make-pipe 65535))) (stderr source sink)))
       (:data   (let-values (((source sink) (make-pipe 65535))) (data source sink)))}
      (set connection.requests record.id ~request)))


(module+ test

  (test-case "begin-request-message"

    (define begin-request-message (pack (make-record FCGI_BEGIN_REQUEST #:id 1)
                                        (ht ('role FCGI_RESPONDER)
                                            ('flags FCGI_KEEP_CONN))))

    (check-pred zero? (remainder (bytes-length begin-request-message) 8))

    (check equal? begin-request-message (pack (make-record FCGI_BEGIN_REQUEST
                                                           #:id 1
                                                           #:role FCGI_RESPONDER
                                                           #:flags FCGI_KEEP_CONN)
                                              undefined))

    (define begin-request-parsed (parse
                                  (make-record)
                                  (open-input-bytes begin-request-message)))

    (check eq? FCGI_RESPONDER (fcgi-begin-request-role begin-request-parsed))
    (check eq? FCGI_KEEP_CONN (fcgi-begin-request-flags begin-request-parsed))))


;;*** == fcgi-abort-request ----------------------------------------- *;;


(define <abort-request> {<record>})


(define/table (<abort-request>:parse in)
  ;; simply throw away payload if any
  (read-bytes (+ self.clen self.plen) in)
  self)


(define/table (<abort-request>:deliver connection)
  (define request (connection :requests self.id))
  (if (and (<request>? request) request.thread)
      ;; TODO request already running, notify it (but why?)
      (thread-send request.thread self)
      ;; request not running, let connection-writer deal with it
      (thread-send connection.writer {<end-request> (:id self.id)
                                                    (:appstatus 1)
                                                    (:protostatus FCGI_REQUEST_COMPLETE)})))


;;*** == fcgi-end-request ------------------------------------------- *;;


(define <end-request> {<record> #:spec
                                ((:appstatus integer?)
                                 (:protostatus integer?))})


(define/table (<end-request>:parse in)
  (define body (read-bytes 8 in))
  (bit-string-case body
    ([(appstatus :: integer bytes 4) protostatus (_ :: bytes 3)]
     (set self :appstatus appstatus)
     (set self :protostatus protostatus)
     (set self :body body))
    (else
     (error "Failed to parse" (object-name record)))))


(define/table (<end-request>:pack content)
  (define appstatus   (if (table? content) content.appstatus self.appstatus))
  (define protostatus (if (table? content) content.protostatus self.protostatus))

  (unless (table? self.content)
    (set self :content {}))

  (hash-union! (or self.content {}) {(:clen 8) ('plen 0)})

  (bit-string->bytes
   (bit-string
    (self :: (fcgi-header))
    (appstatus :: bytes 4)
    protostatus
    (0 :: bytes 3))))


(define/table (<end-request>:deliver connection)
  (write-bytes (self:pack undefined) connection.out)
  self)


(module+ test

  (test-case "end-request-message"

    (define end-request-message (pack (make-record FCGI_END_REQUEST
                                                   #:id 1
                                                   #:app-status 1
                                                   #:proto-status FCGI_OVERLOADED)
                                      undefined))

    (check-pred zero? (remainder (bytes-length end-request-message) 8))

    (check equal? end-request-message (pack (make-record FCGI_END_REQUEST #:id 1)
                                            (ht ('app-status 1)
                                                ('proto-status FCGI_OVERLOADED))))


    (define end-request-parsed (parse (make-record)
                                      (open-input-bytes end-request-message)))

    (check eq? 1 (fcgi-end-request-app-status end-request-parsed))
    (check eq? FCGI_OVERLOADED (fcgi-end-request-proto-status end-request-parsed))))


;;** - management-records ------------------------------------------- *;;


(define <management-record> {<record> (:id FCGI_NULL_REQUEST_ID)})


(define/table (<management-record>:parse in)
  ;; header will have been parsed by record parse that delegates here
  (bit-string-case (read-bytes (+ self.clen self.plen) in)
    ([(params :: binary bytes self.clen) (_ :: bytes self.plen)]
     (set self :body (bit-string->bytes params))
     (set self :payload (parse-name-values self.body)))
    (else
     (error "Failed to parse" (object-name record)))))



;;*** == fcgi-get-values ------------------------------------------- *;;


(define <get-values> {<management-record>})


(define/table (<get-values>:deliver connection)
  (thread-send connection.writer self)
  self)


(module+ test

  (define/checked get-values-message (pack (make-record FCGI_GET_VALUES #:id FCGI_NULL_REQUEST_ID)
                                           (ht (FCGI_MAX_CONNS  "")
                                               (FCGI_MAX_REQS   "")
                                               (FCGI_MPXS_CONNS ""))))

  (check-pred zero? (remainder (bytes-length get-values-message) 8))

  (define get-values-parsed (parse (make-record)
                                   (open-input-bytes get-values-message)))

  (check equal?
         (ht (FCGI_MAX_CONNS  "")
             (FCGI_MAX_REQS   "")
             (FCGI_MPXS_CONNS ""))
         (record-payload get-values-parsed)))


;;*** == fcgi-get-values-result ------------------------------------ *;;


(define <get-values-result> {<get-values-result>})


(define/table (<get-values-result>:deliver connection)
  (write-bytes (self:pack self.payload) connection.out)
  self)


(module+ test

  (define get-values-result-message (pack
                                     (make-record FCGI_GET_VALUES_RESULT
                                                  #:id FCGI_NULL_REQUEST_ID)
                                     (ht (FCGI_MAX_CONNS  "16")
                                         (FCGI_MAX_REQS   "32")
                                         (FCGI_MPXS_CONNS "0"))))

  (check-pred zero? (remainder (bytes-length get-values-result-message) 8))

  (define get-values-result-parsed (parse (make-record)
                                          (open-input-bytes get-values-result-message)))

  (check equal?
         (ht (FCGI_MAX_CONNS  "16")
             (FCGI_MAX_REQS   "32")
             (FCGI_MPXS_CONNS "0"))
         (record-payload get-values-result-parsed)))


;;*** == fcgi-unknown ---------------------------------------------- *;;


(define <unknown> {<management-record> #:spec
                                       ((:unknown-type integer?))})


(define/table (<unknown>:parse in)
  (bit-string-case (read-bytes (+ self.clen self.plen) in)
    ([type (_ :: bytes 7) (_ :: bytes self.plen)]
     (set self :body type)
     (set self :unknown-type type))
    (else
     (error "Failed to parse" (object-name record)))))


(define/table (<unknown>:deliver connection)
  (write-bytes (self:pack self:unknown-type) connection.out)
  self)


;;** - stream-records ----------------------------------------------- *;;


(define <stream-record> {<record> #:spec (:complete? boolean?)})


(define/table (<stream-record>:parse in)
  (let ((clen (self :content :clen))
        (plen (self :content :plen)))
    (bit-string-case (read-bytes (+ clen plen) in)
      ([(body :: binary bytes clen) (_ :: bytes plen)]
       (setf (self :content :body) body)
       (set self :complete? (or self.complete? (zero? clen))))
      (else
       (error "Failed to parse" (object-name record))))
    record))


;; TODO wonder if I could extend some kind of stream interface to stream-record,
;; one that does this kind of assembly as needed? Would it be worth the effort?
(define/table (<stream-record>:assemble #;from connection)
  (define request (connection :requests self.id))
  (for/fold ((stream empty)
             #:result (bytes-append* (reverse stream)))
            ((r (in-list request.records))
             ;; NOTE this presumes that no two stream-records of the same type can
             ;; ever appear: params is a single stream sent once, stdin is a
             ;; single stream also sent once, etc.
             #:when (= r.type self.type))
    r.body))


;;*** == fcgi-params ----------------------------------------------- *;;


;; TODO With current implementation we have to receive all fcgi-params packets to
;; parse name values, but with a streaming bit-string-case we could write partials
;; into a pipe and have its in-end parsed as we go, blocking as needed. Once read
;; it would simply notify relevant request passing it params.

(define <params> {<stream-record>})


(define/table (<params>:deliver connection)
  (define request (connection :requests self.id))
  (request:push self)
  (when self.complete?
    (let* ((stream (self:assemble #;from connection))
           (params (parse-name-values stream)))
      (set self :payload params)
      (set self :body stream)
      (set self :params params)
      ;; extract and run script
      (set request :thread
           (thread
            (thunk

             (write-bytes
              #"Content-type: text/html\r\n\r\n<html><body>hello fcgi</body></html>"
              request.out)

             (displayln
              (format "RESPONSE to ~a SENT" (request-id request)))

             ;; (run
             ;;  (script-of (get: params "SCRIPT_FILENAME"))
             ;;  #;with params
             ;;  #;for request)
             )))))
  self)


(module+ test

  (define params-chunk-1 (pack (make-record FCGI_PARAMS #:id 1) (ht ("SERVER_PORT" "80"))))
  (define params-chunk-2 (pack (make-record FCGI_PARAMS #:id 1) (ht ("SCRIPT_FILENAME" "foo.rkt"))))
  (define params-chunk-end (pack (make-record FCGI_PARAMS #:id 1) ""))

  ;; must be 8 bytes aligned
  (check zero? (remainder (bytes-length params-chunk-1) 8))
  (check zero? (remainder (bytes-length params-chunk-2) 8))

  (define fake-params-port
    (open-input-bytes
     (bytes-append params-chunk-1
                   params-chunk-2
                   params-chunk-end)))

  (check equal? (ht ("SERVER_PORT" "80"))
         (parse-name-values
          (get: (parse (make-record) fake-params-port) 'body)))

  (check equal? (ht ("SCRIPT_FILENAME" "foo.rkt"))
         (parse-name-values
          (get: (parse (make-record) fake-params-port) 'body)))

  ;; end-chunk completes stream
  (check-pred stream-record-complete? (parse (make-record) fake-params-port)))


;;*** == fcgi-stdin ------------------------------------------------ *;;


(define <stdin> {<stream-record>})


(define/table (<stdin>:deliver connection)
  (define request (connection :requests self.id))
  (request:push self)
  ;; TODO in principle we could start writing to request with say
  ;; write-bytes-avail* without waiting for the rest of FCGI_STDIN to arrive.
  ;; In fact spec explicitly says that request may begin its response before
  ;; the entire stdin is read. We don't do it atm. We choose to receive and
  ;; assemble the entire stream before delivering it ot the request. This is
  ;; due to the following complications that requere careful attention:
  ;;
  ;; 1. We might write part of the content, say due to buffering and request
  ;; being slow to receive, so we'd have to write in a loop until done. Best
  ;; done in a separate thread.
  ;;
  ;; 2. Since the stream could be split into several fcgi packets and we
  ;; deliver each independently we may introduce a race: earlier packet is
  ;; still writing while a later packet starts to write to the same port. Write
  ;; therefore must be ordered and complete before the next one begins. One way
  ;; is to guard the port with a semaphore. Cleaner solution might be queue
  ;; every fcgi-stdin packet write, so they are handled in order. Thread's
  ;; mailbox is a natural queue, so we could have a special writer thread
  ;; around, then delivering fcgi-stdin would simply amount to a thread-send.
  (when self.complete?
    (let* ((stream (bytes->string/utf-8
                    (self:assemble #;from connection))))
      (setf (self :content :body) stream)
      ;; throw-away thread just to push content to request
      (thread
       (thunk
        (write-bytes stream request.in)))))
  self)


(module+ test

  (define stdin-chunk-1 (pack (make-record FCGI_STDIN #:id 1) "hello"))
  (define stdin-chunk-2 (pack (make-record FCGI_STDIN #:id 1) " "))
  (define stdin-chunk-3 (pack (make-record FCGI_STDIN #:id 1) "world!"))
  (define stdin-chunk-end (pack (make-record FCGI_STDIN #:id 1) ""))

  ;; must be 8 bytes aligned
  (check zero? (remainder (bytes-length stdin-chunk-1) 8))
  (check zero? (remainder (bytes-length stdin-chunk-2) 8))
  (check zero? (remainder (bytes-length stdin-chunk-3) 8))
  (check zero? (remainder (bytes-length stdin-chunk-end) 8))

  (define fake-stdin-port
    (open-input-bytes
     (bytes-append stdin-chunk-1
                   stdin-chunk-2
                   stdin-chunk-3
                   stdin-chunk-end)))

  (check equal? #"hello"  (get: (parse (make-record) fake-stdin-port) 'body))
  (check equal? #" "      (get: (parse (make-record) fake-stdin-port) 'body))
  (check equal? #"world!" (get: (parse (make-record) fake-stdin-port) 'body))

  (check  stream-record-complete? (parse (make-record) fake-stdin-port)))



;;*** == fcgi-stdout ----------------------------------------------- *;;


;; TODO <stdout> and <stderr> are app -> browser, which means they are simply a
;; passthrough. We could inherit from a relevant <app-to-client> and
;; <stream-record> as an experiment. Then :deliver would be the same.
;;
;; Same goes for <data> and <stdin> that only differ in what channel they use.


(define <stdout> {<stream-record>})


(define/table (<stdout>:deliver connection)
  ;; TODO probably want it best effort without blocking
  (write-bytes (self:pack self.payload) connection.out)
  self)


;;*** == fcgi-stderr ----------------------------------------------- *;;


(define <stderr> {<stream-record>})


(define/table (<stderr>:deliver connection)
  ;; TODO probably want it best effort without blocking
  (write-bytes (self:pack self.payload) connection.out)
  self)


;;*** == fcgi-data ------------------------------------------------- *;;


(define <data> {<stream-record>})

;; TODO like stdin but we write to request-data pipe. Since this is only ever used
;; in filter role I'm not sure if its ever used in the wild at all.
(define/table (<data>:deliver connection)
  (define request (connection :requests self.id))
  (request:push self)
  (when self.complete?
    (let* ((stream (bytes->string/utf-8
                    (self:assemble #;from connection))))
      (setf (self :content :body) stream)
      ;; throw-away thread just to push content to request
      (thread
       (thunk
        ;; TODO if we unify request.data and request.stdin into just a sink, we
        ;; maybe able to implement :deliver on some prototype so that both <stdin>
        ;; and <data> can use it.
        (write-bytes stream request.data)))))
  self)


;;* Connection ------------------------------------------------------ *;;


(define <connection> {#:spec ((:in input-port?)
                              (:out output-port?)
                              (:reader thread?)
                              (:writer thread?)
                              (:requests table?))})


(define/table (<connection>:start-reader)
  (set self :reader (thread
                     (thunk
                      (let loop ()
                        (~> {<record>}
                            (~:parse #;from self.in)
                            (~:deliver #;to self))
                        (loop))))))


(define/table (<connection>:start-writer)
  (define connection self)
  (define ((read-bytes-and-deliver <fcgi-type> request) stream)
    (define bytes (read-bytes-avail! buffer stream))
    (~> {<fcgi-type>}
        (set ~ :id request.id)
        (set ~ :payload (cond
                          ((positive-integer? bytes)
                           (subbytes buffer 0 (sub1 bytes)))
                          ((eof-object? bytes) "")
                          (else (error
                                 "Expected bytes or eof from port, got: "
                                 bytes))))
        (~:deliver #;to connection)))
  (set self :writer
       (thread
        (thunk
         (let loop ()
           (define buffer (make-bytes 65535))
           (define mail (thread-receive-evt))
           (define evt (apply sync never-evt mail self.requests))
           (cond

             ((request? evt)
              ;; request sent some bytes
              (sync
               (handle-evt evt.out (read-bytes-and-deliver <fcgi-stdout> evt))
               (handle-evt evt.err (read-bytes-and-deliver <fcgi-stderr> evt))))

             ((eq? mail evt)
              ;; new request has been started
              (let ((msg (thread-receive)))
                (cond
                  ((isa? msg <fcgi-begin-request>)
                   ;; TODO log then loop
                   (void))
                  ((isa? msg <fcgi-get-values>)
                   ;; reply with the values we support
                   (~> {<fcgi-get-values-result>}
                       (set ~ :id FCGI_NULL_REQUEST_ID)
                       ;; TODO replace with actual values
                       (set ~ :payload {(:FCGI_MAX_CONNS  "4")
                                        (:FCGI_MAX_REQS   "4")
                                        (:FCGI_MPXS_CONNS "1")})
                       (~:deliver #;to connection)))
                  ;; TODO log and ignore
                  (else (void)))))

             ;; should never get here
             (else (error "Eh, should never happen")))
           (loop))))))


(define (main)
  (define tcp-port 9000)
  (define tcp-max-allow-wait 4)
  (define tcp-reuse? false)
  (define tcp-hostname "127.0.0.1")
  (define connection (tcp-listen tcp-port tcp-max-allow-wait tcp-reuse? tcp-hostname))

  (define connections empty)

  (let loop ()
    (let-values (((in out) (tcp-accept connection)))
      (~> {<connection>}
          (set ~ :requests empty)
          (set ~ :in in)
          (set ~ :out out)
          ;; TODO this implies tilda can handle ~.foo ~:foo ~..foo ~::foo
          (~:start-reader)
          (~:start-writer)
          (cons ~ connections)
          (set! connections ~)))
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
