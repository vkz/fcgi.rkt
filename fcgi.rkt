#lang prelude


(require bitsyntax
         racket/generic
         racket/struct)


(module+ test
  (require rackunit))


;;* Structs --------------------------------------------------------- *;;


(struct connection (in out reader writer requests) #:mutable)


(struct request (id records params in out err thread) #:mutable
  #:methods gen:custom-write
  ((define write-proc
     (make-constructor-style-printer
      (λ (self) 'request)
      (λ (self) (list (record-id self)))))))


(struct record (id type content) #:mutable

  #:methods gen:fcgi

  ((define/generic delegate-parse parse)
   (define (parse self in)
     (bit-string-case (read-bytes 8 in)
       ([(record :: (fcgi-header))] (delegate-parse record in))
       (else (error "Failed to parse fcgi-header")))))

  #:methods gen:dict

  ((define/generic super-dict-ref dict-ref)
   (define (dict-ref dict key [default (λ () undefined)])
     (super-dict-ref (record-content dict) key default))

   (define/generic super-dict-set! dict-set!)
   (define (dict-set! dict key v)
     (super-dict-set! (record-content dict) key v))

   (define/generic super-dict-remove! dict-remove!)
   (define (dict-remove! dict key)
     (super-dict-remove! (record-content dict) key))

   (define/generic super-dict-iterate-first dict-iterate-first)
   (define (dict-iterate-first dict)
     (super-dict-iterate-first (record-content dict)))

   (define/generic super-dict-iterate-next dict-iterate-next)
   (define (dict-iterate-next dict pos)
     (super-dict-iterate-next (record-content dict) pos))

   (define/generic super-dict-iterate-key dict-iterate-key)
   (define (dict-iterate-key dict pos)
     (super-dict-iterate-key (record-content dict) pos))

   (define/generic super-dict-iterate-value dict-iterate-value)
   (define (dict-iterate-value dict pos)
     (super-dict-iterate-value (record-content dict) pos)))

  #:methods gen:custom-write

  ((define write-proc
     (make-constructor-style-printer
      (λ (self) 'record)
      (λ (self) (list (record-id self) (record-content self)))))))


;; base records
(struct stream-record     record (complete?) #:mutable)
(struct management-record record ()          #:mutable)


;; Standard ports for request struct
(struct stdin  (source sink) prop:input-port 0 prop:output-port 1)
(struct stdout (source sink) prop:input-port 0 prop:output-port 1)
(struct stderr (source sink) prop:input-port 0 prop:output-port 1)


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
    ((bytes?  thing) thing)
    ((string? thing) (string->bytes/utf-8 thing))
    ((dict?   thing) (pack-name-values thing))
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
  (check equal? #"\3\3bazduh\3\3foobar") (pack-name-values (ht ("foo" "bar")
                                                               ("baz" "duh"))))


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
        (ks (make-record type
                         #:id id
                         #:content (ht ('version version)
                                       ('type type)
                                       ('id id)
                                       ('clen clen)
                                       ('plen plen)))
            rest))
       (else (kf))))

    ;; parse into record passed as argument
    ((_ #t input ks kf r)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([version type (id :: bytes 2) (clen :: bytes 2) plen _ (rest :: binary)]
        (let ((h (ht ('version version)
                     ('type type)
                     ('id id)
                     ('clen clen)
                     ('plen plen))))
          (set-record-id! r id)
          (set-record-type! r type)
          (set-record-content! r (if (hash? (record-content r))
                                     (hash-union! (record-content r) h)
                                     h))
          (ks r rest)))
       (else (kf))))

    ;; unparse header from record
    ((_ #f r)
     (match-let (((kv version type id clen plen) (record-content r)))
       (bit-string (or type (record-type r) 0)
                   ((or id 0) :: bytes 2)
                   ((or clen 0) :: bytes 2)
                   ((or plen 0))
                   #;reserved 0)))))


(module+ test
  (define header (ht ('version 1) ('type 1) ('id 1) ('clen 3) ('plen 5)))
  (define bs (bit-string (header :: (fcgi-header))))

  (check eq? 8 (bit-string-byte-count bs))
  (check equal? '(1 1 0 1 0 3 5 0) (bytes->list (bit-string->bytes bs)))

  ;; (parse (unparse header)) == header
  (check equal? header
         (bit-string-case (bit-string->bytes bs)
           ([(header :: (fcgi-header))] header))))


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


;;* FCGI records ---------------------------------------------------- *;;


(define-generics fcgi
  (parse   fcgi #;from input-port)
  (deliver fcgi #;to connection)
  (pack    fcgi #;with content)
  (respond fcgi #;to request #;over output-port))


(define (make-record [type 'base]
                     #:id           [id           undefined]
                     #:content      [content      (ht)]
                     #:complete?    [complete?    false]
                     #:table        [table        (ht)]
                     #:type         [unknown-type undefined]
                     #:role         [role         undefined]
                     #:flags        [flags        undefined]
                     #:app-status   [app-status   undefined]
                     #:proto-status [proto-status undefined])
  (case type
    ((FCGI_GET_VALUES)        (fcgi-get-values        id type content table))
    ((FCGI_GET_VALUES_RESULT) (fcgi-get-values-result id type content table))
    ((FCGI_BEGIN_REQUEST)     (fcgi-begin-request     id type content role flags))
    ((FCGI_PARAMS)            (fcgi-params            id type content complete? table))
    ((FCGI_STDIN)             (fcgi-stdin             id type content))
    ((FCGI_STDOUT)            (fcgi-stdout            id type content))
    ((FCGI_STDERR)            (fcgi-stderr            id type content))
    ((FCGI_DATA)              (fcgi-data              id type content))
    ((FCGI_ABORT_REQUEST)     (fcgi-abort-request     id type content))
    ((FCGI_END_REQUEST)       (fcgi-end-request       id type content app-status proto-status))
    ((FCGI_UNKNOWN_TYPE)      (fcgi-unknown           id type content unknown-type))
    (('base)                  (record                 id type content))
    ;; Spec only mentions it once without providing any detail
    ;; ((FCGI_MAXTYPE)           (fcgi-maxtype id type content))
    ))


;; TODO wonder if I could extend some kind of stream interface to stream-record,
;; one that does this kind of assembly as needed? Would it be worth the effort?
(define (assemble-stream record #;from connection)
  (define request (get: (connection-requests connection) (record-id record)))
  (define records (request-records request))
  (for/fold ((stream empty)
             #:result (bytes-append* (reverse stream)))
            ((r (in-list records))
             #:when (= (record-type r) (record-type record)))
    (get: r 'body)))


(define (request-of #:id [id undefined]
                    #:record [record undefined]
                    connection)
  (set! id (or id (record-id record)))
  (get: (connection-requests connection) id))


(define (push! record #;onto request)
  (set-request-records! request (cons record (request-records request)))
  request)


;; NOTE parse and pack for fcgi-get-values, fcgi-get-values-result and fcgi-params
;; are almost exactly the same, so we pull impl out and reuse
(define (parse-fcgi/name-values record in)
  (let ((clen (get: record 'clen))
        (plen (get: record 'plen)))
    (bit-string-case (read-bytes (+ clen plen) in)
      ([(params :: binary bytes clen) (_ :: bytes plen)]
       (set! params (bit-string->bytes params))
       (set: record 'body params)
       (cond
         ((fcgi-get-values? record)
          (set-fcgi-get-values-table! record (parse-name-values params)))

         ((stream-record? record)
          (when (zero? clen)
            (set-stream-record-complete? true)))))
      (else
       (error "Failed to parse" (object-name record))))
    record))


(define (pack-fcgi/name-values record content)
  (set! content
        (pack-name-values
         (cond
           ((hash? content) content)
           ((fcgi-params? record) (fcgi-params-table record))
           ((fcgi-get-values? record) (fcgi-get-values-table record)))))
  (define clen (bytes-length content))
  (define-values (plen padding content) (pad content))
  (define header (ht ('version 1)
                     ('type (record-type record))
                     ('id (or (record-id record) FCGI_NULL_REQUEST_ID))
                     ('clen clen)
                     ('plen plen)))
  (bit-string->bytes
   (bit-string
    (header :: (fcgi-header))
    (content :: binary))))


;; fcgi records
(struct fcgi-stdout stream-record ()      #:mutable)
(struct fcgi-stderr stream-record ()      #:mutable)
(struct fcgi-data   stream-record ()      #:mutable)
(struct fcgi-abort-request record        ()      #:mutable)
(struct fcgi-end-request   record (app-status proto-status) #:mutable)
(struct fcgi-unknown           management-record (type)  #:mutable)


;;** - fcgi-get-values[-result] ------------------------------------- *;;


(struct fcgi-get-values management-record (table) #:mutable

  #:methods gen:fcgi

  ((define parse parse-fcgi/name-values)
   (define pack pack-fcgi/name-values)
   ;; NOTE connection-writer will respond with appropriate FCGI_GET_VALUES_RESULT
   (define (deliver record connection)
     (thread-send (connection-writer connection) record)
     record)))


(struct fcgi-get-values-result fcgi-get-values () #:mutable

  #:methods gen:fcgi

  ((define parse parse-fcgi/name-values)
   (define pack pack-fcgi/name-values)
   ;; NOTE this is to be called by connection-writer. Just being consistent.
   (define (deliver record connection)
     ;; TODO probably want it best effort without blocking
     (write-bytes (pack record) (connection-out connection))
     record)))


(module+ test
  (define get-values-table
    (ht ("FCGI_MAX_CONNS" "")
        ("FCGI_MAX_REQS" "")
        ("FCGI_MPXS_CONNS" "")))

  (define get-values-message
    (pack (make-record FCGI_GET_VALUES) get-values-table))

  (check equal? get-values-table (with-input-from-bytes get-values-message
                                   (parse (make-record) (current-input-port)))))


;;** - fcgi-begin-request ------------------------------------------- *;;


(struct fcgi-begin-request record (role flags) #:mutable

  #:methods gen:fcgi

  ((define (parse record in)
     (bit-string-case (read-bytes (+ (get: record 'clen) (get: record 'plen)) in)
       ([(role :: bytes 2) flags (_ :: bytes 5)]
        (set: record 'role role)
        (set: record 'flags flags)
        (set-fcgi-begin-request-role!  record role)
        (set-fcgi-begin-request-flags! record flags))
       (else
        (error "Failed to parse fcgi-begin-request")))
     record)


   (define (pack fcgi content)
     (define clen (bytes-length content))
     (define-values (plen padding content) (pad content))
     (define header (ht ('version 1)
                        ('type FCGI_BEGIN_REQUEST)
                        ('id (record-id fcgi))
                        ('clen clen)
                        ('plen plen)))
     (bit-string->bytes
      (bit-string
       (header :: (fcgi-header))
       (content :: binary))))


   (define (deliver record connection)
     ;; TODO check for duplicate id in requests
     (set: (connection-requests connection)
           (record-id record)
           (request (record-id record)
                    (list record)
                    (ht)
                    (let-values ((source sink) (make-pipe 65535)) (stdin source sink))
                    (let-values ((source sink) (make-pipe 65535)) (stdout source sink))
                    (let-values ((source sink) (make-pipe 65535)) (stderr source sink)))))))


(module+ test
  (define begin-request-message
    (bit-string->bytes
     (bit-string ((ht ('version 1) ('type 1) ('id 1) ('clen 8) ('plen 0)) :: (fcgi-header))
                 (FCGI_RESPONDER :: bytes 2)
                 (FCGI_KEEP_CONN :: bytes 1)
                 (0 :: bytes 5))))
  (check-pred hash? (parse-begin-request begin-request-message)))


;;** - fcgi-params -------------------------------------------------- *;;


;; TODO With current implementation we have to receive all fcgi-params packets to
;; parse name values, but with a streaming bit-string-case we could write partials
;; into a pipe and have its in-end parsed as we go, blocking as needed. Once read
;; it would simply notify relevant request passing it params.


(struct fcgi-params stream-record (table) #:mutable

  #:methods gen:fcgi

  ((define parse parse-fcgi/name-values)
   ;; NOTE technically the the content table to be packed as name-values maybe too
   ;; large for a single packet and would have to be split, but we'll only ever
   ;; use pack here for testing and implement it to offer consistent interface. If
   ;; this ever changes would need to consider packing into multiple packets.
   (define pack pack-fcgi/name-values)
   (define (deliver record connection)
     (define request (request-of #:record record connection))
     (push! record #;onto request)
     (when (stream-record-complete? record)
       (let* ((stream (assemble-stream record #;from connection))
              (params (parse-name-values stream)))
         (set-fcgi-params-table! record params)
         (set: record 'body stream)
         (set: record 'params params)
         ;; extract and run script
         (set-request-thread!
          request
          (thread
           (thunk
            ;; TODO decide what running a script even mean, we're in the
            ;; "web-framework" territory here, so think hard.
            (run
             (script-of (get: params "SCRIPT_FILENAME"))
             #;with params
             #;for request))))))
     record)))


;;** - fcgi-stdin --------------------------------------------------- *;;


(struct fcgi-stdin  stream-record () #:mutable

  #:methods gen:fcgi

  ((define (parse record in)
     (let ((clen (get: record 'clen))
           (plen (get: record 'plen)))
       (bit-string-case (read-bytes (+ clen plen) in)
         ([(input :: binary bytes clen) (_ :: bytes plen)]
          (set! input (bit-string->bytes input))
          (set: record 'body input)
          (when (zero? clen)
            (set-stream-record-complete? true)))
         (else
          (error "Failed to parse" 'fcgi-stdin)))
       record))


   (define (pack record content)
     (set! content (bytes-of content))
     (define clen (bytes-length content))
     (define-values (plen padding content) (pad content))
     (define header (ht ('version 1)
                        ('type FCGI_STDIN)
                        ('id (record-id record))
                        ('clen clen)
                        ('plen plen)))
     (bit-string->bytes
      (bit-string
       (header :: (fcgi-header))
       (content :: binary))))


   (define (deliver record connection)
     (define request (request-of #:record record connection))
     (push! record #;onto request)
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
     (when (stream-record-complete? record)
       (let* ((stream (bytes->string/utf-8
                       (assemble-stream record #;from connection))))
         (set: record 'body stream)
         ;; throw-away thread just to push content to request
         (thread
          (thunk
           (write-bytes stream (request-stdin request))))))
     record)))


(module+ test

  (define stdin-chunk-1 (pack (make-record FCGI_STDIN #:id 1) "hello"))
  (define stdin-chunk-2 (pack (make-record FCGI_STDIN #:id 1) " "))
  (define stdin-chunk-3 (pack (make-record FCGI_STDIN #:id 1) "world!"))
  (define stdin-chunk-end (pack (make-record FCGI_STDIN #:id 1) ""))

  ;; must be 8 bytes aligned
  (check zero? (remainder (bytes-length stdin-chunk-1) 8))
  (check zero? (remainder (bytes-length stdin-chunk-2) 8))
  (check zero? (remainder (bytes-length stdin-chunk-3) 8))
  (check zero? (remainder (bytes-length stdin-chunk-4) 8))

  (define fake-port
    (open-input-bytes
     (bytes-append stdin-chunk-1
                   stdin-chunk-2
                   stdin-chunk-3
                   stdin-chunk-end)))

  (check equal? #"hello"  (get: (parse (make-request) fake-request-stdin-port) 'body))
  (check equal? #" "      (get: (parse (make-request) fake-request-stdin-port) 'body))
  (check equal? #"world!" (get: (parse (make-request) fake-request-stdin-port) 'body))

  (check stream-request-complete? (parse (make-request) fake-request-stdin-port)))


;;* Connection ------------------------------------------------------ *;;


(define (start-reader connection)
  (define in (connection-in connection))
  (thread
   (thunk
    (let loop ()
      (deliver (parse (make-record) #;from in) #;to connection)
      (loop)))))


(define (start-writer connection)
  (define out (connection-out connection))
  (thread
   (thunk
    (define buffer (make-bytes 65535))
    (let loop ()
      (let* ((mail (thread-receive-evt))
             (evt (apply sync never-evt mail (connection-requests connection))))
        (cond

          ;; request sent some bytes
          ((request? evt)
           (sync
            (handle-evt
             ;; stdout?
             (request-out evt)
             (λ (stdout)
               (define bytes (read-bytes-avail! buffer stdout))
               (if (positive-integer? bytes)
                   (respond (pack (fcgi-stdout (request-id request)) (subbytes buffer 0 (sub1 bytes))) #;to evt #;over out)
                   (respond (pack (fcgi-stdout (request-id request)) eof) #;to evt #;over out))))
            (handle-evt
             ;; stderr?
             (request-err evt)
             (λ (stderr)
               (define bytes (read-bytes-avail! buffer stderr))
               (if (positive-integer? bytes)
                   (respond (pack (fcgi-stderr (request-id request)) (subbytes buffer 0 (sub1 bytes))) #;to evt #;over out)
                   (respond (pack (fcgi-stderr (request-id request)) eof) #;to evt #;over out))))))

          ;; new request has been started
          ((eq? mail evt)
           (let ((msg (thread-receive)))
             (cond
               ((fcgi-begin-request? msg) (loop))
               ((fcgi-get-values? msg) (respond-with FCGI_GET_VALUES_RESULT))
               (else (log-and-ignore))))
           (loop))

          ;; should never get here
          (else (error "Eh, should never happen")))

        ;; keep listening for output from live requests
        (loop))))))


(define (main)
  (define tcp-port 9000)
  (define tcp-max-allow-wait 4)
  (define tcp-reuse? false)
  (define tcp-hostname "127.0.0.1")
  (define connection (tcp-listen tcp-port tcp-max-allow-wait tcp-reuse? tcp-hostname))

  (define connections empty)

  (let loop ()
    (let-values ((in out) (tcp-accept connection))
      (let ((c (connection in out
                           #;reader undefined
                           #;writer undefined
                           #;requests empty)))
        (set-connection-reader! c (start-reader c))
        (set-connection-writer! c (start-writer c))
        (set! connections (cons c connections))))
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
