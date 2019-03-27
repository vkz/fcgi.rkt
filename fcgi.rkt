#lang prelude


(require bitsyntax
         racket/generic
         racket/struct)


(module+ test
  (require rackunit))


(define-generics fcgi
  (parse   fcgi #;from input-port)
  (deliver fcgi #;to connection)
  (pack    fcgi #;with content)
  (respond fcgi #;to request #;over output-port))


;;* Structs --------------------------------------------------------- *;;


(struct connection (in out reader writer requests) #:mutable)


(struct request (id records params in out err thread) #:mutable
  #:methods gen:custom-write
  ((define write-proc
     (make-constructor-style-printer
      (λ (self) 'request)
      (λ (self) (list (record-id self)))))))


;; record struct gets created by a reader every time it a new fcgi packet is read.
(struct record (id type content) #:mutable

  ;; content keys: type, clen, plen, body

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
(struct stream-record          record            (complete?)               #:mutable)
(struct management-record      record            ()                        #:mutable)
;; fcgi records
(struct fcgi-get-values        management-record (table)             #:mutable)
(struct fcgi-get-values-result management-record (table)             #:mutable)
(struct fcgi-unknown           management-record (type)                    #:mutable)

(struct fcgi-begin-request record (role flags) #:mutable
  #:methods gen:fcgi
  ((define parse parse-fcgi-begin-request)
   (define pack pack-fcgi-begin-request)
   (define deliver deliver-fcgi-begin-request)))

(struct fcgi-params            stream-record     (table)             #:mutable)
(struct fcgi-stdin             stream-record     ()                        #:mutable)
(struct fcgi-data              stream-record     ()                        #:mutable)
(struct fcgi-stdout            stream-record     ()                        #:mutable)
(struct fcgi-stderr            stream-record     ()                        #:mutable)
(struct fcgi-abort-request     record            ()                        #:mutable)
(struct fcgi-end-request       record            (app-status proto-status) #:mutable)


(define (fcgi-record-from fcgi-type)
  (define make-record
    (case (fcgi-type)
      ((FCGI_BEGIN_REQUEST)     fcgi-begin-request)
      ((FCGI_ABORT_REQUEST)     fcgi-abort-request)
      ((FCGI_END_REQUEST)       fcgi-end-request)
      ((FCGI_PARAMS)            fcgi-params)
      ((FCGI_STDIN)             fcgi-stdin)
      ((FCGI_STDOUT)            fcgi-stdout)
      ((FCGI_STDERR)            fcgi-stderr)
      ((FCGI_DATA)              fcgi-data)
      ((FCGI_GET_VALUES)        fcgi-get-values)
      ((FCGI_GET_VALUES_RESULT) fcgi-get-values-result)
      ((FCGI_UNKNOWN_TYPE)      fcgi-unknown-type)
      ((FCGI_MAXTYPE)           fcgi-maxtype)))
  (make-record))


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


;;* Parsers --------------------------------------------------------- *;;


(define-syntax fcgi-header
  (syntax-rules ()

    ;; parse and create a record
    ((_ #t input ks kf)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([version type (id :: bytes 2) (clen :: bytes 2) plen _ (rest :: binary)]
        (let ((r (fcgi-record-from type)))
          (set-record-id! r id)
          (set-record-type! r type)
          (set-record-content! r (ht ('version version) ('type type) ('id id) ('clen clen) ('plen plen)))
          (ks r rest)))
       (else (kf))))

    ;; parse into record passed as argument
    ((_ #t input ks kf r)
     (bit-string-case input
       #:on-short (λ (fail) (kf #t))
       ([version type (id :: bytes 2) (clen :: bytes 2) plen _ (rest :: binary)]
        (set-record-id! r id)
        (set-record-type! r type)
        (set-record-content! r (ht ('version version) ('type type) ('id id) ('clen clen) ('plen plen)))
        (ks r rest))
       (else (kf))))

    ;; unparse header from record
    ((_ #f r)
     (match-let (((kv version type id clen plen) (record-content r)))
       (bit-string (or type (record-type r) 0)
                   ((or id 0) :: bytes 2)
                   ((or clen 0) :: bytes 2)
                   ((or plen 0))
                   #;reserved 0)))))


(define (pad content)
  (define clen (bytes-length content))
  (define-values (quotient remainder) (quotient/remainder clen 8))
  (define plen (if (zero? quotient) 0 (- (* (add1 quotient) 8) clen)))
  (define padding (make-bytes plen))
  (values plen padding (bytes-append content padding)))


(define (push! record #;onto request)
  (set-request-records! request (cons record (request-records request)))
  request)


;;** - fcgi-begin-request -------------------------------------------- *;;


(define (parse-fcgi-begin-request record in)
  (bit-string-case (read-bytes (+ (get: record 'clen) (get: record 'plen)) in)
    ([(role :: bytes 2) flags (_ :: bytes 5)]
     (set: record 'role role)
     (set: record 'flags flags)
     (set-fcgi-begin-request-role!  record role)
     (set-fcgi-begin-request-flags! record flags))
    (else
     (error "Failed to parse fcgi-begin-request")))
  record)


(define (pack-fcgi-begin-request fcgi content)
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


(struct stdin  (source sink) prop:input-port 0 prop:output-port 1)
(struct stdout (source sink) prop:input-port 0 prop:output-port 1)
(struct stderr (source sink) prop:input-port 0 prop:output-port 1)


(define (deliver-fcgi-begin-request record connection)
  ;; TODO check for duplicate id in requests
  (set: (connection-requests connection)
        (record-id record)
        (request (record-id record)
                 (list record)
                 (ht)
                 (let-values ((source sink) (make-pipe 65535)) (stdin source sink))
                 (let-values ((source sink) (make-pipe 65535)) (stdout source sink))
                 (let-values ((source sink) (make-pipe 65535)) (stderr source sink)))))


(module+ test

  (define header (ht ('version 1) ('type 1) ('id 1) ('clen 3) ('plen 5)))
  (define bs (bit-string (header :: (fcgi-header))))

  (check eq? 8 (bit-string-byte-count bs))
  (check equal? '(1 1 0 1 0 3 5 0) (bytes->list (bit-string->bytes bs)))

  ;; (parse (unparse header)) == header
  (check equal? header
         (bit-string-case (bit-string->bytes bs)
           ([(header :: (fcgi-header))] header)))

  (define begin-request-message
    (bit-string->bytes
     (bit-string ((ht ('version 1) ('type 1) ('id 1) ('clen 8) ('plen 0)) :: (fcgi-header))
                 (FCGI_RESPONDER :: bytes 2)
                 (FCGI_KEEP_CONN :: bytes 1)
                 (0 :: bytes 5))))

  (check-pred hash? (parse-begin-request begin-request-message)))


;;** - fcgi-params --------------------------------------------------- *;;


(define (empty-bytes? bytes)
  (and (bytes? bytes)
       (equal? #"" bytes)))


(define (bytes-of thing)
  (cond
    ((bytes?   thing) thing)
    ((string?  thing) (string->bytes/utf-8 thing))
    ;; TODO this actually packs an int, but we'd typically want to pack int's
    ;; visual representation, right?
    ;; ((integer? thing) (integer->integer-bytes
    ;;                    thing
    ;;                    (cond
    ;;                      ;; fits 1 byte
    ;;                      ((thing . < . (arithmetic-shift 1 8))  1)
    ;;                      ;; fits 2 bytes
    ;;                      ((thing . < . (arithmetic-shift 1 16)) 2)
    ;;                      ;; fits 4 bytes
    ;;                      ((thing . < . (arithmetic-shift 1 32)) 4)
    ;;                      ;; ((thing . < . (arithmetic-shift 1 64)) 8)
    ;;                      (else (error "Integer does not fit into 4 bytes")))
    ;;                    ;; unsigned
    ;;                    false
    ;;                    ;; big-endian
    ;;                    true))
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

(define (parse-fcgi-params record in)
  (let ((clen (get: record 'clen))
        (plen (get: record 'plen)))
    (bit-string-case (read-bytes (+ clen plen) in)
      ([(params :: binary bytes clen) (_ :: bytes plen)]
       (set! params (bit-string->bytes params))
       (set: record 'body params)
       (when (equal? #"" params)
         (set-stream-record-complete? true)))
      (else
       (error "Failed to parse fcgi-params")))
    record))


(define (pack-fcgi-params record content)
  (if (hash? content)
      (set! content (pack-name-values content))
      (set! content (pack-name-values (fcgi-params-table record))))
  (define clen (bytes-length content))
  (define-values (plen padding content) (pad content))
  (define header (ht ('version 1)
                     ('type FCGI_PARAMS)
                     ('id (record-id record))
                     ('clen clen)
                     ('plen plen)))
  (bit-string->bytes
   (bit-string
    (header :: (fcgi-header))
    (content :: binary))))


(define (deliver-fcgi-params record connection)
  (define request (request-of #:record record connection))
  (push! record #;onto request)
  (when (stream-record-complete? record)
    (let* ((stream (assemble-stream record #;from connection))
           (params (parse-name-values stream)))
      (set-fcgi-params-table! record params)
      (set: record 'body stream)
      (set: record 'params params)
      ;; now we should be able to extract script or function to run
      (set-request-thread!
       request
       (thread
        (thunk
         (run
          (script-of (get: params "SCRIPT_FILENAME"))
          #;with params
          #;for request))))))
  record)


;;* Connection ------------------------------------------------------ *;;


(define (start-reader connection)
  (define in (connection-in connection))
  (thread
   (thunk
    (let loop ((fcgi-type (peek-byte in #;skip 1)))
      (deliver #;fcgi-record
               (parse (fcgi-record-from fcgi-type) #;from in)
               #;to
               connection)
      (loop (peek-byte in #;skip 1))))))


(define (start-writer connection)
  (define out (connection-out connection))
  (thread
   (thunk
    (define buffer (make-bytes 65535))
    (let loop ()
      (let ((evt (apply sync never-evt new-request-added (connection-requests connection))))
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
          ((new-request-added? evt) (loop))

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
