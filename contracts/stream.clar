(define-constant ERR_UNAUTHORIZED (err u0))
(define-constant ERR_INVALID_SIGNATURE (err u1))
(define-constant ERR_STREAM_STILL_ACTIVE (err u2))
(define-constant ERR_INVALID_STREAM_ID (err u3))


;; DATA vars

(define-data-var latest-stream-id uint u0)

;; Data maps

(define-map streams
  uint
  {
    sender: principal,
    recipient: principal,
    balance: uint,
    withdraw-balance: uint,
    payment-per-block: uint,
    timeframe: (tuple(start-block uint)(end-block uint)),
  }
)


  (define-public (stream-to
  (recipient principal)
  (initial-balance uint)
  (timeframe(tuple(start-block uint)(end-block uint)))
  (payment-per-block uint)
  )
  (let(
    (stream{
        sender: contract-caller,
        recipient: recipient,
        balance: initial-balance,
        withdraw-balance: u0,
        payment-per-block: payment-per-block,
        timeframe: timeframe
    })
    (current-stream-id (var-get latest-stream-id))

    (try! (STX-transfer? initial-balance contract-caller (as-contract tx-sender))

    (map-set streams current-stream-id stream)
    (var-set latest-stream-id (+ current-stream-id u1))
    (ok current-stream-id
  ))
 )))

    (define-public(refuel
    (stream-id uint)
    (amount uint)
    )
   (let (
    (stream unwrap!((map-get? streams stream-id)ERR_INVALID_STREAM_ID))
   )
   (asserts!(is-eq contract-caller (get sender stream) ERR_UNAUTHORIZED))
   (try! (STX-transfer? amount contract-caller (as-contract tx-sender))
   (map-set streams stream-id (merge stream {balance: (+ (get balance stream) amount)}))
   (ok amount)
   ) ))


   ;; calculate the number of blocks a stream as been active
   (define-read-only(calculate-block-delta
   (timeframe(tuple(start-block uint)(end-block uint)))
   )
   (let(
    (start-block(get start-block timeframe))
    (stop-block(get stop-block timeframe))

    (delta
    (if(<=block-height start-block)
    0u
    (if(< block-height stop-block)
       ;;then
       (- block-height start-block)
       ;;else
       (- stop-block start-block)
       ))
   )
   
   delta)
   ))


   ;; check balance for a party involved in a stream

   (define-read-only(balance-of
   (stream-id uint)
   (who principal))
   )(
    let(
        (stream(unwrap!((map-get? streams stream-id)u0)))
        (block-delta(calculate-block-delta(get timeframe stream)))
        (recipient-balance(* block-delta (get payment-per-block stream)))
    )

    if(is-eq who (get recipient stream))
    (- recipient-balance (get withdraw-balance stream))
    (if (is-eq who (get sender stream))
    (- (get balance stream) recipient-balance)
    (- recipient-balance (get withdraw-balance stream))
    if (is-eq who (get sender stream))
    (- (get balance stream) recipient-balance
    )
    u128
    
    )
   )