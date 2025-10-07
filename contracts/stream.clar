;; Error constants
(define-constant ERR-UNAUTHORIZED (err u0))
(define-constant ERR-INVALID-SIGNATURE (err u1))
(define-constant ERR-STREAM-STILL-ACTIVE (err u2))
(define-constant ERR-INVALID-STREAM-ID (err u3))

;; Data variables
(define-data-var latest-stream-id uint u0)

;; Data maps
(define-map streams
  uint
  {
    sender: principal,
    recipient: principal,
    balance: uint,
    withdrawn-balance: uint,
    payment-per-block: uint,
    timeframe:     {
      start-block: uint,
      stop-block: uint,
    },
  }
)

;; Create a new stream
(define-public (stream-to
    (recipient principal)
    (initial-balance uint)
    (timeframe {
      start-block: uint,
      stop-block: uint,
    })
    (payment-per-block uint)
  )
  (let (
      (stream {
        sender: contract-caller,
        recipient: recipient,
        balance: initial-balance,
        withdrawn-balance: u0,
        payment-per-block: payment-per-block,
        timeframe: timeframe,
      })
      (current-stream-id (var-get latest-stream-id))
    )
    (try! (stx-transfer? initial-balance contract-caller (as-contract tx-sender)))
    (map-set streams current-stream-id stream)
    (var-set latest-stream-id (+ current-stream-id u1))
    (ok current-stream-id)
  )
)

;; Refuel an existing stream
(define-public (refuel
    (stream-id uint)
    (amount uint)
  )
  (let ((stream (unwrap! (map-get? streams stream-id) ERR-INVALID-STREAM-ID)))
    (asserts! (is-eq contract-caller (get sender stream)) ERR-UNAUTHORIZED)
    (try! (stx-transfer? amount contract-caller (as-contract tx-sender)))
    (map-set streams stream-id
      (merge stream { balance: (+ (get balance stream) amount) })
    )
    (ok amount)
  )
)

;; Calculate the number of blocks a stream has been active
(define-read-only (calculate-block-delta (timeframe {
  start-block: uint,
  stop-block: uint,
}))
  (let (
      (start-block (get start-block timeframe))
      (stop-block (get stop-block timeframe))
      (current-height stacks-block-height)
      (delta (if (<= current-height start-block)
        u0
        (if (< current-height stop-block)
          (- current-height start-block)
          (- stop-block start-block)
        )
      ))
    )
    delta
  )
)

;; Check balance for a party involved in a stream
(define-read-only (balance-of
    (stream-id uint)
    (who principal)
  )
  (let (
      (stream (unwrap! (map-get? streams stream-id) u0))
      (block-delta (calculate-block-delta (get timeframe stream)))
      (recipient-balance (* block-delta (get payment-per-block stream)))
    )
    (if (is-eq who (get recipient stream))
      (- recipient-balance (get withdrawn-balance stream))
      (if (is-eq who (get sender stream))
        (- (get balance stream) recipient-balance)
        u0
      )
    )
  )
)

;; Withdraw funds
(define-public (withdraw (stream-id uint))
  (let (
      (stream (unwrap! (map-get? streams stream-id) ERR-INVALID-STREAM-ID))
      (balance (balance-of stream-id contract-caller))
    )
    (asserts! (is-eq contract-caller (get recipient stream)) ERR-UNAUTHORIZED)
    (map-set streams stream-id
      (merge stream { withdrawn-balance: (+ (get withdrawn-balance stream) balance) })
    )
    (as-contract (try! (stx-transfer? balance tx-sender (get recipient stream))))
    (ok balance)
  )
)

;; Refund excess funds to sender when stream is over
(define-public (refund (stream-id uint))
  (let (
      (stream (unwrap! (map-get? streams stream-id) ERR-INVALID-STREAM-ID))
      (balance (balance-of stream-id (get sender stream)))
      (current-height stacks-block-height)
    )
    (asserts! (is-eq contract-caller (get sender stream)) ERR-UNAUTHORIZED)
    (asserts! (>= current-height (get stop-block (get timeframe stream)))
      ERR-STREAM-STILL-ACTIVE
    )
    (map-set streams stream-id
      (merge stream { balance: (- (get balance stream) balance) })
    )
    (as-contract (try! (stx-transfer? balance tx-sender (get sender stream))))
    (ok balance)
  )
)

;; Hash a stream for signature verification
(define-read-only (hash-stream
    (stream-id uint)
    (new-payment-per-block uint)
    (new-timeframe {
      start-block: uint,
      stop-block: uint,
    })
  )
  (let (
      (stream (unwrap! (map-get? streams stream-id) (sha256 0x00)))
      (msg (concat
        (concat (unwrap-panic (to-consensus-buff? stream))
          (unwrap-panic (to-consensus-buff? new-payment-per-block))
        )
        (unwrap-panic (to-consensus-buff? new-timeframe))
      ))
    )
    (sha256 msg)
  )
)

;; Signature verification function
(define-read-only (validate-signature
    (hash (buff 32))
    (signature (buff 65))
    (signer principal)
  )
  (is-eq (principal-of? (unwrap! (secp256k1-recover? hash signature) false))
    (ok signer)
  )
)

;; Update stream configuration
(define-public (update-details
    (stream-id uint)
    (payment-per-block uint)
    (timeframe {
      start-block: uint,
      stop-block: uint,
    })
    (signer principal)
    (signature (buff 65))
  )
  (let ((stream (unwrap! (map-get? streams stream-id) ERR-INVALID-STREAM-ID)))
    (asserts!
      (validate-signature (hash-stream stream-id payment-per-block timeframe)
        signature signer
      )
      ERR-INVALID-SIGNATURE
    )
    (asserts!
      (or
        (and (is-eq (get sender stream) contract-caller) (is-eq (get recipient stream) signer))
        (and (is-eq (get sender stream) signer) (is-eq (get recipient stream) contract-caller))
      )
      ERR-UNAUTHORIZED
    )
    (map-set streams stream-id
      (merge stream {
        payment-per-block: payment-per-block,
        timeframe: timeframe,
      })
    )
    (ok stream-id)
  )
)
