;; Expertise Network - Decentralized Knowledge Exchange Platform
;; A system for peer exchange of knowledge hours facilitated through crypto token payments
;; This smart contract enables a marketplace for expertise trading

;; Administrative Constants
(define-constant admin-address tx-sender)
(define-constant error-admin-restricted (err u300))
(define-constant error-funds-depleted (err u301))
(define-constant error-expertise-invalid (err u302))
(define-constant error-compensation-invalid (err u303))
(define-constant error-network-capacity-exceeded (err u304))
(define-constant error-forbidden-action (err u305))

;; Platform Configuration Variables
(define-data-var hourly-compensation-base uint u10) ;; Base compensation per hour (in microstacks)
(define-data-var network-commission uint u10) ;; Commission percentage taken by the platform (e.g., 10%)
(define-data-var expertise-hour-capacity-total uint u0) ;; Current network capacity of expertise hours
(define-data-var expertise-hour-capacity-maximum uint u1000) ;; Maximum allowed network capacity
(define-data-var expertise-hour-limit-per-member uint u100) ;; Maximum contributions per member

;; Storage Structures
(define-map member-expertise-holdings principal uint) ;; Member's expertise balance in hours
(define-map member-token-holdings principal uint) ;; Member's token balance
(define-map expertise-offerings {member: principal} {hours-available: uint, compensation-rate: uint})

;; Helper Functions

;; Calculate the platform's commission on a transaction
(define-private (compute-network-commission (transaction-value uint))
  (/ (* transaction-value (var-get network-commission)) u100))

;; Update the network's available expertise capacity
(define-private (modify-expertise-capacity (change-amount int))
  (let (
    (current-capacity (var-get expertise-hour-capacity-total))
    (adjusted-capacity (if (< change-amount 0)
                     (if (>= current-capacity (to-uint (- 0 change-amount)))
                         (- current-capacity (to-uint (- 0 change-amount)))
                         u0)
                     (+ current-capacity (to-uint change-amount))))
  )
    (asserts! (<= adjusted-capacity (var-get expertise-hour-capacity-maximum)) error-network-capacity-exceeded)
    (var-set expertise-hour-capacity-total adjusted-capacity)
    (ok true)))

;; Core Transaction Functions

;; List expertise hours on the marketplace
(define-public (list-expertise-hours (hours uint) (rate uint))
  (let (
    (current-holdings (default-to u0 (map-get? member-expertise-holdings tx-sender)))
    (current-listing (get hours-available (default-to {hours-available: u0, compensation-rate: u0} 
                                           (map-get? expertise-offerings {member: tx-sender}))))
    (total-listed (+ hours current-listing))
  )
    (asserts! (> hours u0) error-expertise-invalid) ;; Hours must be positive
    (asserts! (> rate u0) error-compensation-invalid) ;; Rate must be positive
    (asserts! (>= current-holdings total-listed) error-funds-depleted)
    (try! (modify-expertise-capacity (to-int hours)))
    (map-set expertise-offerings {member: tx-sender} {hours-available: total-listed, compensation-rate: rate})
    (ok true)))

;; Purchase expertise hours from another member
(define-public (purchase-expertise (provider principal) (hours uint))
  (let (
    (listing-details (default-to {hours-available: u0, compensation-rate: u0} 
                      (map-get? expertise-offerings {member: provider})))
    (transaction-cost (* hours (get compensation-rate listing-details)))
    (platform-fee (compute-network-commission transaction-cost))
    (total-transaction-cost (+ transaction-cost platform-fee))
    (provider-expertise-balance (default-to u0 (map-get? member-expertise-holdings provider)))
    (purchaser-token-balance (default-to u0 (map-get? member-token-holdings tx-sender)))
    (provider-token-balance (default-to u0 (map-get? member-token-holdings provider)))
  )
    (asserts! (not (is-eq tx-sender provider)) error-forbidden-action)
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (>= (get hours-available listing-details) hours) error-funds-depleted)
    (asserts! (>= provider-expertise-balance hours) error-funds-depleted)
    (asserts! (>= purchaser-token-balance total-transaction-cost) error-funds-depleted)

    ;; Update provider's expertise balance and listing
    (map-set member-expertise-holdings provider (- provider-expertise-balance hours))
    (map-set expertise-offerings {member: provider} 
             {hours-available: (- (get hours-available listing-details) hours), 
              compensation-rate: (get compensation-rate listing-details)})

    ;; Update purchaser's token and expertise balance
    (map-set member-token-holdings tx-sender (- purchaser-token-balance total-transaction-cost))
    (map-set member-expertise-holdings tx-sender (+ (default-to u0 (map-get? member-expertise-holdings tx-sender)) hours))

    ;; Update provider's token balance
    (map-set member-token-holdings provider (+ provider-token-balance transaction-cost))

    ;; Add commission to platform admin's balance
    (map-set member-token-holdings admin-address 
             (+ (default-to u0 (map-get? member-token-holdings admin-address)) platform-fee))

    (ok true)))

;; Add new expertise hours to a member's account
(define-public (acquire-expertise-hours (hours uint))
  (let (
    (member tx-sender)
    (current-expertise (default-to u0 (map-get? member-expertise-holdings member)))
    (maximum-allowed (var-get expertise-hour-limit-per-member))
    (acquisition-cost (* hours (var-get hourly-compensation-base)))
    (member-tokens (default-to u0 (map-get? member-token-holdings member)))
  )
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (<= (+ current-expertise hours) maximum-allowed) (err u306))
    (asserts! (>= member-tokens acquisition-cost) error-funds-depleted)

    ;; Update member balances
    (map-set member-expertise-holdings member (+ current-expertise hours))
    (map-set member-token-holdings member (- member-tokens acquisition-cost))

    ;; Credit platform admin
    (map-set member-token-holdings admin-address 
             (+ (default-to u0 (map-get? member-token-holdings admin-address)) acquisition-cost))

    (ok true)))

;; Update platform configuration parameters (admin only)
(define-public (configure-platform-parameters (new-base-rate uint) 
                                              (new-commission uint) 
                                              (new-member-limit uint) 
                                              (new-capacity-limit uint))
  (begin
    (asserts! (is-eq tx-sender admin-address) error-admin-restricted)
    (asserts! (> new-base-rate u0) error-compensation-invalid)
    (asserts! (<= new-commission u30) (err u308)) ;; Commission capped at 30%
    (asserts! (> new-member-limit u0) (err u309)) ;; Member limit must be positive
    (asserts! (>= new-capacity-limit (var-get expertise-hour-capacity-total)) (err u310))

    ;; Update configuration parameters
    (var-set hourly-compensation-base new-base-rate)
    (var-set network-commission new-commission)
    (var-set expertise-hour-limit-per-member new-member-limit)
    (var-set expertise-hour-capacity-maximum new-capacity-limit)
    (ok true)))

;; Member Verification System for Premium Expertise
(define-map verified-members principal bool)
(define-map premium-expertise-offerings {member: principal} {hours-available: uint, compensation-rate: uint, credential-verified: bool})

;; Offer premium verified expertise (requires verification)
(define-public (list-premium-expertise (hours uint) (rate uint))
  (let (
    (current-expertise (default-to u0 (map-get? member-expertise-holdings tx-sender)))
    (verification-status (default-to false (map-get? verified-members tx-sender)))
    (current-offering (get hours-available (default-to {hours-available: u0, compensation-rate: u0} 
                                            (map-get? expertise-offerings {member: tx-sender}))))
    (total-offered (+ hours current-offering))
  )
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> rate u0) error-compensation-invalid)
    (asserts! verification-status (err u311)) ;; Must be verified
    (asserts! (>= current-expertise total-offered) error-funds-depleted)
    (try! (modify-expertise-capacity (to-int hours)))

    ;; Update standard offerings
    (map-set expertise-offerings {member: tx-sender} 
             {hours-available: total-offered, compensation-rate: rate})

    ;; Register premium offering
    (map-set premium-expertise-offerings {member: tx-sender} 
             {hours-available: hours, compensation-rate: rate, credential-verified: true})

    (ok true)))

;; Deposit tokens to member's platform balance
(define-public (deposit-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? member-token-holdings tx-sender)))
    (updated-balance (+ current-balance amount))
  )
    (asserts! (> amount u0) (err u306))

    ;; Transfer tokens from sender to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))

    ;; Update member's token balance
    (map-set member-token-holdings tx-sender updated-balance)
    (ok true)))

;; Withdraw tokens from member's platform balance
(define-public (withdraw-tokens (amount uint))
  (let (
    (current-balance (default-to u0 (map-get? member-token-holdings tx-sender)))
    (contract-balance (as-contract (stx-get-balance tx-sender)))
  )
    (asserts! (> amount u0) (err u306))
    (asserts! (>= current-balance amount) error-funds-depleted)
    (asserts! (>= contract-balance amount) error-funds-depleted)

    ;; Transfer tokens from contract to member
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))

    ;; Update member's token balance
    (map-set member-token-holdings tx-sender (- current-balance amount))

    (ok true)))

;; Member Reputation System
(define-map member-ratings {expert: principal, evaluator: principal} uint)
(define-map member-reputation principal {cumulative-rating: uint, evaluation-count: uint})

;; Rate an expertise provider after exchange
(define-public (evaluate-expert (expert principal) (rating uint))
  (let (
    (expert-data (default-to {cumulative-rating: u0, evaluation-count: u0} 
                 (map-get? member-reputation expert)))
    (existing-total (get cumulative-rating expert-data))
    (existing-count (get evaluation-count expert-data))
    (updated-total (+ existing-total rating))
    (updated-count (+ existing-count u1))
  )
    (asserts! (not (is-eq tx-sender expert)) error-forbidden-action)
    (asserts! (>= rating u1) (err u312))
    (asserts! (<= rating u5) (err u313))

    ;; Record this specific rating
    (map-set member-ratings {expert: expert, evaluator: tx-sender} rating)

    ;; Update expert's overall reputation
    (map-set member-reputation expert 
             {cumulative-rating: updated-total, evaluation-count: updated-count})

    (ok true)))

;; Expertise Package System with Discounts
(define-map expertise-packages {member: principal} 
            {hours-available: uint, compensation-rate: uint, discount-percentage: uint})

;; Create an expertise package with discount
(define-public (create-expertise-package (hours uint) (rate uint) (discount-percentage uint))
  (let (
    (current-expertise (default-to u0 (map-get? member-expertise-holdings tx-sender)))
    (current-offering (get hours-available (default-to {hours-available: u0, compensation-rate: u0} 
                                            (map-get? expertise-offerings {member: tx-sender}))))
    (existing-package (default-to {hours-available: u0, compensation-rate: u0, discount-percentage: u0} 
                       (map-get? expertise-packages {member: tx-sender})))
    (updated-offering (+ hours current-offering))
    (total-package-hours (+ hours (get hours-available existing-package)))
  )
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> rate u0) error-compensation-invalid)
    (asserts! (> discount-percentage u0) (err u314))
    (asserts! (<= discount-percentage u50) (err u315))
    (asserts! (>= current-expertise updated-offering) error-funds-depleted)

    ;; Update expertise capacity
    (try! (modify-expertise-capacity (to-int hours)))

    ;; Update expertise offerings
    (map-set expertise-offerings {member: tx-sender} 
             {hours-available: updated-offering, compensation-rate: rate})

    ;; Create or update expertise package
    (map-set expertise-packages {member: tx-sender} 
             {hours-available: total-package-hours, 
              compensation-rate: rate, 
              discount-percentage: discount-percentage})

    (ok true)))

;; Group Learning Session System
(define-map learning-sessions uint 
            {organizer: principal, 
             participants: (list 10 principal), 
             hours-allocated: uint, 
             compensation-rate: uint, 
             session-status: (string-ascii 20)})
(define-data-var session-counter uint u0)

;; Create a group learning session
(define-public (organize-learning-session (participants (list 10 principal)) (hours uint) (rate uint))
  (let (
    (current-expertise (default-to u0 (map-get? member-expertise-holdings tx-sender)))
    (session-id (var-get session-counter))
    (participant-count (len participants))
    (total-hours-required (* hours participant-count))
  )
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (> rate u0) error-compensation-invalid)
    (asserts! (>= current-expertise total-hours-required) error-funds-depleted)

    ;; Update expertise capacity
    (try! (modify-expertise-capacity (to-int total-hours-required)))

    ;; Update organizer's expertise balance
    (map-set member-expertise-holdings tx-sender (- current-expertise total-hours-required))

    ;; Increment session counter
    (var-set session-counter (+ session-id u1))

    (ok session-id)))

;; Reclaim expertise hours previously offered but not exchanged
(define-public (reclaim-offered-expertise (hours uint))
  (let (
    (offering-data (default-to {hours-available: u0, compensation-rate: u0} 
                    (map-get? expertise-offerings {member: tx-sender})))
    (listed-hours (get hours-available offering-data))
    (member-balance (default-to u0 (map-get? member-expertise-holdings tx-sender)))
  )
    (asserts! (> hours u0) error-expertise-invalid)
    (asserts! (>= listed-hours hours) error-funds-depleted)

    ;; Update member's expertise offering
    (map-set expertise-offerings {member: tx-sender} {
      hours-available: (- listed-hours hours),
      compensation-rate: (get compensation-rate offering-data)
    })

    ;; Update member's expertise balance
    (map-set member-expertise-holdings tx-sender member-balance)

    ;; Check and update premium offerings if present
    (if (is-some (map-get? premium-expertise-offerings {member: tx-sender}))
        (let (
          (premium-data (unwrap-panic (map-get? premium-expertise-offerings {member: tx-sender})))
          (premium-hours (get hours-available premium-data))
        )
          (if (>= premium-hours hours)
              (map-set premium-expertise-offerings {member: tx-sender} {
                hours-available: (- premium-hours hours),
                compensation-rate: (get compensation-rate premium-data),
                credential-verified: (get credential-verified premium-data)
              })
              (map-delete premium-expertise-offerings {member: tx-sender})
          )
        )
        true
    )

    (ok true)))

