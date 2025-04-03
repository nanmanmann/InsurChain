;; InsurChain - Insurance Policy Management Contract

;; Constants
(define-constant contract-owner tx-sender)
(define-constant policy-active u1)
(define-constant policy-inactive u0)

;; Data Maps
(define-map policies
    { policy-id: uint }
    {
        owner: principal,
        premium: uint,
        coverage-amount: uint,
        status: uint,
        expiry: uint
    }
)

(define-map claims 
    { claim-id: uint }
    {
        policy-id: uint,
        amount: uint,
        status: (string-ascii 20),
        timestamp: uint
    }
)

;; Storage Variables
(define-data-var next-policy-id uint u1)
(define-data-var next-claim-id uint u1)

;; Public Functions

;; Create new insurance policy
(define-public (create-policy (premium uint) (coverage-amount uint) (duration uint))
    (let
        (
            (policy-id (var-get next-policy-id))
            (expiry-block (+ stacks-block-height duration))
        )
        (try! (stx-transfer? premium tx-sender contract-owner))
        (map-set policies
            { policy-id: policy-id }
            {
                owner: tx-sender,
                premium: premium,
                coverage-amount: coverage-amount,
                status: policy-active,
                expiry: expiry-block
            }
        )
        (var-set next-policy-id (+ policy-id u1))
        (ok policy-id)
    )
)

;; Submit insurance claim
(define-public (submit-claim (policy-id uint) (amount uint))
    (let
        (
            (claim-id (var-get next-claim-id))
            (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
        )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (asserts! (is-eq (get status policy) policy-active) (err u3))
        (asserts! (<= amount (get coverage-amount policy)) (err u4))
        
        (map-set claims
            { claim-id: claim-id }
            {
                policy-id: policy-id,
                amount: amount,
                status: "PENDING",
                timestamp: stacks-block-height
            }
        )
        (var-set next-claim-id (+ claim-id u1))
        (ok claim-id)
    )
)

;; Read-only functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
    (map-get? policies {policy-id: policy-id})
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
    (map-get? claims {claim-id: claim-id})
)

;; Check if policy is active
(define-read-only (is-policy-active (policy-id uint))
    (match (map-get? policies {policy-id: policy-id})
        policy (is-eq (get status policy) policy-active)
        false
    )
)


(define-public (renew-policy (policy-id uint) (duration uint))
    (let (
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
        (current-premium (get premium policy))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (try! (stx-transfer? current-premium tx-sender contract-owner))
        (map-set policies
            { policy-id: policy-id }
            {
                owner: (get owner policy),
                premium: current-premium,
                coverage-amount: (get coverage-amount policy),
                status: policy-active,
                expiry: (+ stacks-block-height duration)
            }
        )
        (ok true)
    )
)

(define-constant refund-percentage u70) ;; 70% refund
(define-constant hundred u100)

(define-public (cancel-policy (policy-id uint))
    (let (
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
        (refund-amount (/ (* (get premium policy) refund-percentage) hundred))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (asserts! (is-eq (get status policy) policy-active) (err u3))
        
        (try! (as-contract (stx-transfer? refund-amount contract-owner (get owner policy))))
        (map-set policies
            { policy-id: policy-id }
            {
                owner: (get owner policy),
                premium: (get premium policy),
                coverage-amount: (get coverage-amount policy),
                status: policy-inactive,
                expiry: (get expiry policy)
            }
        )
        (ok refund-amount)
    )
)

(define-public (upgrade-coverage (policy-id uint) (new-coverage-amount uint))
    (let (
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
        (coverage-difference (- new-coverage-amount (get coverage-amount policy)))
        (premium-increase (/ (* coverage-difference (get premium policy)) (get coverage-amount policy)))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (asserts! (is-eq (get status policy) policy-active) (err u3))
        (asserts! (> new-coverage-amount (get coverage-amount policy)) (err u4))
        
        (try! (stx-transfer? premium-increase tx-sender contract-owner))
        (map-set policies
            { policy-id: policy-id }
            {
                owner: (get owner policy),
                premium: (+ (get premium policy) premium-increase),
                coverage-amount: new-coverage-amount,
                status: (get status policy),
                expiry: (get expiry policy)
            }
        )
        (ok true)
    )
)

(define-constant emergency-status "EMERGENCY")
(define-constant emergency-multiplier u150) ;; 150% of normal coverage

(define-public (submit-emergency-claim (policy-id uint) (amount uint))
    (let (
        (claim-id (var-get next-claim-id))
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
        (max-emergency-coverage (/ (* (get coverage-amount policy) emergency-multiplier) hundred))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (asserts! (is-eq (get status policy) policy-active) (err u3))
        (asserts! (<= amount max-emergency-coverage) (err u4))
        
        (map-set claims
            { claim-id: claim-id }
            {
                policy-id: policy-id,
                amount: amount,
                status: emergency-status,
                timestamp: stacks-block-height
            }
        )
        (var-set next-claim-id (+ claim-id u1))
        (ok claim-id)
    )
)

