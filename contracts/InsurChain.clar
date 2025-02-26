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
