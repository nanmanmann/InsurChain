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
        (new-expiry (+ stacks-block-height duration))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (try! (stx-transfer? (get premium policy) tx-sender contract-owner))
        (map-set policies
            { policy-id: policy-id }
            {
                owner: (get owner policy),
                premium: (get premium policy),
                coverage-amount: (get coverage-amount policy),
                status: policy-active,
                expiry: new-expiry
            }
        )
        (ok true)
    )
)



(define-public (cancel-policy (policy-id uint))
    (let (
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (asserts! (is-eq (get status policy) policy-active) (err u3))
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
        (ok true)
    )
)

(define-public (approve-claim (claim-id uint))
    (let (
        (claim (unwrap! (map-get? claims {claim-id: claim-id}) (err u1)))
    )
        (asserts! (is-eq (get status claim) "PENDING") (err u2))
        (map-set claims
            { claim-id: claim-id }
            {
                policy-id: (get policy-id claim),
                amount: (get amount claim),
                status: "APPROVED",
                timestamp: (get timestamp claim)
            }
        )
        (ok true)
    )
)


(define-constant health-insurance u1)
(define-constant life-insurance u2)
(define-constant property-insurance u3)

(define-map policy-categories
    { policy-id: uint }
    { category: uint }
)

(define-public (set-policy-category (policy-id uint) (category uint))
    (let (
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (map-set policy-categories
            { policy-id: policy-id }
            { category: category }
        )
        (ok true)
    )
)


(define-map claim-evidence
    { claim-id: uint }
    { evidence-hash: (string-ascii 64), timestamp: uint }
)

(define-public (submit-claim-evidence (claim-id uint) (evidence-hash (string-ascii 64)))
    (let (
        (claim (unwrap! (map-get? claims {claim-id: claim-id}) (err u1)))
    )
        (map-set claim-evidence
            { claim-id: claim-id }
            { 
                evidence-hash: evidence-hash,
                timestamp: stacks-block-height
            }
        )
        (ok true)
    )
)




(define-public (transfer-policy (policy-id uint) (new-owner principal))
    (let (
        (policy (unwrap! (map-get? policies {policy-id: policy-id}) (err u1)))
    )
        (asserts! (is-eq (get owner policy) tx-sender) (err u2))
        (map-set policies
            { policy-id: policy-id }
            {
                owner: new-owner,
                premium: (get premium policy),
                coverage-amount: (get coverage-amount policy),
                status: (get status policy),
                expiry: (get expiry policy)
            }
        )
        (ok true)
    )
)
