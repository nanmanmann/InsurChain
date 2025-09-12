;; LoyaltyRewards - Policy Loyalty Rewards System for InsurChain
;; Rewards long-term policyholders with loyalty points and premium discounts

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized u401)
(define-constant err-insufficient-points u402)
(define-constant err-invalid-amount u405)

;; Loyalty tiers and requirements
(define-constant bronze-threshold u1000)
(define-constant silver-threshold u5000)
(define-constant gold-threshold u15000)
(define-constant points-per-stx u10)
(define-constant basis-points u10000)

;; Data Maps
(define-map loyalty-accounts
    { user: principal }
    {
        total-points: uint,
        points-balance: uint,
        tier: (string-ascii 10),
        policies-held: uint,
        total-premiums-paid: uint,
        last-activity: uint
    }
)

;; Data Variables
(define-data-var total-loyalty-members uint u0)
(define-data-var total-points-issued uint u0)

;; Private Helper Functions
(define-private (get-tier-from-points (points uint))
    (if (>= points gold-threshold) "GOLD"
        (if (>= points silver-threshold) "SILVER"
            (if (>= points bronze-threshold) "BRONZE" "NONE"))))

(define-private (get-tier-discount (tier (string-ascii 10)))
    (if (is-eq tier "GOLD") u750
        (if (is-eq tier "SILVER") u500
            (if (is-eq tier "BRONZE") u250 u0))))

(define-private (get-loyalty-account-or-default (user principal))
    (default-to
        { total-points: u0, points-balance: u0, tier: "NONE", policies-held: u0, total-premiums-paid: u0, last-activity: u0 }
        (map-get? loyalty-accounts { user: user })))

;; Public Functions
(define-public (award-loyalty-points (user principal) (premium-paid uint) (policy-id uint))
    (let (
        (current-account (get-loyalty-account-or-default user))
        (base-points (* premium-paid points-per-stx))
        (new-total-points (+ (get total-points current-account) base-points))
        (new-tier (get-tier-from-points new-total-points))
        (is-new-member (is-eq (get last-activity current-account) u0))
    )
        (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
        (asserts! (> premium-paid u0) (err err-invalid-amount))

        ;; Update loyalty account
        (map-set loyalty-accounts
            { user: user }
            {
                total-points: new-total-points,
                points-balance: (+ (get points-balance current-account) base-points),
                tier: new-tier,
                policies-held: (+ (get policies-held current-account) u1),
                total-premiums-paid: (+ (get total-premiums-paid current-account) premium-paid),
                last-activity: stacks-block-height
            }
        )

        (var-set total-points-issued (+ (var-get total-points-issued) base-points))
        (if is-new-member (var-set total-loyalty-members (+ (var-get total-loyalty-members) u1)) true)
        (ok base-points)
    )
)

(define-public (redeem-points-for-discount (points-to-redeem uint))
    (let (
        (current-account (unwrap! (map-get? loyalty-accounts { user: tx-sender }) (err err-insufficient-points)))
        (available-points (get points-balance current-account))
    )
        (asserts! (>= available-points points-to-redeem) (err err-insufficient-points))
        (asserts! (> points-to-redeem u0) (err err-invalid-amount))

        ;; Update points balance
        (map-set loyalty-accounts
            { user: tx-sender }
            {
                total-points: (get total-points current-account),
                points-balance: (- available-points points-to-redeem),
                tier: (get tier current-account),
                policies-held: (get policies-held current-account),
                total-premiums-paid: (get total-premiums-paid current-account),
                last-activity: stacks-block-height
            }
        )
        (ok points-to-redeem)
    )
)

(define-public (calculate-loyalty-discount (user principal) (base-premium uint))
    (let (
        (account (get-loyalty-account-or-default user))
        (user-tier (get tier account))
        (discount-rate (get-tier-discount user-tier))
        (discount-amount (/ (* base-premium discount-rate) basis-points))
    )
        (ok {
            base-premium: base-premium,
            discounted-premium: (- base-premium discount-amount),
            discount-amount: discount-amount,
            discount-percentage: discount-rate,
            tier: user-tier
        })
    )
)

;; Read-only Functions
(define-read-only (get-loyalty-account (user principal))
    (map-get? loyalty-accounts { user: user })
)

(define-read-only (get-loyalty-stats)
    {
        total-members: (var-get total-loyalty-members),
        total-points-issued: (var-get total-points-issued)
    }
)

(define-read-only (get-tier-requirements)
    {
        bronze-threshold: bronze-threshold,
        silver-threshold: silver-threshold,
        gold-threshold: gold-threshold,
        points-per-stx: points-per-stx
    }
)