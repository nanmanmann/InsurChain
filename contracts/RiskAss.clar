(define-constant contract-owner tx-sender)
(define-constant max-risk-score u1000)
(define-constant base-risk-score u500)
(define-constant low-risk-threshold u300)
(define-constant high-risk-threshold u700)
(define-constant discount-rate u20)
(define-constant penalty-rate u50)
(define-constant hundred u100)

(define-map user-risk-profiles
    { user: principal }
    {
        risk-score: uint,
        claim-frequency: uint,
        total-claims: uint,
        last-updated: uint,
        policy-count: uint
    }
)

(define-map risk-factors
    { factor-type: (string-ascii 20) }
    { weight: uint, max-impact: uint }
)

(define-data-var risk-assessment-enabled bool true)

(define-private (initialize-risk-factors)
    (begin
        (map-set risk-factors { factor-type: "claim-frequency" } { weight: u30, max-impact: u200 })
        (map-set risk-factors { factor-type: "claim-amount" } { weight: u25, max-impact: u150 })
        (map-set risk-factors { factor-type: "policy-duration" } { weight: u15, max-impact: u100 })
        (map-set risk-factors { factor-type: "payment-history" } { weight: u20, max-impact: u120 })
        (map-set risk-factors { factor-type: "policy-lapses" } { weight: u10, max-impact: u80 })
        true
    )
)

(define-private (get-user-risk-profile (user principal))
    (default-to
        {
            risk-score: base-risk-score,
            claim-frequency: u0,
            total-claims: u0,
            last-updated: u0,
            policy-count: u0
        }
        (map-get? user-risk-profiles { user: user })
    )
)

(define-private (calculate-claim-frequency-impact (claim-count uint) (policy-count uint))
    (if (is-eq policy-count u0)
        u0
        (let ((frequency-ratio (/ (* claim-count hundred) policy-count)))
            (if (> frequency-ratio u50)
                u200
                (if (> frequency-ratio u25)
                    u100
                    u0
                )
            )
        )
    )
)

(define-private (calculate-total-claims-impact (total-claims uint))
    (if (> total-claims u10)
        u150
        (if (> total-claims u5)
            u75
            u0
        )
    )
)

(define-private (update-risk-score (user principal) (claim-impact uint) (policy-impact uint))
    (let (
        (current-profile (get-user-risk-profile user))
        (frequency-impact (calculate-claim-frequency-impact 
            (get claim-frequency current-profile) 
            (get policy-count current-profile)
        ))
        (claims-impact (calculate-total-claims-impact (get total-claims current-profile)))
        (new-risk-score (+ base-risk-score frequency-impact claims-impact claim-impact policy-impact))
        (capped-risk-score (if (> new-risk-score max-risk-score) max-risk-score new-risk-score))
    )
        (map-set user-risk-profiles
            { user: user }
            {
                risk-score: capped-risk-score,
                claim-frequency: (get claim-frequency current-profile),
                total-claims: (get total-claims current-profile),
                last-updated: stacks-block-height,
                policy-count: (get policy-count current-profile)
            }
        )
        capped-risk-score
    )
)

(define-public (record-new-policy (user principal))
    (let (
        (current-profile (get-user-risk-profile user))
        (new-policy-count (+ (get policy-count current-profile) u1))
    )
        (map-set user-risk-profiles
            { user: user }
            {
                risk-score: (get risk-score current-profile),
                claim-frequency: (get claim-frequency current-profile),
                total-claims: (get total-claims current-profile),
                last-updated: stacks-block-height,
                policy-count: new-policy-count
            }
        )
        (ok (update-risk-score user u0 u0))
    )
)

(define-public (record-new-claim (user principal) (claim-amount uint))
    (let (
        (current-profile (get-user-risk-profile user))
        (new-claim-frequency (+ (get claim-frequency current-profile) u1))
        (new-total-claims (+ (get total-claims current-profile) u1))
        (claim-impact (if (> claim-amount u1000000) u100 u50))
    )
        (map-set user-risk-profiles
            { user: user }
            {
                risk-score: (get risk-score current-profile),
                claim-frequency: new-claim-frequency,
                total-claims: new-total-claims,
                last-updated: stacks-block-height,
                policy-count: (get policy-count current-profile)
            }
        )
        (ok (update-risk-score user claim-impact u0))
    )
)

(define-public (calculate-dynamic-premium (base-premium uint) (user principal))
    (let (
        (risk-profile (get-user-risk-profile user))
        (risk-score (get risk-score risk-profile))
    )
        (if (< risk-score low-risk-threshold)
            (let ((discount (/ (* base-premium discount-rate) hundred)))
                (ok (- base-premium discount))
            )
            (if (> risk-score high-risk-threshold)
                (let ((penalty (/ (* base-premium penalty-rate) hundred)))
                    (ok (+ base-premium penalty))
                )
                (ok base-premium)
            )
        )
    )
)

(define-public (get-risk-assessment (user principal))
    (let (
        (risk-profile (get-user-risk-profile user))
        (risk-score (get risk-score risk-profile))
    )
        (ok {
            risk-score: risk-score,
            risk-level: (if (< risk-score low-risk-threshold)
                "LOW"
                (if (> risk-score high-risk-threshold)
                    "HIGH"
                    "MEDIUM"
                )
            ),
            claim-frequency: (get claim-frequency risk-profile),
            total-claims: (get total-claims risk-profile),
            policy-count: (get policy-count risk-profile),
            last-updated: (get last-updated risk-profile)
        })
    )
)

(define-public (get-premium-quote (base-premium uint) (user principal))
    (let (
        (dynamic-premium (unwrap! (calculate-dynamic-premium base-premium user) (err u1)))
        (risk-assessment (unwrap! (get-risk-assessment user) (err u2)))
    )
        (ok {
            base-premium: base-premium,
            adjusted-premium: dynamic-premium,
            risk-level: (get risk-level risk-assessment),
            discount-or-penalty: (if (> dynamic-premium base-premium)
                (- dynamic-premium base-premium)
                (- base-premium dynamic-premium)
            )
        })
    )
)

(define-read-only (get-user-risk-score (user principal))
    (get risk-score (get-user-risk-profile user))
)

(define-read-only (is-low-risk-user (user principal))
    (< (get-user-risk-score user) low-risk-threshold)
)

(define-read-only (is-high-risk-user (user principal))
    (> (get-user-risk-score user) high-risk-threshold)
)

(define-public (reset-user-risk-profile (user principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (map-delete user-risk-profiles { user: user })
        (ok true)
    )
)

(define-public (adjust-risk-thresholds (new-low-threshold uint) (new-high-threshold uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (asserts! (< new-low-threshold new-high-threshold) (err u400))
        (asserts! (< new-high-threshold max-risk-score) (err u400))
        (ok true)
    )
)

(initialize-risk-factors)