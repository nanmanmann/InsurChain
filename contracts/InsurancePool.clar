;; Insurance Pool Management Contract
;; Manages liquidity pools for backing insurance policies
;; Allows investors to stake capital and earn yield from premiums

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-authorized u401)
(define-constant err-insufficient-funds u402)
(define-constant err-pool-not-found u404)
(define-constant err-invalid-amount u400)
(define-constant err-withdrawal-too-early u405)
(define-constant err-pool-inactive u406)
(define-constant minimum-stake u1000000) ;; 1 STX minimum stake
(define-constant withdrawal-cooldown u1440) ;; 1440 blocks (~10 days)
(define-constant yield-calculation-period u144) ;; Daily yield calculation (144 blocks)
(define-constant basis-points u10000)

;; Data maps for pool management
(define-map insurance-pools
    { pool-id: uint }
    {
        pool-name: (string-ascii 50),
        total-staked: uint,
        available-capital: uint,
        premium-income: uint,
        claims-paid: uint,
        investor-count: uint,
        annual-yield-rate: uint, ;; Annual yield in basis points (e.g., 800 = 8%)
        is-active: bool,
        risk-score: uint,
        solvency-ratio: uint, ;; Percentage of capital vs liabilities
        created-at: uint
    }
)

;; Track individual investor positions
(define-map investor-stakes
    { pool-id: uint, investor: principal }
    {
        staked-amount: uint,
        share-tokens: uint,
        last-yield-claim: uint,
        stake-timestamp: uint,
        total-yield-earned: uint,
        withdrawable-amount: uint
    }
)

;; Pool activity tracking
(define-map pool-activities
    { pool-id: uint, activity-id: uint }
    {
        activity-type: (string-ascii 20), ;; "STAKE", "UNSTAKE", "PREMIUM", "CLAIM", "YIELD"
        amount: uint,
        participant: principal,
        timestamp: uint,
        pool-balance-after: uint
    }
)

;; Risk-weighted yield multipliers
(define-map risk-yield-multipliers
    { risk-level: uint }
    { multiplier: uint } ;; Basis points multiplier (e.g., 12000 = 120% yield)
)

;; Data variables
(define-data-var next-pool-id uint u1)
(define-data-var next-activity-id uint u1)
(define-data-var total-pools-created uint u0)
(define-data-var global-liquidity uint u0)

;; Initialize risk-yield multipliers
(map-set risk-yield-multipliers { risk-level: u1 } { multiplier: u8000 }) ;; Low risk: 80% yield
(map-set risk-yield-multipliers { risk-level: u2 } { multiplier: u10000 }) ;; Medium risk: 100% yield
(map-set risk-yield-multipliers { risk-level: u3 } { multiplier: u15000 }) ;; High risk: 150% yield

;; Create new insurance liquidity pool
(define-public (create-liquidity-pool (pool-name (string-ascii 50)) (annual-yield-rate uint) (risk-score uint))
    (let (
        (pool-id (var-get next-pool-id))
    )
        (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
        (asserts! (<= annual-yield-rate u3000) (err err-invalid-amount)) ;; Max 30% annual yield
        (asserts! (<= risk-score u3) (err err-invalid-amount)) ;; Risk score 1-3
        
        (map-set insurance-pools
            { pool-id: pool-id }
            {
                pool-name: pool-name,
                total-staked: u0,
                available-capital: u0,
                premium-income: u0,
                claims-paid: u0,
                investor-count: u0,
                annual-yield-rate: annual-yield-rate,
                is-active: true,
                risk-score: risk-score,
                solvency-ratio: u10000, ;; 100% initially
                created-at: stacks-block-height
            }
        )
        (var-set next-pool-id (+ pool-id u1))
        (var-set total-pools-created (+ (var-get total-pools-created) u1))
        (ok pool-id)
    )
)

;; Stake capital in insurance pool
(define-public (stake-in-pool (pool-id uint) (stake-amount uint))
    (let (
        (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) (err err-pool-not-found)))
        (existing-stake (map-get? investor-stakes { pool-id: pool-id, investor: tx-sender }))
        (activity-id (var-get next-activity-id))
        (total-pool-tokens (get total-staked pool))
        (new-share-tokens stake-amount) ;; Simple 1:1 token ratio for now
    )
        (asserts! (get is-active pool) (err err-pool-inactive))
        (asserts! (>= stake-amount minimum-stake) (err err-invalid-amount))
        
        ;; Transfer STX to contract
        (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
        
        (let (
            (current-staked (match existing-stake stake (get staked-amount stake) u0))
            (current-tokens (match existing-stake stake (get share-tokens stake) u0))
            (total-staked (+ current-staked stake-amount))
            (total-tokens (+ current-tokens new-share-tokens))
            (is-new-investor (is-none existing-stake))
        )
            ;; Update investor position
            (map-set investor-stakes
                { pool-id: pool-id, investor: tx-sender }
                {
                    staked-amount: total-staked,
                    share-tokens: total-tokens,
                    last-yield-claim: stacks-block-height,
                    stake-timestamp: stacks-block-height,
                    total-yield-earned: (match existing-stake stake (get total-yield-earned stake) u0),
                    withdrawable-amount: u0 ;; Locked initially
                }
            )
            
            ;; Update pool stats
            (map-set insurance-pools
                { pool-id: pool-id }
                {
                    pool-name: (get pool-name pool),
                    total-staked: (+ (get total-staked pool) stake-amount),
                    available-capital: (+ (get available-capital pool) stake-amount),
                    premium-income: (get premium-income pool),
                    claims-paid: (get claims-paid pool),
                    investor-count: (if is-new-investor (+ (get investor-count pool) u1) (get investor-count pool)),
                    annual-yield-rate: (get annual-yield-rate pool),
                    is-active: (get is-active pool),
                    risk-score: (get risk-score pool),
                    solvency-ratio: (get solvency-ratio pool),
                    created-at: (get created-at pool)
                }
            )
            
            ;; Record activity
            (map-set pool-activities
                { pool-id: pool-id, activity-id: activity-id }
                {
                    activity-type: "STAKE",
                    amount: stake-amount,
                    participant: tx-sender,
                    timestamp: stacks-block-height,
                    pool-balance-after: (+ (get available-capital pool) stake-amount)
                }
            )
            (var-set next-activity-id (+ activity-id u1))
            (var-set global-liquidity (+ (var-get global-liquidity) stake-amount))
            (ok new-share-tokens)
        )
    )
)

;; Request withdrawal from pool (with cooldown period)
(define-public (request-withdrawal (pool-id uint) (withdrawal-amount uint))
    (let (
        (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) (err err-pool-not-found)))
        (stake (unwrap! (map-get? investor-stakes { pool-id: pool-id, investor: tx-sender }) (err err-not-authorized)))
        (activity-id (var-get next-activity-id))
    )
        (asserts! (>= (get staked-amount stake) withdrawal-amount) (err err-insufficient-funds))
        (asserts! (>= (get available-capital pool) withdrawal-amount) (err err-insufficient-funds))
        (asserts! (>= stacks-block-height (+ (get stake-timestamp stake) withdrawal-cooldown)) (err err-withdrawal-too-early))
        
        ;; Update withdrawable amount (cooldown mechanism)
        (map-set investor-stakes
            { pool-id: pool-id, investor: tx-sender }
            {
                staked-amount: (get staked-amount stake),
                share-tokens: (get share-tokens stake),
                last-yield-claim: (get last-yield-claim stake),
                stake-timestamp: (get stake-timestamp stake),
                total-yield-earned: (get total-yield-earned stake),
                withdrawable-amount: withdrawal-amount
            }
        )
        
        ;; Record withdrawal request
        (map-set pool-activities
            { pool-id: pool-id, activity-id: activity-id }
            {
                activity-type: "UNSTAKE",
                amount: withdrawal-amount,
                participant: tx-sender,
                timestamp: stacks-block-height,
                pool-balance-after: (- (get available-capital pool) withdrawal-amount)
            }
        )
        (var-set next-activity-id (+ activity-id u1))
        (ok withdrawal-amount)
    )
)

;; Execute withdrawal after cooldown
(define-public (execute-withdrawal (pool-id uint))
    (let (
        (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) (err err-pool-not-found)))
        (stake (unwrap! (map-get? investor-stakes { pool-id: pool-id, investor: tx-sender }) (err err-not-authorized)))
        (withdrawal-amount (get withdrawable-amount stake))
    )
        (asserts! (> withdrawal-amount u0) (err err-invalid-amount))
        (asserts! (>= (get available-capital pool) withdrawal-amount) (err err-insufficient-funds))
        
        ;; Update investor position
        (map-set investor-stakes
            { pool-id: pool-id, investor: tx-sender }
            {
                staked-amount: (- (get staked-amount stake) withdrawal-amount),
                share-tokens: (- (get share-tokens stake) withdrawal-amount),
                last-yield-claim: (get last-yield-claim stake),
                stake-timestamp: (get stake-timestamp stake),
                total-yield-earned: (get total-yield-earned stake),
                withdrawable-amount: u0
            }
        )
        
        ;; Update pool
        (map-set insurance-pools
            { pool-id: pool-id }
            {
                pool-name: (get pool-name pool),
                total-staked: (- (get total-staked pool) withdrawal-amount),
                available-capital: (- (get available-capital pool) withdrawal-amount),
                premium-income: (get premium-income pool),
                claims-paid: (get claims-paid pool),
                investor-count: (if (is-eq (- (get staked-amount stake) withdrawal-amount) u0) 
                                   (- (get investor-count pool) u1) 
                                   (get investor-count pool)),
                annual-yield-rate: (get annual-yield-rate pool),
                is-active: (get is-active pool),
                risk-score: (get risk-score pool),
                solvency-ratio: (get solvency-ratio pool),
                created-at: (get created-at pool)
            }
        )
        
        (var-set global-liquidity (- (var-get global-liquidity) withdrawal-amount))
        
        ;; Transfer STX back to investor
        (try! (as-contract (stx-transfer? withdrawal-amount tx-sender tx-sender)))
        (ok withdrawal-amount)
    )
)

;; Add premium income to pool
(define-public (deposit-premium (pool-id uint) (premium-amount uint))
    (let (
        (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) (err err-pool-not-found)))
        (activity-id (var-get next-activity-id))
        (new-solvency-ratio (/ (* (+ (get available-capital pool) premium-amount) basis-points) 
                             (+ (get total-staked pool) premium-amount)))
    )
        (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
        
        (map-set insurance-pools
            { pool-id: pool-id }
            {
                pool-name: (get pool-name pool),
                total-staked: (get total-staked pool),
                available-capital: (+ (get available-capital pool) premium-amount),
                premium-income: (+ (get premium-income pool) premium-amount),
                claims-paid: (get claims-paid pool),
                investor-count: (get investor-count pool),
                annual-yield-rate: (get annual-yield-rate pool),
                is-active: (get is-active pool),
                risk-score: (get risk-score pool),
                solvency-ratio: new-solvency-ratio,
                created-at: (get created-at pool)
            }
        )
        
        ;; Record premium deposit
        (map-set pool-activities
            { pool-id: pool-id, activity-id: activity-id }
            {
                activity-type: "PREMIUM",
                amount: premium-amount,
                participant: tx-sender,
                timestamp: stacks-block-height,
                pool-balance-after: (+ (get available-capital pool) premium-amount)
            }
        )
        (var-set next-activity-id (+ activity-id u1))
        (ok true)
    )
)

;; Process claim payout from pool
(define-public (process-claim-from-pool (pool-id uint) (claim-amount uint) (policy-owner principal))
    (let (
        (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) (err err-pool-not-found)))
        (activity-id (var-get next-activity-id))
        (new-solvency-ratio (/ (* (- (get available-capital pool) claim-amount) basis-points) 
                             (get total-staked pool)))
    )
        (asserts! (is-eq tx-sender contract-owner) (err err-not-authorized))
        (asserts! (>= (get available-capital pool) claim-amount) (err err-insufficient-funds))
        
        (map-set insurance-pools
            { pool-id: pool-id }
            {
                pool-name: (get pool-name pool),
                total-staked: (get total-staked pool),
                available-capital: (- (get available-capital pool) claim-amount),
                premium-income: (get premium-income pool),
                claims-paid: (+ (get claims-paid pool) claim-amount),
                investor-count: (get investor-count pool),
                annual-yield-rate: (get annual-yield-rate pool),
                is-active: (get is-active pool),
                risk-score: (get risk-score pool),
                solvency-ratio: new-solvency-ratio,
                created-at: (get created-at pool)
            }
        )
        
        ;; Record claim payout
        (map-set pool-activities
            { pool-id: pool-id, activity-id: activity-id }
            {
                activity-type: "CLAIM",
                amount: claim-amount,
                participant: policy-owner,
                timestamp: stacks-block-height,
                pool-balance-after: (- (get available-capital pool) claim-amount)
            }
        )
        (var-set next-activity-id (+ activity-id u1))
        
        ;; Transfer claim payout
        (try! (as-contract (stx-transfer? claim-amount tx-sender policy-owner)))
        (ok true)
    )
)

;; Claim yield earnings with risk-adjusted rates
(define-public (claim-yield-earnings (pool-id uint))
    (let (
        (pool (unwrap! (map-get? insurance-pools { pool-id: pool-id }) (err err-pool-not-found)))
        (stake (unwrap! (map-get? investor-stakes { pool-id: pool-id, investor: tx-sender }) (err err-not-authorized)))
        (blocks-since-last-claim (- stacks-block-height (get last-yield-claim stake)))
        (risk-multiplier (get multiplier (unwrap! (map-get? risk-yield-multipliers { risk-level: (get risk-score pool) }) (err err-pool-not-found))))
        (base-yield (/ (* (get share-tokens stake) (get annual-yield-rate pool) blocks-since-last-claim) 
                       (* basis-points (* u365 yield-calculation-period))))
        (risk-adjusted-yield (/ (* base-yield risk-multiplier) basis-points))
        (activity-id (var-get next-activity-id))
    )
        (asserts! (> risk-adjusted-yield u0) (err err-invalid-amount))
        (asserts! (>= (get available-capital pool) risk-adjusted-yield) (err err-insufficient-funds))
        
        ;; Update investor position
        (map-set investor-stakes
            { pool-id: pool-id, investor: tx-sender }
            {
                staked-amount: (get staked-amount stake),
                share-tokens: (get share-tokens stake),
                last-yield-claim: stacks-block-height,
                stake-timestamp: (get stake-timestamp stake),
                total-yield-earned: (+ (get total-yield-earned stake) risk-adjusted-yield),
                withdrawable-amount: (get withdrawable-amount stake)
            }
        )
        
        ;; Update pool available capital
        (map-set insurance-pools
            { pool-id: pool-id }
            {
                pool-name: (get pool-name pool),
                total-staked: (get total-staked pool),
                available-capital: (- (get available-capital pool) risk-adjusted-yield),
                premium-income: (get premium-income pool),
                claims-paid: (get claims-paid pool),
                investor-count: (get investor-count pool),
                annual-yield-rate: (get annual-yield-rate pool),
                is-active: (get is-active pool),
                risk-score: (get risk-score pool),
                solvency-ratio: (get solvency-ratio pool),
                created-at: (get created-at pool)
            }
        )
        
        ;; Record yield claim
        (map-set pool-activities
            { pool-id: pool-id, activity-id: activity-id }
            {
                activity-type: "YIELD",
                amount: risk-adjusted-yield,
                participant: tx-sender,
                timestamp: stacks-block-height,
                pool-balance-after: (- (get available-capital pool) risk-adjusted-yield)
            }
        )
        (var-set next-activity-id (+ activity-id u1))
        
        ;; Transfer yield to investor
        (try! (as-contract (stx-transfer? risk-adjusted-yield tx-sender tx-sender)))
        (ok risk-adjusted-yield)
    )
)

;; Read-only functions
(define-read-only (get-pool-details (pool-id uint))
    (map-get? insurance-pools { pool-id: pool-id })
)

(define-read-only (get-investor-position (pool-id uint) (investor principal))
    (map-get? investor-stakes { pool-id: pool-id, investor: investor })
)

(define-read-only (get-pool-activity (pool-id uint) (activity-id uint))
    (map-get? pool-activities { pool-id: pool-id, activity-id: activity-id })
)

(define-read-only (calculate-potential-yield (pool-id uint) (investor principal))
    (match (map-get? investor-stakes { pool-id: pool-id, investor: investor })
        stake 
        (match (map-get? insurance-pools { pool-id: pool-id })
            pool
            (match (map-get? risk-yield-multipliers { risk-level: (get risk-score pool) })
                risk-data
                (let (
                    (blocks-since-last-claim (- stacks-block-height (get last-yield-claim stake)))
                    (risk-multiplier (get multiplier risk-data))
                    (base-yield (/ (* (get share-tokens stake) (get annual-yield-rate pool) blocks-since-last-claim) 
                                 (* basis-points (* u365 yield-calculation-period))))
                    (risk-adjusted-yield (/ (* base-yield risk-multiplier) basis-points))
                )
                    (some risk-adjusted-yield)
                )
                none
            )
            none
        )
        none
    )
)

(define-read-only (get-pool-performance-metrics (pool-id uint))
    (match (map-get? insurance-pools { pool-id: pool-id })
        pool
        (some {
            total-staked: (get total-staked pool),
            available-capital: (get available-capital pool),
            premium-income: (get premium-income pool),
            claims-paid: (get claims-paid pool),
            net-profit: (- (get premium-income pool) (get claims-paid pool)),
            solvency-ratio: (get solvency-ratio pool),
            investor-count: (get investor-count pool),
            risk-score: (get risk-score pool),
            utilization-rate: (if (> (get total-staked pool) u0)
                                 (/ (* (- (get total-staked pool) (get available-capital pool)) basis-points) 
                                    (get total-staked pool))
                                 u0)
        })
        none
    )
)

(define-read-only (get-global-pool-stats)
    {
        total-pools: (var-get total-pools-created),
        global-liquidity: (var-get global-liquidity),
        next-pool-id: (var-get next-pool-id)
    }
)


