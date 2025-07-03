(define-constant contract-owner tx-sender)
(define-constant approval-threshold u2)
(define-constant max-approvers u5)
(define-constant claim-approved "APPROVED")
(define-constant claim-rejected "REJECTED")
(define-constant claim-pending "PENDING")
(define-constant large-claim-threshold u500000)

(define-map claim-approvals
    { claim-id: uint }
    {
        required-approvals: uint,
        current-approvals: uint,
        status: (string-ascii 20),
        approvers: (list 5 principal),
        rejectors: (list 5 principal),
        approval-deadline: uint,
        payout-amount: uint,
        policy-owner: principal
    }
)

(define-map approver-registry
    { approver: principal }
    {
        is-active: bool,
        approval-count: uint,
        last-activity: uint
    }
)

(define-map approver-votes
    { claim-id: uint, approver: principal }
    {
        vote: (string-ascii 10),
        timestamp: uint,
        notes: (string-ascii 100)
    }
)

(define-data-var total-approvers uint u0)

(define-public (register-approver (approver principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (asserts! (< (var-get total-approvers) max-approvers) (err u400))
        (map-set approver-registry
            { approver: approver }
            {
                is-active: true,
                approval-count: u0,
                last-activity: stacks-block-height
            }
        )
        (var-set total-approvers (+ (var-get total-approvers) u1))
        (ok true)
    )
)

(define-public (deactivate-approver (approver principal))
    (begin
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (asserts! (is-some (map-get? approver-registry { approver: approver })) (err u404))
        (map-set approver-registry
            { approver: approver }
            {
                is-active: false,
                approval-count: (get approval-count (unwrap! (map-get? approver-registry { approver: approver }) (err u404))),
                last-activity: stacks-block-height
            }
        )
        (ok true)
    )
)

(define-public (initiate-claim-approval (claim-id uint) (payout-amount uint) (policy-owner principal))
    (let (
        (required-approvals (if (>= payout-amount large-claim-threshold) approval-threshold u1))
        (approval-deadline (+ stacks-block-height u1440))
    )
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (asserts! (is-none (map-get? claim-approvals { claim-id: claim-id })) (err u409))
        (map-set claim-approvals
            { claim-id: claim-id }
            {
                required-approvals: required-approvals,
                current-approvals: u0,
                status: claim-pending,
                approvers: (list),
                rejectors: (list),
                approval-deadline: approval-deadline,
                payout-amount: payout-amount,
                policy-owner: policy-owner
            }
        )
        (ok true)
    )
)

(define-public (approve-claim (claim-id uint) (notes (string-ascii 100)))
    (let (
        (claim-approval (unwrap! (map-get? claim-approvals { claim-id: claim-id }) (err u404)))
        (approver-info (unwrap! (map-get? approver-registry { approver: tx-sender }) (err u403)))
        (current-approvals (get current-approvals claim-approval))
        (required-approvals (get required-approvals claim-approval))
        (current-approvers (get approvers claim-approval))
        (new-approvals (+ current-approvals u1))
        (updated-approvers (unwrap! (as-max-len? (append current-approvers tx-sender) u5) (err u400)))
    )
        (asserts! (get is-active approver-info) (err u403))
        (asserts! (is-eq (get status claim-approval) claim-pending) (err u400))
        (asserts! (< stacks-block-height (get approval-deadline claim-approval)) (err u408))
        (asserts! (is-none (map-get? approver-votes { claim-id: claim-id, approver: tx-sender })) (err u409))
        
        (map-set approver-votes
            { claim-id: claim-id, approver: tx-sender }
            {
                vote: "APPROVE",
                timestamp: stacks-block-height,
                notes: notes
            }
        )
        
        (map-set approver-registry
            { approver: tx-sender }
            {
                is-active: true,
                approval-count: (+ (get approval-count approver-info) u1),
                last-activity: stacks-block-height
            }
        )
        
        (let (
            (final-status (if (>= new-approvals required-approvals) claim-approved claim-pending))
        )
            (map-set claim-approvals
                { claim-id: claim-id }
                {
                    required-approvals: required-approvals,
                    current-approvals: new-approvals,
                    status: final-status,
                    approvers: updated-approvers,
                    rejectors: (get rejectors claim-approval),
                    approval-deadline: (get approval-deadline claim-approval),
                    payout-amount: (get payout-amount claim-approval),
                    policy-owner: (get policy-owner claim-approval)
                }
            )
            (ok final-status)
        )
    )
)

(define-public (reject-claim (claim-id uint) (notes (string-ascii 100)))
    (let (
        (claim-approval (unwrap! (map-get? claim-approvals { claim-id: claim-id }) (err u404)))
        (approver-info (unwrap! (map-get? approver-registry { approver: tx-sender }) (err u403)))
        (current-rejectors (get rejectors claim-approval))
        (updated-rejectors (unwrap! (as-max-len? (append current-rejectors tx-sender) u5) (err u400)))
    )
        (asserts! (get is-active approver-info) (err u403))
        (asserts! (is-eq (get status claim-approval) claim-pending) (err u400))
        (asserts! (< stacks-block-height (get approval-deadline claim-approval)) (err u408))
        (asserts! (is-none (map-get? approver-votes { claim-id: claim-id, approver: tx-sender })) (err u409))
        
        (map-set approver-votes
            { claim-id: claim-id, approver: tx-sender }
            {
                vote: "REJECT",
                timestamp: stacks-block-height,
                notes: notes
            }
        )
        
        (map-set claim-approvals
            { claim-id: claim-id }
            {
                required-approvals: (get required-approvals claim-approval),
                current-approvals: (get current-approvals claim-approval),
                status: claim-rejected,
                approvers: (get approvers claim-approval),
                rejectors: updated-rejectors,
                approval-deadline: (get approval-deadline claim-approval),
                payout-amount: (get payout-amount claim-approval),
                policy-owner: (get policy-owner claim-approval)
            }
        )
        (ok claim-rejected)
    )
)

(define-public (process-approved-claim (claim-id uint))
    (let (
        (claim-approval (unwrap! (map-get? claim-approvals { claim-id: claim-id }) (err u404)))
        (payout-amount (get payout-amount claim-approval))
        (policy-owner (get policy-owner claim-approval))
    )
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (asserts! (is-eq (get status claim-approval) claim-approved) (err u400))
        (try! (as-contract (stx-transfer? payout-amount contract-owner policy-owner)))
        (ok payout-amount)
    )
)

(define-public (extend-approval-deadline (claim-id uint) (additional-blocks uint))
    (let (
        (claim-approval (unwrap! (map-get? claim-approvals { claim-id: claim-id }) (err u404)))
        (new-deadline (+ (get approval-deadline claim-approval) additional-blocks))
    )
        (asserts! (is-eq tx-sender contract-owner) (err u403))
        (asserts! (is-eq (get status claim-approval) claim-pending) (err u400))
        (map-set claim-approvals
            { claim-id: claim-id }
            {
                required-approvals: (get required-approvals claim-approval),
                current-approvals: (get current-approvals claim-approval),
                status: (get status claim-approval),
                approvers: (get approvers claim-approval),
                rejectors: (get rejectors claim-approval),
                approval-deadline: new-deadline,
                payout-amount: (get payout-amount claim-approval),
                policy-owner: (get policy-owner claim-approval)
            }
        )
        (ok new-deadline)
    )
)

(define-read-only (get-claim-approval-status (claim-id uint))
    (map-get? claim-approvals { claim-id: claim-id })
)

(define-read-only (get-approver-info (approver principal))
    (map-get? approver-registry { approver: approver })
)

(define-read-only (get-approver-vote (claim-id uint) (approver principal))
    (map-get? approver-votes { claim-id: claim-id, approver: approver })
)

(define-read-only (is-claim-approved (claim-id uint))
    (match (map-get? claim-approvals { claim-id: claim-id })
        approval (is-eq (get status approval) claim-approved)
        false
    )
)

(define-read-only (is-claim-rejected (claim-id uint))
    (match (map-get? claim-approvals { claim-id: claim-id })
        approval (is-eq (get status approval) claim-rejected)
        false
    )
)

(define-read-only (get-pending-claims-count)
    (var-get total-approvers)
)

(define-read-only (is-approval-deadline-expired (claim-id uint))
    (match (map-get? claim-approvals { claim-id: claim-id })
        approval (> stacks-block-height (get approval-deadline approval))
        false
    )
)
