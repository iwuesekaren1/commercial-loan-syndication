;; Commercial Loan Syndication Platform
;; Automates loan syndication process with multi-lender participation and payment distribution

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-unauthorized (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-input (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-loan-not-active (err u104))
(define-constant err-already-participated (err u105))
(define-constant err-syndication-closed (err u106))

;; Data variables
(define-data-var loan-counter uint u0)
(define-data-var participation-counter uint u0)
(define-data-var payment-counter uint u0)

;; Loan status constants
(define-constant status-originating "ORIGINATING")
(define-constant status-syndication "SYNDICATION")
(define-constant status-active "ACTIVE")
(define-constant status-defaulted "DEFAULTED")
(define-constant status-paid-off "PAID_OFF")

;; Borrower registry
(define-map borrowers
  { borrower: principal }
  {
    credit-score: uint,
    total-debt: uint,
    verified: bool,
    kyc-status: bool,
    registration-date: uint
  }
)

;; Lender registry
(define-map lenders
  { lender: principal }
  {
    available-capital: uint,
    total-commitments: uint,
    risk-appetite: uint,
    verified: bool,
    registration-date: uint,
    total-participations: uint
  }
)

;; Loan registry
(define-map loans
  { loan-id: uint }
  {
    borrower: principal,
    lead-arranger: principal,
    loan-amount: uint,
    interest-rate: uint,
    term-months: uint,
    monthly-payment: uint,
    syndication-target: uint,
    syndicated-amount: uint,
    status: (string-ascii 20),
    origination-date: uint,
    maturity-date: uint,
    payments-made: uint,
    principal-remaining: uint
  }
)

;; Loan participations
(define-map participations
  { participation-id: uint }
  {
    loan-id: uint,
    lender: principal,
    participation-amount: uint,
    participation-percentage: uint,
    committed-at: uint,
    interest-earned: uint,
    principal-repaid: uint
  }
)

;; Payment records
(define-map payments
  { payment-id: uint }
  {
    loan-id: uint,
    payment-amount: uint,
    principal-portion: uint,
    interest-portion: uint,
    payment-date: uint,
    paid-by: principal
  }
)

;; Loan terms and conditions
(define-map loan-terms
  { loan-id: uint }
  {
    collateral-description: (string-ascii 200),
    collateral-value: uint,
    loan-purpose: (string-ascii 100),
    financial-covenants: (list 5 (string-ascii 100)),
    syndication-deadline: uint,
    minimum-participation: uint
  }
)

;; Risk assessment data
(define-map risk-assessments
  { loan-id: uint }
  {
    debt-to-income: uint,
    collateral-coverage: uint,
    credit-rating: (string-ascii 10),
    risk-score: uint,
    assessed-by: principal,
    assessment-date: uint
  }
)

;; Authorization functions
(define-private (is-contract-owner)
  (is-eq tx-sender contract-owner)
)

(define-private (is-verified-borrower (borrower principal))
  (match (map-get? borrowers { borrower: borrower })
    b (and (get verified b) (get kyc-status b))
    false
  )
)

(define-private (is-verified-lender (lender principal))
  (match (map-get? lenders { lender: lender })
    l (get verified l)
    false
  )
)

(define-private (is-lead-arranger (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan (is-eq tx-sender (get lead-arranger loan))
    false
  )
)

;; Borrower functions
(define-public (register-borrower
  (credit-score uint)
  (kyc-verified bool)
)
  (begin
    (asserts! (and (>= credit-score u300) (<= credit-score u850)) err-invalid-input)
    
    (map-set borrowers
      { borrower: tx-sender }
      {
        credit-score: credit-score,
        total-debt: u0,
        verified: true,
        kyc-status: kyc-verified,
        registration-date: u1
      }
    )
    (ok true)
  )
)

;; Lender functions
(define-public (register-lender
  (available-capital uint)
  (risk-appetite uint)
)
  (begin
    (asserts! (> available-capital u0) err-invalid-input)
    (asserts! (<= risk-appetite u10) err-invalid-input)
    
    (map-set lenders
      { lender: tx-sender }
      {
        available-capital: available-capital,
        total-commitments: u0,
        risk-appetite: risk-appetite,
        verified: true,
        registration-date: u1,
        total-participations: u0
      }
    )
    (ok true)
  )
)

(define-public (update-available-capital (new-capital uint))
  (begin
    (asserts! (is-verified-lender tx-sender) err-unauthorized)
    
    (match (map-get? lenders { lender: tx-sender })
      lender
      (begin
        (map-set lenders
          { lender: tx-sender }
          (merge lender { available-capital: new-capital })
        )
        (ok true)
      )
      err-not-found
    )
  )
)

;; Loan origination
(define-public (originate-loan
  (borrower principal)
  (loan-amount uint)
  (interest-rate uint)
  (term-months uint)
  (syndication-target uint)
  (collateral-value uint)
  (loan-purpose (string-ascii 100))
)
  (begin
    (asserts! (is-verified-lender tx-sender) err-unauthorized)
    (asserts! (is-verified-borrower borrower) err-unauthorized)
    (asserts! (> loan-amount u0) err-invalid-input)
    (asserts! (> syndication-target u0) err-invalid-input)
    (asserts! (<= syndication-target loan-amount) err-invalid-input)
    
    (let 
      (
        (loan-id (+ (var-get loan-counter) u1))
        (monthly-payment (calculate-monthly-payment loan-amount interest-rate term-months))
        (maturity-date (+ u1 (* term-months u30))) ;; Simplified: 30 days per month
      )
      
      (map-set loans
        { loan-id: loan-id }
        {
          borrower: borrower,
          lead-arranger: tx-sender,
          loan-amount: loan-amount,
          interest-rate: interest-rate,
          term-months: term-months,
          monthly-payment: monthly-payment,
          syndication-target: syndication-target,
          syndicated-amount: u0,
          status: status-originating,
          origination-date: u1,
          maturity-date: maturity-date,
          payments-made: u0,
          principal-remaining: loan-amount
        }
      )
      
      (map-set loan-terms
        { loan-id: loan-id }
        {
          collateral-description: "Commercial Real Estate",
          collateral-value: collateral-value,
          loan-purpose: loan-purpose,
          financial-covenants: (list "Debt Service Coverage > 1.25" "LTV < 80%"),
          syndication-deadline: (+ u1 u30), ;; 30 days from origination
          minimum-participation: (/ syndication-target u10) ;; Minimum 10% of target
        }
      )
      
      (var-set loan-counter loan-id)
      (ok loan-id)
    )
  )
)

;; Loan syndication
(define-public (participate-in-loan
  (loan-id uint)
  (participation-amount uint)
)
  (begin
    (asserts! (is-verified-lender tx-sender) err-unauthorized)
    (asserts! (> participation-amount u0) err-invalid-input)
    
    (match (map-get? loans { loan-id: loan-id })
      loan
      (begin
        (asserts! (is-eq (get status loan) status-originating) err-loan-not-active)
        (asserts! (<= (+ (get syndicated-amount loan) participation-amount) (get syndication-target loan)) err-invalid-input)
        
        (match (map-get? lenders { lender: tx-sender })
          lender
          (begin
            (asserts! (>= (get available-capital lender) participation-amount) err-insufficient-funds)
            
            (let 
              (
                (participation-id (+ (var-get participation-counter) u1))
                (participation-percentage (/ (* participation-amount u10000) (get syndication-target loan)))
                (new-syndicated-amount (+ (get syndicated-amount loan) participation-amount))
              )
              
              ;; Record participation
              (map-set participations
                { participation-id: participation-id }
                {
                  loan-id: loan-id,
                  lender: tx-sender,
                  participation-amount: participation-amount,
                  participation-percentage: participation-percentage,
                  committed-at: u1,
                  interest-earned: u0,
                  principal-repaid: u0
                }
              )
              
              ;; Update loan syndicated amount
              (map-set loans
                { loan-id: loan-id }
                (merge loan { 
                  syndicated-amount: new-syndicated-amount,
                  status: (if (is-eq new-syndicated-amount (get syndication-target loan)) status-active status-originating)
                })
              )
              
              ;; Update lender capital and commitments
              (map-set lenders
                { lender: tx-sender }
                (merge lender {
                  available-capital: (- (get available-capital lender) participation-amount),
                  total-commitments: (+ (get total-commitments lender) participation-amount),
                  total-participations: (+ (get total-participations lender) u1)
                })
              )
              
              (var-set participation-counter participation-id)
              (ok participation-id)
            )
          )
          err-not-found
        )
      )
      err-not-found
    )
  )
)

;; Payment processing
(define-public (make-payment
  (loan-id uint)
  (payment-amount uint)
)
  (begin
    (match (map-get? loans { loan-id: loan-id })
      loan
      (begin
        (asserts! (is-eq tx-sender (get borrower loan)) err-unauthorized)
        (asserts! (is-eq (get status loan) status-active) err-loan-not-active)
        (asserts! (> payment-amount u0) err-invalid-input)
        
        (let 
          (
            (payment-id (+ (var-get payment-counter) u1))
            (interest-portion (calculate-interest-portion (get principal-remaining loan) (get interest-rate loan)))
            (principal-portion (- payment-amount interest-portion))
            (new-principal-remaining (- (get principal-remaining loan) principal-portion))
          )
          
          ;; Record payment
          (map-set payments
            { payment-id: payment-id }
            {
              loan-id: loan-id,
              payment-amount: payment-amount,
              principal-portion: principal-portion,
              interest-portion: interest-portion,
              payment-date: u1,
              paid-by: tx-sender
            }
          )
          
          ;; Update loan
          (map-set loans
            { loan-id: loan-id }
            (merge loan {
              payments-made: (+ (get payments-made loan) u1),
              principal-remaining: new-principal-remaining,
              status: (if (is-eq new-principal-remaining u0) status-paid-off status-active)
            })
          )
          
          (var-set payment-counter payment-id)
          (ok payment-id)
        )
      )
      err-not-found
    )
  )
)

;; Helper functions
(define-private (calculate-monthly-payment (principal uint) (annual-rate uint) (term-months uint))
  ;; Simplified calculation - in production would use proper amortization formula
  (/ (+ principal (* principal annual-rate term-months (/ u1 u1200))) term-months)
)

(define-private (calculate-interest-portion (principal uint) (annual-rate uint))
  ;; Monthly interest calculation
  (/ (* principal annual-rate) u1200)
)

;; Read-only functions
(define-read-only (get-borrower (borrower principal))
  (map-get? borrowers { borrower: borrower })
)

(define-read-only (get-lender (lender principal))
  (map-get? lenders { lender: lender })
)

(define-read-only (get-loan (loan-id uint))
  (map-get? loans { loan-id: loan-id })
)

(define-read-only (get-participation (participation-id uint))
  (map-get? participations { participation-id: participation-id })
)

(define-read-only (get-payment (payment-id uint))
  (map-get? payments { payment-id: payment-id })
)

(define-read-only (get-loan-terms (loan-id uint))
  (map-get? loan-terms { loan-id: loan-id })
)

(define-read-only (get-risk-assessment (loan-id uint))
  (map-get? risk-assessments { loan-id: loan-id })
)

(define-read-only (get-loan-counter)
  (var-get loan-counter)
)

(define-read-only (get-participation-counter)
  (var-get participation-counter)
)

(define-read-only (calculate-loan-ltv (loan-id uint))
  (match (map-get? loans { loan-id: loan-id })
    loan
    (match (map-get? loan-terms { loan-id: loan-id })
      terms
      (some (/ (* (get loan-amount loan) u10000) (get collateral-value terms)))
      none
    )
    none
  )
)
