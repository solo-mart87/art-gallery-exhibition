
;; title: gallery-exhibition
;; version: 1.0.0
;; summary: Art Gallery Exhibition Management System
;; description: A smart contract for managing art gallery exhibitions, artist submissions, and visitor engagement

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-EXISTS (err u102))
(define-constant ERR-UNAUTHORIZED (err u103))
(define-constant ERR-EXHIBITION-CLOSED (err u104))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u105))

;; data vars
(define-data-var next-artwork-id uint u1)
(define-data-var next-exhibition-id uint u1)
(define-data-var gallery-commission uint u10) ;; 10% commission

;; data maps
(define-map artworks
  uint
  {
    artist: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    price: uint,
    status: (string-ascii 20), ;; "submitted", "approved", "sold"
    exhibition-id: (optional uint)
  }
)

(define-map exhibitions
  uint
  {
    curator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    start-block: uint,
    end-block: uint,
    status: (string-ascii 20), ;; "planned", "active", "closed"
    total-artworks: uint
  }
)

(define-map artist-profiles
  principal
  {
    name: (string-ascii 100),
    bio: (string-ascii 500),
    total-submissions: uint,
    total-sales: uint
  }
)

(define-map visitor-engagement
  { visitor: principal, exhibition-id: uint }
  {
    visit-block: uint,
    rating: (optional uint), ;; 1-5 stars
    feedback: (optional (string-ascii 300))
  }
)

;; public functions

;; Artist submits artwork for exhibition consideration
(define-public (submit-artwork (title (string-ascii 100)) (description (string-ascii 500)) (price uint))
  (let ((artwork-id (var-get next-artwork-id)))
    (map-set artworks artwork-id {
      artist: tx-sender,
      title: title,
      description: description,
      price: price,
      status: "submitted",
      exhibition-id: none
    })
    ;; Update artist profile
    (match (map-get? artist-profiles tx-sender)
      existing-profile (map-set artist-profiles tx-sender
        (merge existing-profile { total-submissions: (+ (get total-submissions existing-profile) u1) }))
      (map-set artist-profiles tx-sender {
        name: "",
        bio: "",
        total-submissions: u1,
        total-sales: u0
      }))
    (var-set next-artwork-id (+ artwork-id u1))
    (ok artwork-id)
  )
)

;; Update artist profile information
(define-public (update-artist-profile (name (string-ascii 100)) (bio (string-ascii 500)))
  (match (map-get? artist-profiles tx-sender)
    existing-profile (begin
      (map-set artist-profiles tx-sender
        (merge existing-profile { name: name, bio: bio }))
      (ok true))
    (begin
      (map-set artist-profiles tx-sender {
        name: name,
        bio: bio,
        total-submissions: u0,
        total-sales: u0
      })
      (ok true))
  )
)

;; Create new exhibition (curator only)
(define-public (create-exhibition (title (string-ascii 100)) (description (string-ascii 500)) (duration-blocks uint))
  (let ((exhibition-id (var-get next-exhibition-id))
        (start-block stacks-block-height)
        (end-block (+ stacks-block-height duration-blocks)))
    (map-set exhibitions exhibition-id {
      curator: tx-sender,
      title: title,
      description: description,
      start-block: start-block,
      end-block: end-block,
      status: "planned",
      total-artworks: u0
    })
    (var-set next-exhibition-id (+ exhibition-id u1))
    (ok exhibition-id)
  )
)

;; Approve artwork for exhibition (curator only)
(define-public (approve-artwork (artwork-id uint) (exhibition-id uint))
  (let ((artwork (unwrap! (map-get? artworks artwork-id) ERR-NOT-FOUND))
        (exhibition (unwrap! (map-get? exhibitions exhibition-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get curator exhibition)) ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status artwork) "submitted") ERR-ALREADY-EXISTS)
    (map-set artworks artwork-id (merge artwork {
      status: "approved",
      exhibition-id: (some exhibition-id)
    }))
    (map-set exhibitions exhibition-id (merge exhibition {
      total-artworks: (+ (get total-artworks exhibition) u1)
    }))
    (ok true)
  )
)

;; Start exhibition (curator only)
(define-public (start-exhibition (exhibition-id uint))
  (let ((exhibition (unwrap! (map-get? exhibitions exhibition-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get curator exhibition)) ERR-UNAUTHORIZED)
    (asserts! (>= stacks-block-height (get start-block exhibition)) ERR-UNAUTHORIZED)
    (map-set exhibitions exhibition-id (merge exhibition { status: "active" }))
    (ok true)
  )
)

;; Purchase artwork
(define-public (purchase-artwork (artwork-id uint))
  (let ((artwork (unwrap! (map-get? artworks artwork-id) ERR-NOT-FOUND))
        (exhibition-id (unwrap! (get exhibition-id artwork) ERR-NOT-FOUND))
        (exhibition (unwrap! (map-get? exhibitions exhibition-id) ERR-NOT-FOUND))
        (price (get price artwork))
        (commission (/ (* price (var-get gallery-commission)) u100))
        (artist-payment (- price commission)))
    (asserts! (is-eq (get status artwork) "approved") ERR-UNAUTHORIZED)
    (asserts! (is-eq (get status exhibition) "active") ERR-EXHIBITION-CLOSED)
    (asserts! (<= stacks-block-height (get end-block exhibition)) ERR-EXHIBITION-CLOSED)
    (try! (stx-transfer? price tx-sender CONTRACT-OWNER))
    (try! (stx-transfer? artist-payment CONTRACT-OWNER (get artist artwork)))
    (map-set artworks artwork-id (merge artwork { status: "sold" }))
    ;; Update artist sales count
    (match (map-get? artist-profiles (get artist artwork))
      existing-profile (map-set artist-profiles (get artist artwork)
        (merge existing-profile { total-sales: (+ (get total-sales existing-profile) u1) }))
      true)
    (ok true)
  )
)

;; Record visitor engagement
(define-public (record-visit (exhibition-id uint) (rating (optional uint)) (feedback (optional (string-ascii 300))))
  (let ((exhibition (unwrap! (map-get? exhibitions exhibition-id) ERR-NOT-FOUND)))
    (asserts! (is-eq (get status exhibition) "active") ERR-EXHIBITION-CLOSED)
    (asserts! (<= stacks-block-height (get end-block exhibition)) ERR-EXHIBITION-CLOSED)
    (match rating
      some-rating (asserts! (and (>= some-rating u1) (<= some-rating u5)) ERR-UNAUTHORIZED)
      true)
    (map-set visitor-engagement { visitor: tx-sender, exhibition-id: exhibition-id } {
      visit-block: stacks-block-height,
      rating: rating,
      feedback: feedback
    })
    (ok true)
  )
)

;; Close exhibition (curator only)
(define-public (close-exhibition (exhibition-id uint))
  (let ((exhibition (unwrap! (map-get? exhibitions exhibition-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get curator exhibition)) ERR-UNAUTHORIZED)
    (map-set exhibitions exhibition-id (merge exhibition { status: "closed" }))
    (ok true)
  )
)

;; read only functions

(define-read-only (get-artwork (artwork-id uint))
  (map-get? artworks artwork-id)
)

(define-read-only (get-exhibition (exhibition-id uint))
  (map-get? exhibitions exhibition-id)
)

(define-read-only (get-artist-profile (artist principal))
  (map-get? artist-profiles artist)
)

(define-read-only (get-visitor-engagement (visitor principal) (exhibition-id uint))
  (map-get? visitor-engagement { visitor: visitor, exhibition-id: exhibition-id })
)

(define-read-only (get-gallery-commission)
  (var-get gallery-commission)
)

(define-read-only (get-next-artwork-id)
  (var-get next-artwork-id)
)

(define-read-only (get-next-exhibition-id)
  (var-get next-exhibition-id)
)
