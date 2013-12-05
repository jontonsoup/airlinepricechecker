#lang racket

(require json
         plot)

(define (json-from-file f)
  (read-json (open-input-file f)))

(define offers
  (append (json-from-file "edited-flight_output.json")
          (json-from-file "flight_output2.json")))

(struct flight (airline num)
  #:transparent)

(define (get-flight-id off)
  (flight (hash-ref off 'airlineCode) (hash-ref off 'flightNum)))

(define flight-map
  (for/fold ([ht (hash)])
            ([offer (in-list offers)])
    (define (update old-set)
      (set-add old-set offer))
    (hash-update ht
                 (get-flight-id offer)
                 update
                 (set offer))))

(define (timestr->number dstr tstr)
  #;#;
  (define y (string->number (substring dstr 1 4)))
  (define mo (string->number (substring dstr 4 6)))
  (define d (string->number (substring dstr 6 8)))
  (match-define (list h m s) 
                (regexp-split #rx":" tstr))
  (+ (* 24 60 60 d)
     (* 60 60 (string->number h))
     (* 60 (string->number m))
     (string->number s)))

(define (to-points offs)
  (define (to-point off)
    (vector (timestr->number (hash-ref off 'date) (hash-ref off 'time))
            (hash-ref off 'price)))
  (sort (map to-point offs) < #:key (λ (v) (vector-ref v 0))))

(define (partition-browser offers)
  (partition (λ (v) 
               (string=? (hash-ref v 'browser)
                         "Tor"))
             offers))

(define (plot-flight-prices flight flight-map)
  (define offers (set->list (hash-ref flight-map flight)))
  (define-values (tors ffs) (partition-browser offers))
  (plot (list (points (to-points tors) #:color "red"  #:label "tor")
              (points (to-points ffs)  #:color "blue" #:label "firefox"))
        #:y-label "Price ($)"
        #:x-label "Elapsed Time (seconds)"
        #:legend-anchor 'left))

(define flights (hash-keys flight-map))
#;(plot-flight-prices (flight "UA" "3905") flight-map)

(define (sort-on-fst l)
  (sort l < #:key (λ (v) (vector-ref v 0))))
#;(map vector (sort-on-fst tor-ps) (sort-on-fst ff-ps))

(define (no-change? l)
  (>= 1 (set-count (list->set l))))

(define plots
  (for/list ([(flight offers) (in-hash flight-map)])
    (displayln flight)
    (define off-list (set->list offers))
    (define (map-key offs key)
      (map (λ (ht) (hash-ref ht key))
           offs))
    (define (to-prices offs)
      (map-key offs 'price))
    (define (to-times offs)
      (map-key offs 'time))
    (cond [(no-change? (to-prices off-list))
           (vector 'no-price-change flight (hash-ref (first off-list) 'price))]
          [(no-change? (to-times off-list))
           (vector 'no-time-change flight )]
          [else
           (define-values (tors ffs) (partition-browser off-list))
           (cond [(or (no-change? (to-prices tors)))
                  (or (no-change? (to-prices ffs)))
                  (vector 'different-no-change flight)]
                 [(or (no-change? (to-times tors))
                      (no-change? (to-times ffs)))
                  (vector 'different-no-time-change flight)]
                 [else
                  (plot-flight-prices flight flight-map)])])))
plots
