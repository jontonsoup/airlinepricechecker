#lang racket

(require json
         plot)

(define offers
  (read-json (open-input-file "edited-flight_output.json")))

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

(define (timestr->number str)
  (match-define (list h m s) 
                (regexp-split #rx":" str))
  (+ (* 60 60 (string->number h))
     (* 60 (string->number m))
     (string->number s)))

(define (to-points offs)
  (define (to-point off)
    (vector (timestr->number (hash-ref off 'time))
            (hash-ref off 'price)))
  (map to-point offs))

(define (partition-browser offers)
  (partition (λ (v) 
               (string=? (hash-ref v 'browser)
                         "Tor"))
             offers))

(define (plot-flight-prices flight flight-map)
  (define offers (set->list (hash-ref flight-map flight)))
  (define-values (tors ffs) (partition-browser offers))
  (list (plot (points (to-points tors)))
        (plot (points (to-points ffs)))))

(define flights (hash-keys flight-map))
#;(plot-flight-prices (flight "UA" "3905") flight-map)

(define (sort-on-fst l)
  (sort l < #:key (λ (v) (vector-ref v 0))))
#;(map vector (sort-on-fst tor-ps) (sort-on-fst ff-ps))

(define (no-change? l)
  (>= 1 (set-count (list->set l))))

(define plots
  (for/list ([(flight offers) (in-hash flight-map)])
    (define off-list (set->list offers))
    (define (to-prices offs)
      (map (λ (ht) (hash-ref ht 'price))
           offs))
    (cond [(no-change? (to-prices off-list))
           (vector 'no-change flight (hash-ref (first off-list) 'price))]
          [else
           (define-values (tors ffs) (partition-browser off-list))
           (cond [(or (no-change? (to-prices tors)))
                  (or (no-change? (to-prices ffs)))
                  'different-no-change]
                 [else
                  (plot-flight-prices flight flight-map)])])))