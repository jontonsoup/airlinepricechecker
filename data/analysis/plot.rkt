#lang racket

(require json
         math/statistics
         plot
         unstable/list)

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
  (define points 
    (sort (map to-point offs) < #:key (λ (v) (vector-ref v 0))))
  
  (define min (vector-ref (first points) 0))
  (map (λ (v) (vector (/ (- (vector-ref v 0) min) (* 60 60))
                      (vector-ref v 1)))
       points))

(define (partition-browser offers)
  (partition (λ (v) 
               (string=? (hash-ref v 'browser)
                         "Tor"))
             offers))

(define (plot-flight-prices flight flight-map)
  (define offers (set->list (hash-ref flight-map flight)))
  (define prices (map (λ (v) (hash-ref v 'price)) offers))
  (define min-price (apply min prices))
  (define max-price (apply max prices))
  (define extra (/ (- max-price min-price) 10))
  
  (define-values (tors ffs) (partition-browser offers))
  (define title (string-append "Flight " (flight-airline flight) " " (flight-num flight)))
  (plot-file
   (list (points (to-points tors) #:color "red"  #:label "tor")
         (points (to-points ffs)  #:color "blue" #:label "firefox"))
   (string-append "plots/" title ".png")
   'png
   #:title title
   #:y-label "Price ($)"
   #:x-label "Elapsed Time (hours)"
   #:y-min (- min-price extra)
   #:y-max (+ max-price extra)
   #:legend-anchor 'left))

(define flights (hash-keys flight-map))

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

(define-values (tors ffs)
               (partition-browser (set->list (apply set-union (hash-values flight-map)))))

(define flight-tor-ips
  (for/hash ([(flight offers) (in-hash flight-map)])
    (define tor-ips
      (list->set
       (map (λ (ht) (hash-ref ht 'ip))
            (filter (λ (ht) (string=? "Tor" (hash-ref ht 'browser)))
                    (set->list offers)))))
    (values flight tor-ips)))
(define tor-ips
  (apply set-union (hash-values flight-tor-ips)))

(define ip-tor-flights
  (for/hash ([ip (in-set tor-ips)])
    (define flight-counts
      (for/hash ([(flight ips) (in-hash flight-tor-ips)])
        (define count 
          (cond [(set-member? ips ip)
                 (set-count (filter (λ (ht) (string=? ip (hash-ref ht 'ip)))
                                    (set->list (hash-ref flight-map flight))))]
                [else 0]))
        (values flight count)))
    (values ip flight-counts)))

(define ip-flight-counts
  (for/list ([(ip flight-counts) (in-hash ip-tor-flights)])
    (cons ip (hash-values flight-counts))))
(define grouped (group-by identity (map (compose (curry apply max) cdr) ip-flight-counts)))

(define ip-flight-count-densities
  (for/list ([l (in-list (sort grouped < #:key car))])
    (vector (car l) (length l))))

(plot-file (discrete-histogram ip-flight-count-densities)
      "ip-flight-max-histogram.png"
      'png
      #:title "Max Number of times a Flight is Seen by Specific Tor Ip Address"
      #:x-label "Max Number of times Flight is Seen"
      #:y-label "Number of Tor Ip Addresses"
      )

(define interval-densities
  (let ([id-map
         (for/fold ([ht (hash)])
           ([(flight ips) (in-hash flight-tor-ips)])
           (define i (* 10 (floor (/ (set-count ips) 10))))
           (define interval (ivl i (+ i 10)))
           (hash-update ht interval add1 1))])
    (for/list ([(i d) (in-hash id-map)])
    (vector i d))))
(plot-file (discrete-histogram interval-densities) 
      "flight-ip-histogram.png"
      'png
      #:x-label "Number of Tor Ip Addresses"
      #:y-label "Number of Flights seen by that Many Tor Ip Addresses"
      #:title "Number of Different IPs Seeing Flight")