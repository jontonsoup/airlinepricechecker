#lang racket

(require json)

(define offers
  (read-json (open-input-file "edited-flight_output.json")))

(struct flight (airline num)
  #:transparent)

(define (get-flight-id off)
  (flight (hash-ref off 'airlineCode) (hash-ref off 'flightNum)))