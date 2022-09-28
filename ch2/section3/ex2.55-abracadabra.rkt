#lang sicp

(car ''abracadabra) ; prints quote

; This happens because ''abracadabra, which is same as
; (quote (quote abracadabra)), creates the list
; (quote abracadabra). Thus, (car ''abracadbra), which
; is same as (car '(quote abracadabra)), is quote.
