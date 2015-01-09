#lang racket

(define delta 2654435769)
(define numrounds 32)
(define decryptsum (* delta numrounds))
(define key '(1 2 3 4))

(define (encipher n v0 v1 k sum)
  (let ([newsum (+ sum delta)])
    (if (= n 0)
        (list v0 v1)
        (encipher (- n 1)
                  (bitwise-and #xFFFFFFFF (+ v0
                     (bitwise-xor 
                      (+ sum (list-ref k (bitwise-and sum 3)))
                      (+ v1 (bitwise-xor
                             (bitwise-and #xFFFFFFFF (arithmetic-shift v1 4))
                             (arithmetic-shift v1 -5))))))
                  (bitwise-and #xFFFFFFFF (+ v1 (bitwise-xor 
                         (+ newsum 
                            (list-ref k (bitwise-and 
                                         (arithmetic-shift newsum -11) 3)))
                         (+ v0 (bitwise-xor
                                (bitwise-and #xFFFFFFFF (arithmetic-shift v0 4))
                                (arithmetic-shift v0 -5))))))
                  k newsum))))


(define (decipher n v0 v1 k sum)
  (let ([newsum (- sum delta)])
    (if (= n 0)
        (list v0 v1)
        (decipher (- n 1) 
                  (bitwise-and #xFFFFFFFF (- v1 (bitwise-xor 
                         (+ newsum 
                            (list-ref k (bitwise-and 
                                         (arithmetic-shift newsum -11) 3)))
                         (+ v0 (bitwise-xor
                                (bitwise-and #xFFFFFFFF (arithmetic-shift v0 4))
                                (arithmetic-shift v0 -5))))))
                  (bitwise-and #xFFFFFFFF (- v0
                     (bitwise-xor 
                      (+ sum (list-ref k (bitwise-and sum 3)))
                      (+ v1 (bitwise-xor
                             (bitwise-and #xFFFFFFFF (arithmetic-shift v1 4))
                             (arithmetic-shift v1 -5))))))
                  k newsum))))

