; Josh Reavis
; Summer 2016


#lang racket

; Checks a set to see if it has the reflexive property
(define reflexive (λ(L S)
                    (cond ((null? S) #t) ;we've gone through everything and haven't returned false yet means it's true
                          (else
                          (cond ((eq? (reflexhelper (car S) L) #f) #f ) 
                                (else
                                 (reflexive L (cdr S)))
                                ))
                          )))

(define reflexhelper (λ(e L) ; takes on the first element of the set, and the list of pairs 
                       (cond ( (null? L) #f)
                             ( (and (equal? e (car (car L))) (equal? e (cdr (car L))) ) #t) ;if both elements in the first pair in the list are equal to the element of the set, return true
                             (else
                              (reflexhelper e (cdr L))
                              ))))
                              
; Checks a set to see if it has the symmetric property
(define symmetric (λ(L)
                    (symmetrichelpercaller L L) ; since the lab defines that symmetric only has one input parameter, we need another function to keep track of our pairs
                    ))

(define symmetrichelper (λ(P L) ; essentially the inner loop which takes a pair P and checks each other pair in the list to see if we have a symmetric pair to P.
                          (cond ( (null? L) #f)
                                ( (and (equal? (car P) (cdr (car L))) (equal? (cdr P) (car (car L))) ) #t) ;if both elements in the first pair in the list are equal to the element of the set, return true
                                (else
                                 (symmetrichelper P (cdr L))
                                 ))))

(define symmetrichelpercaller (λ(P L)
                    (cond ((null? P) #t) ;we've gone through everything and haven't returned false yet means it's true
                          (else
                          (cond ((eq? (symmetrichelper (car P) L) #f) #f ) 
                                (else
                                 (symmetrichelpercaller  (cdr P) L))
                                ))
                          )))
                          
; Checks a set to see if it has the transitive property
(define transitive (λ(L)
                    (transitivehelpercaller L L) ; since the lab defines that transitive only has one input parameter, we need another function to keep track of our pairs
                    ))

(define transitivehelper (λ(P L) ; essentially the inner loop which takes a pair P and checks to see if the list is transitive for that pair.
                          (cond ( (null? L) #t)
                                ((equal? (cdr P) (car (car L)))
                                       (if (transitivecheck P (car L) L) (transitivehelper P (cdr L)) #f ))   ;if we find a pair for which the 2nd element in the pair is equal to the 1st element of another we must check for transitivity
                                (else                            ; if ^ transcheck is true, keep going
                                 (transitivehelper P (cdr L))
                                 ))))

(define transitivecheck (λ(P X L)
                          (cond ( (null? L) #f)
                                ( (and (equal? (car P) (car (car L))) (equal? (cdr X) (cdr (car L))) ) #t) ;for the first element in the pair P do we have another pair who's first element is the same and second element is the 2nd element of X if so return true
                                (else
                                 (transitivecheck P X (cdr L))
                                 ))))                          
                          

(define transitivehelpercaller (λ(P L) ; calls transitivehelper with one of the pairs in the list and the list
                                       ; recursively calls itself with a smaller pair list each time
                    (cond ((null? P) #t) ;we've gone through everything and haven't returned false yet means it's true
                          (else
                          (cond ((eq? (transitivehelper (car P) L) #f) #f ) 
                                (else
                                 (transitivehelpercaller  (cdr P) L))
                                ))
                          )))
                          
; Checks a set to see if it has the antitransitive property
(define antitransitive (λ(L)
                    (antitransitivehelpercaller L L) ; since the lab defines that antitransitive only has one input parameter, we need another function to keep track of our pairs
                    ))

(define antitransitivehelper (λ(P L) ; essentially the inner loop which takes a pair P and checks to see if the list is antitransitive for that pair.
                          (cond ( (null? L) #t)
                                ((equal? (cdr P) (car (car L)))
                                       (if (antitransitivecheck P (car L) L) #f (antitransitivehelper P (cdr L))))   ;if we find a pair for which the 2nd element in the pair is equal to the 1st element of another we must check for transitivity
                                (else                            ; if ^ we find transitivity return false and stop
                                 (antitransitivehelper P (cdr L))
                                 ))))

(define antitransitivecheck (λ(P X L) ; returns true if we find a transitive property in the pair
                          (cond ( (null? L) #f)
                                ( (and (equal? (car P) (car (car L))) (equal? (cdr X) (cdr (car L))) ) #t) ;for the first element in the pair P do we have another pair who's first element is the same and second element is the 2nd element of X if so return true
                                (else
                                 (antitransitivecheck P X (cdr L))
                                 ))))                          
                          

(define antitransitivehelpercaller (λ(P L) ; calls antitransitivehelper with one of the pairs in the list and the list
                                       ; recursively calls itself with a smaller pair list each time
                    (cond ((null? P) #t) ;we've gone through everything and haven't returned false yet means it's true
                          (else
                          (cond ((eq? (antitransitivehelper (car P) L) #f) #f ) 
                                (else
                                 (antitransitivehelpercaller  (cdr P) L))
                                ))
                          )))
                    

;Test cases I have made which should all result in true.
;(reflexive '( (1 . 2) (1 . 1) (2 . 2) (3 . 3) ) '(1 2 3))
;(symmetric '( (1 . 2) (3 . 2) (2 . 3) (2 . 1) (5 . 5)) )
;(transitive '( (1 . 2) (2 . 3) (1 . 3) ) )
;(antitransitive '( (1 . 2) (2 . 3) (1 . 3) ) )
