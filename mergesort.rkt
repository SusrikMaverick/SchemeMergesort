(define L '(3 5 2 8 6 1 4 9 7))

(display "---- original list \n")
(display L)
(newline)

; Define center as the midpoint of the list
(define center (quotient (length L) 2))

; Procedure to extract left sublist
(define (leftlist lst n)
  (if (= n 0)
      '()
      (cons (car lst)
            (leftlist (cdr lst) (- n 1)))))

; Procedure to extract right sublist
(define (rightlist lst n)
  (if (= n 0)
      lst
      (rightlist (cdr lst) (- n 1))))

(display "---- left and right lists\n")
(display (leftlist L center))
(newline)
(display (rightlist L center))
(newline)

; Procedure to extract left half of the list
(define (lefthalf lst)
  (leftlist lst (quotient (length lst) 2)))

; Procedure to extract right half of the list
(define (righthalf lst)
  (rightlist lst (quotient (length lst) 2)))

(display "---- left and right half\n")
(display "left halves\n")
(display (lefthalf L))
(newline)
(display (lefthalf (lefthalf L)))
(newline)
(display "right halves\n")
(display (righthalf L))
(newline)
(display (righthalf (righthalf L)))
(newline)

; Procedure to merge two sorted lists
(define (merge lst1 lst2)
  (cond ((null? lst1) lst2)
        ((null? lst2) lst1)
        ((< (car lst1) (car lst2))
         (cons (car lst1) (merge (cdr lst1) lst2)))
        (else
         (cons (car lst2) (merge lst1 (cdr lst2))))))

; Procedure to perform merge sort
(define (mergesort lst)
  (if (> (length lst) 1)
      (let* ((left (lefthalf lst))
             (right (righthalf lst)))
        (merge (mergesort left) (mergesort right)))
      lst))

(display "---- mergesort test\n")
(display (mergesort L))
(newline)
