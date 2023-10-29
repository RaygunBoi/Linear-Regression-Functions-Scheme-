(define (read-csv file header column)
    (let ((comma-count 0) (value-to-add "") (cols-list '())) ;initializes three variables: comma-count keeps track of how many commas the program goes over, value-to-add temporarily stores the value to be added to cols-list, and cols-list stores the values of the file's column
        (with-input-from-file file ;opens file
            (lambda ()
                (
                    let loop ((char (read-char))) ;loop that goes through every character in the file
                    (cond
                        (
                            (eof-object? char) ;if the loop reaches the end of the file, run the following statements
                            (
                                begin
                                (if header ;if statement that checks for the header
                                    (if (not(string-null? value-to-add)) ;if there is a header AND if value-to-add isn't null, add the final value to the list, remove the header from the list, and output cols-list
                                        (
                                            begin
                                            (set! cols-list (cons (string->number value-to-add) cols-list))
                                            (set! cols-list (reverse cols-list))
                                            (set! cols-list (cdr cols-list))
                                            cols-list
                                        )
                                        ( ;if value-to-add is null, remove the header from the list and output cols-list
                                            begin
                                            (set! cols-list (reverse cols-list))
                                            (set! cols-list (cdr cols-list))
                                            cols-list
                                        )
                                    )
                                    (if (not(string-null? value-to-add)) ;if there isn't a header AND if value-to-add isn't null, add the final value to the list and output it
                                        (
                                            begin
                                            (set! cols-list (cons (string->number value-to-add) cols-list))
                                            (set! cols-list (reverse cols-list))
                                            cols-list
                                        )
                                        ( ;if value-to-add is null, output cols-list
                                            begin
                                            (set! cols-list (reverse cols-list))
                                            cols-list
                                        )
                                    )
                                )
                            )
                        )
                        (
                            (char=? char #\,) ;if the current char is a comma, add 1 to comma-count and go to the next char
                            (
                                begin
                                (set! comma-count (+ comma-count 1))
                                (loop (read-char))
                            )
                        )
                        (
                            (and (= column comma-count) (not(char=? char #\newline)) (not(char-whitespace? char))) ;if comma-count is equal to the column number, and if the current char isn't a newline char, add the char to value-to-add and go to the next char
                            (
                                begin
                                (set! value-to-add (string-append value-to-add (string char)))
                                (loop (read-char))
                            )
                        )
                        (
                            (char=? char #\newline) ;if the current char is a newline char, perform the following statements
                            (
                                begin
                                (if (not(string-null? value-to-add)) ;if statement that checks if value-to-add is empty
                                    ( ;if it isn't empty, change it to a number, add it to cols-list, reset both value-to-add and comma-count, and go to the next char
                                        begin
                                        (set! cols-list (cons (string->number value-to-add) cols-list))
                                        (set! value-to-add "")
                                        (set! comma-count 0)
                                        (loop (read-char))
                                    )
                                    ( ;otherwise, reset comma-count and go to the next char
                                        begin
                                        (set! comma-count 0)
                                        (loop (read-char))
                                    )
                                )
                            )
                        )
                        ( ;go to the next char
                            else
                            (loop (read-char))
                        )
                    )
                )
            )
        )
    )
)

(define (mean values)
    (/ (apply + values) (length values)) ;divides all the numbers in values by the size of the values list
)

(define (stddev values)
    (let* ;initializes two variables: squared-differences and variance
        (
            (squared-differences (map (lambda (x) (expt (- x (mean values)) 2)) values)) ;squared-differences is a list which uses map to store the squared differences of each value and the mean
            (variance (/ (apply + squared-differences) (length values))) ;the variance is calculated here
        )
        (/ (round (* (expt 10 4) (sqrt variance))) (expt 10 4)) ;after variance is calculated, it is square rooted and rounded to 4 decimal places
    )
)

(define (regressiona xvalues yvalues)
    (let* ;initializes the following variables: n, sum-x, sum-y, sum-xy, sum-x2, and a
        (
            (n (length xvalues)) ;n grabs the length of either list, in this case xvalues
            (sum-x (apply + xvalues)) ;sum-x stores the sum of all the values in the xvalues list
            (sum-y (apply + yvalues)) ;sum-y stores the sum of all the values in the yvalues list
            (sum-xy (apply + (map * xvalues yvalues))) ;sum-xy stores the sum of all xvalues and yvalues multiplied together
            (sum-x2 (apply + (map (lambda (x) (* x x)) xvalues))) ;sum-x2 stores the sum of all xvalues squared
            (a (exact->inexact (/ (- (* n sum-xy) (* sum-x sum-y)) (- (* n sum-x2) (* sum-x sum-x))))) ;a is calculated here
        )
        (/ (round (* (expt 10 4) a)) (expt 10 4)) ;after a is calculated, it is rounded to four places
    )
)

(define (regressionb xvalues yvalues)
    (let* ;initializes the following variables: n, sum-x, sum-y, sum-xy, sum-x2, a, and b
        (
            (n (length xvalues)) ;n grabs the length of either list, in this case xvalues
            (sum-x (apply + xvalues)) ;sum-x stores the sum of all the values in the xvalues list
            (sum-y (apply + yvalues)) ;sum-y stores the sum of all the values in the yvalues list
            (sum-xy (apply + (map * xvalues yvalues))) ;sum-xy stores the sum of all xvalues and yvalues multiplied together
            (sum-x2 (apply + (map (lambda (x) (* x x)) xvalues))) ;sum-x2 stores the sum of all xvalues squared
            (a (/ (- (* n sum-xy) (* sum-x sum-y)) (- (* n sum-x2) (* sum-x sum-x)))) ;a is calculated here
            (b (- (mean yvalues) (* a (mean xvalues)))) ;b is calculated here
        )
        (/ (round (* (expt 10 4) b)) (expt 10 4)) ;after b is calculated, it is rounded to four places
    )
)

(define (correlation xvalues yvalues)
    (let* ;initializes the following variables: n, sum-x, sum-y, sum-x2, sum-y2, sum-xy, and c
        (
            (n (length xvalues)) ;n grabs the length of either list, in this case xvalues
            (sum-x (apply + xvalues)) ;sum-x stores the sum of all the values in the xvalues list
            (sum-y (apply + yvalues)) ;sum-y stores the sum of all the values in the yvalues list
            (sum-x2 (apply + (map (lambda (x) (* x x)) xvalues))) ;sum-x2 stores the sum of all xvalues squared
            (sum-y2 (apply + (map (lambda (y) (* y y)) yvalues))) ;sum-y2 stores the sum of all xvalues squared
            (sum-xy (apply + (map * xvalues yvalues))) ;sum-xy stores the sum of all xvalues and yvalues multiplied together
            (c (/ (- (* n sum-xy) (* sum-x sum-y)) (sqrt (* (- (* n sum-x2) (expt sum-x 2)) (- (* n sum-y2) (expt sum-y 2)))))) ;c is calculated here
        )
        (/ (round (* (expt 10 4) c)) (expt 10 4)) ;after c is calculated, it is rounded to four places
    )
)

; Extra Credit placeholder

(define (apply-regression sat gpa test)
    0.0000000001); Make sure to concatenate your stats.scm to this file
; Instructions:
; cat stats.scm test.scm > mytest.scm
; Then run:
; mit-scheme --load mytest.scm
; 11 test total, 1 extra credit

(newline)
(newline)

(define (double-equals a b)
  (< (abs (- a b)) 0.0001)
)

; DATA
(define sat (list 1714 1664 1760 1685 1693 1764 1764 1792 1850 1735))
(define gpa (list 2.4 2.52 2.54 2.74 2.83 3 3 3.01 3.01 3.02))


; Test Average
(display (double-equals 17421/10 (mean sat))) ;should be #t
(newline)

(display (double-equals 2.8070 (mean gpa))) ;should be #t
(newline)

; Test Standard Deviation
(display (double-equals 52.9367 (stddev sat))) ;should be #t
(newline)

(display (double-equals 0.2295 (stddev gpa))) ;should be #t
(newline)

; Test Correlation
(display (double-equals 0.5823 (correlation sat gpa))) ;should be #t
(newline)

; Test Regression
(display (double-equals 0.0025 (regressiona sat gpa)))    ;should be #t
(newline)

(display (double-equals -1.5909 (regressionb sat gpa)))    ;should be #t
(newline)

; Test Read CSV File
(define x (read-csv "data1.csv" #f 0))
(display (equal? (list 10 11 12 14 9) x)) ;should be #t
(newline)

(define y (read-csv "data1.csv" #f 1))
(display (equal? (list 21 15 23 27 18) y)) ;should be #t
(newline)

; Test Read CSV File with Header
(define x (read-csv "data2.csv" #t 0))
(display (equal? (list 10 11 12 14 9) x)) ;should be #t
(newline)

(define y (read-csv "data2.csv" #t 1))
(display (equal? (list 21 15 23 27 18) y)) ;should be #t
(newline)

; Test Extra Credit

(define test (list 1200 1650 1705 1800))
(define result (apply-regression sat gpa test)) ;should be #t

(display (equal? (list 1.4091	2.5341	2.6716	2.9091) result)) ; should be #t
(newline)
(newline)




