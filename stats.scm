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
                                    ( ;if there is a header, add the final value to the list, remove the header from the list, and output cols-list
                                        begin
                                        (set! cols-list (cons (string->number value-to-add) cols-list))
                                        (set! cols-list (reverse cols-list))
                                        (set! cols-list (cdr cols-list))
                                        cols-list
                                    )
                                    ( ;if there isn't, add the final value to the list and output it
                                        begin
                                        (set! cols-list (cons (string->number value-to-add) cols-list))
                                        (set! cols-list (reverse cols-list))
                                        cols-list
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
    (let* 
        (
            (n (length xvalues))
            (sum-x (apply + xvalues))
            (sum-y (apply + yvalues))
            (sum-xy (apply + (map * xvalues yvalues)))
            (sum-x2 (apply + (map (lambda (x) (* x x)) xvalues)))
            (a (exact->inexact (/ (- (* n sum-xy) (* sum-x sum-y)) (- (* n sum-x2) (* sum-x sum-x)))))
        )
        (/ (round (* (expt 10 4) a)) (expt 10 4))
    )
)

(define (regressionb xvalues yvalues)
    (let* 
        (
            (n (length xvalues))
            (sum-x (apply + xvalues))
            (sum-y (apply + yvalues))
            (sum-xy (apply + (map * xvalues yvalues)))
            (sum-x2 (apply + (map (lambda (x) (* x x)) xvalues)))
            (a (/ (- (* n sum-xy) (* sum-x sum-y)) (- (* n sum-x2) (* sum-x sum-x))))
            (b (- (mean yvalues) (* a (mean xvalues))))
        )
        (/ (round (* (expt 10 4) b)) (expt 10 4))
    )
)

(define (correlation xvalues yvalues)
    (let* 
        (
            (n (length xvalues))
            (sum-x (apply + xvalues))
            (sum-y (apply + yvalues))
            (sum-x2 (apply + (map (lambda (x) (* x x)) xvalues)))
            (sum-y2 (apply + (map (lambda (y) (* y y)) yvalues)))
            (sum-xy (apply + (map * xvalues yvalues)))
            (c (/ (- (* n sum-xy) (* sum-x sum-y)) (sqrt (* (- (* n sum-x2) (expt sum-x 2)) (- (* n sum-y2) (expt sum-y 2))))))
        )
        (/ (round (* (expt 10 4) c)) (expt 10 4))
    )
)

; Extra Credit placeholder

(define (apply-regression sat gpa test)
    0.0000000001)