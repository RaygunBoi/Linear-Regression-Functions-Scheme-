## Description
This Scheme program implements the following functions:

- `(read-csv file header column)` This function will read the column/field `column` from the file `file`. The `header` parameter indictates whether the CSV file has a header row or not. Your function will return a list of values corresponding to all the data found in the given column.
- `(regressiona xvalues yvalues)` This function will calculate the $\alpha$ parameter given the values for x (`xvalues`) and the values for y (`yvalues`). Note that `xvalues` and `yvalues` are lists of numbers.
- `(regressionb xvalues yvalues)` This function will calculate the $\beta$ parameter given the values for x (`xvalues`) and the values for y (`yvalues`). Note that `xvalues` and `yvalues` are lists of numbers.
- `(correlation xvalues yvalues)` This function will calculate the *Pearson Correlation Coefficient* given the values for x (`xvalues`) and the values for y (`yvalues`).  Note that `xvalues` and `yvalues` are lists of numbers.
- `(mean values)` This function calculates the mean of the values in the list `values`.
- `(stddev values)` This function calculates the standard deviation of the values of the list `values`.
- `(apply-regression xvalues yvalues output)` This function goes through the values in xvalues and yvalues and puts them through the linear regression equation using functions regressiona and regressionb. The output of each value is put into the `output` list.

## Testing your Program
You can load your program into scheme `scheme --load stats.scm` and test each of the functions. This is shown below.

Suppose you have the following headless CSV data file `somedata.csv`:
| | |
|--|--|
|10|21|
|11|15|
|12|23|
|14|27|
|9|18|

```scheme
(define colx (read-csv "somedata.csv" #f 0))
;Value: colx

(define coly (read-csv "somedata.csv" #f 1))
;Value coly

colx
;Value: (10 11 12 14 9)

coly
;Value (21 15 23 27 18)

```
> Basically the first line asks scheme to read the CSV file `somedata.csv` without headers (`#f` second parameter) and getting the first column of data (`0` third parameter). This is bound to the object `colx`. The same happens for `coly` with the difference that this time we want the second column (`1` third parameter).

Using the same data as before, this is what the other operations should look like (fraction or decimals with 4 places precision):
```scheme
(define a (mean colx))
;Value a

a
; Value: 56/5

(define b (mean coly))
;Value b

b
;Value 104/5

(define sd (stddev coly))
;Value sd

sd
;Value 4.1183

(define rega (regressiona colx coly))
;Value rega

rega
;Value 1.7703

(define regb (regressionb colx coly))
;Value regb

regb
;Value 0.9730


```