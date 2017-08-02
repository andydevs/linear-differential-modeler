# LinearDifferentialModeler

A 2'nd order linear differential equation numerical solver written in Haskell

## Usage

LinearDifferentialModeler models differential equations of the form:

    a*x'' + b*x' + c*x = s*t

It takes modeling parameters from the input stream and prints a CSV table to the standard output stream (with columns representing t, x, and x'). This allows the program to be written as follows:

    $ echo 1 2 3 2 0 0 0 1 0.001 | Main > output.csv

Modeling parameters are given in this order:

1. The first coefficient in the characteristic (the a term)
2. The second coefficient in the characteristic (the b term)
3. The third coefficient in the characteristic (the c term)
4. The coefficient of the linear forcing function (the s term)
5. The initial x value (the x0 term)
6. The initial x' value (the x'0 term)
7. The initial t value (the t0 term)
8. The final t value (the tf term)
9. The change in t value (the dt term)
