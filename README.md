# linear.regression

  <!-- badges: start -->
  [![Travis build status](https://travis-ci.org/BangyaoZhao/linear.regression.svg?branch=master)](https://travis-ci.org/BangyaoZhao/linear.regression)
  <!-- badges: end -->
  
    <!-- badges: start -->
  [![Codecov test coverage](https://codecov.io/gh/BangyaoZhao/linear.regression/branch/master/graph/badge.svg)](https://codecov.io/gh/BangyaoZhao/linear.regression?branch=master)
  <!-- badges: end -->


`linear.regression` is an adapted version of `stats::lm` function. It is designed to be widely useful, user friendly, and reliable. It has been fully and seriously tested to ensure stability. In the era of big data, `linear.regression` is the powerful tool to creat possibilities. 

## installation

Install `linear.regression` package from github:
```
devtools::install_github("BangyaoZhao/linear.regression")
```

Load package by:
```
library(linear.regression)
```

## usage

`linear_regression` is the only function the package provides, but it is widely useful.

`linear.regression` allows users to: 

1. Fit linear regression model with or without intercept and obtain the estimated coeficients. 
2. Obtain estimated y's.
3. Obtain sum of squares. 
4. Obtain variances of estimators.
5. Obtain the t and f statistics and p values.
6. Obtain the hat matrix. 
7. Obtain residuals. 

The details of using the function can be found in the Vignettes. Use the following code to see Vignettes:
```
browseVignettes("linear.regression")
```

## Learning more

linear regression in difficult in concept and math, however there are now a number of
valuable resources to help!

1.Introduction_to_Linear_Regression_Analysis__5th_ed._Douglas_C._Montgomery__Elizabeth_A._Peck__and_G._

2.[Wikipedia](https://en.wikipedia.org/wiki/Linear_regression)
