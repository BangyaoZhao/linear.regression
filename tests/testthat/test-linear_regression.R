test_that("linear_regression works", {
  coefs = linear_regression(c(3, 5), c(1, 2))$coefs
  coefs = as.vector(coefs)
  expect_equal(coefs, c(1, 2))
})


test_that("linear_regression works", {
  y = c(1, 2, 5, 4, 8)
  x = matrix(c(1, 5, 2, 4, 8, 5, 7, 1, 6, 9), 5, 2)
  coefs_1 = linear_regression(y, x)$coefs
  coefs_1 = as.vector(coefs_1)
  coefs_2 = lm(y ~ x)$coefficients
  coefs_2 = as.vector(coefs_2)
  expect_equal(coefs_1, coefs_2)
})

test_that("linear_regression works", {
  y = c(1, 2, 5, 4, 8)
  x = matrix(c(1, 5, 2, 4, 8, 5, 7, 1, 6, 9), 5, 2)
  coefs_1 = linear_regression(y, x, intercept = F)$coefs
  coefs_1 = as.vector(coefs_1)
  coefs_2 = lm(y ~ 0 + x)$coefficients
  coefs_2 = as.vector(coefs_2)
  expect_equal(coefs_1, coefs_2)
})

test_that("linear_regression works", {
  y_bars1 = linear_regression(mtcars_mpg, mtcars_covariates, y_bars = T)$y_bars
  y_bars2 = lm(mtcars_mpg ~ mtcars_covariates)$fitted.values
  y_bars1 = as.vector(y_bars1)
  y_bars2 = as.vector(y_bars2)
  expect_equal(y_bars1, y_bars2)
})

test_that("linear_regression works", {
  y_bars1 = linear_regression(mtcars_mpg,
                              mtcars_covariates,
                              intercept = F,
                              y_bars = T)$y_bars
  y_bars2 = lm(mtcars_mpg ~ 0 + mtcars_covariates)$fitted.values
  y_bars1 = as.vector(y_bars1)
  y_bars2 = as.vector(y_bars2)
  expect_equal(y_bars1, y_bars2)
})

test_that("linear_regression works", {
  y_bars1 = linear_regression(mtcars_mpg,
                              mtcars_covariates,
                              intercept = F,
                              y_bars = T)$y_bars
  y_bars2 = lm(mtcars_mpg ~ 0 + mtcars_covariates)$fitted.values
  y_bars1 = as.vector(y_bars1)
  y_bars2 = as.vector(y_bars2)
  expect_equal(y_bars1, y_bars2)
})

test_that("linear_regression works", {
  ssr1 = linear_regression(mtcars_mpg, mtcars_covariates, ss = T)$ss$ssr
  dfr1 = linear_regression(mtcars_mpg, mtcars_covariates, ss = T)$ss$dfr
  sse1 = linear_regression(mtcars_mpg, mtcars_covariates, ss = T)$ss$sse
  dfe1 = linear_regression(mtcars_mpg, mtcars_covariates, ss = T)$ss$dfe
  res1 = c(dfr1, dfe1, ssr1, sse1)
  res2 = anova(lm(mtcars_mpg ~ mtcars_covariates))
  res2 = c(res2[[1]],res2[[2]])
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  ssr1 = linear_regression(mtcars_mpg,intercept = F ,mtcars_covariates, ss = T)$ss$ssr
  dfr1 = linear_regression(mtcars_mpg,intercept = F ,mtcars_covariates, ss = T)$ss$dfr
  sse1 = linear_regression(mtcars_mpg,intercept = F ,mtcars_covariates, ss = T)$ss$sse
  dfe1 = linear_regression(mtcars_mpg,intercept = F ,mtcars_covariates, ss = T)$ss$dfe
  res1 = c(dfr1, dfe1, ssr1, sse1)
  res2 = anova(lm(mtcars_mpg ~ 0+mtcars_covariates))
  res2 = c(res2[[1]],res2[[2]])
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
res1=linear_regression(mtcars_mpg,mtcars_covariates,t_test=T)
res1=res1$t_test$p_values
res1=as.vector(res1)
res2=summary(lm(mtcars_mpg~mtcars_covariates))
res2=res2[["coefficients"]][,4]
res2=as.vector(res2)
  expect_equal(res1, res2)
})



test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,intercept = F,t_test=T)
  res1=res1$t_test$p_values
  res1=as.vector(res1)
  res2=summary(lm(mtcars_mpg~0+mtcars_covariates))
  res2=res2[["coefficients"]][,4]
  res2=as.vector(res2)
  expect_equal(res1, res2)
})


test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,variances = T)
  res1=res1$variances$v_coefs
  res1=diag(res1)
  res1=sqrt(res1)
  res1=as.vector(res1)
  res2=summary(lm(mtcars_mpg~mtcars_covariates))
  res2=res2[["coefficients"]][,2]
  res2=as.vector(res2)
  expect_equal(res1, res2)
})


test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,intercept = F,variances = T)
  res1=res1$variances$v_coefs
  res1=diag(res1)
  res1=sqrt(res1)
  res1=as.vector(res1)
  res2=summary(lm(mtcars_mpg~0+mtcars_covariates))
  res2=res2[["coefficients"]][,2]
  res2=as.vector(res2)
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,f_test  = T)
  res1=c(res1$f_test$f_statistics,res1$f_test$df)
  res2=summary(lm(mtcars_mpg~mtcars_covariates))
  res2=c(res2[["fstatistic"]][["value"]],res2[["fstatistic"]][["numdf"]],res2[["fstatistic"]][["dendf"]])
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,intercept = F,f_test  = T)
  res1=c(res1$f_test$f_statistics,res1$f_test$df)
  res2=summary(lm(mtcars_mpg~0+mtcars_covariates))
  res2=c(res2[["fstatistic"]][["value"]],res2[["fstatistic"]][["numdf"]],res2[["fstatistic"]][["dendf"]])
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,hat_matrix = T)
  res1=res1$hat_matrix
  res1=diag(res1)
  res2=lm.influence(lm(mtcars_mpg~mtcars_covariates))
  res2=res2$hat
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,intercept = F,hat_matrix = T)
  res1=res1$hat_matrix
  res1=diag(res1)
  res2=lm.influence(lm(mtcars_mpg~0+mtcars_covariates))
  res2=res2$hat
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,residuals = T)
  res1=res1$residuals
  res1=as.vector(res1)
  res2=lm(mtcars_mpg~mtcars_covariates)
  res2=res2$residuals
  res2=as.vector(res2)
  expect_equal(res1, res2)
})

test_that("linear_regression works", {
  res1=linear_regression(mtcars_mpg,mtcars_covariates,intercept = F,residuals = T)
  res1=res1$residuals
  res1=as.vector(res1)
  res2=lm(mtcars_mpg~0+mtcars_covariates)
  res2=res2$residuals
  res2=as.vector(res2)
  expect_equal(res1, res2)
})


























