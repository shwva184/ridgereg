context("ridgereg")

data("iris")

test_that("ridge rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris, lambda = 0))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis, lambda = 0))
})



test_that("class is correct", {
  reg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_s3_class(reg_mod, "ridgereg")
})

test_that("predict() works", {
  reg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 1)
  
  expect_equal(round(unname(predict(reg_mod)[c(1,5,7)]),2), c(1.86, 1.55, 1.11))    
})


test_that("coef() works", {
  reg_mod <- ridgereg(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda = 2)
  
  expect_true(all(round(unname(coef(reg_mod)),2) %in% c(-2.43, -1.33, 1.75)))
})

library(MASS)
test_that("Compare results to lm.ridge", {
  lm_mod = lm.ridge(Petal.Length~Species, data = iris, lambda = 1)
  reg_mod = ridgereg(Petal.Length~Species, data = iris, lambda = 1)
  expect_true(all(round(coef(lm_mod),2) == round(coef(reg_mod),2)))
  
  lm_mod = lm.ridge(Petal.Length~Species, data = iris, lambda = 0.01)
  reg_mod = ridgereg(Petal.Length~Species, data = iris, lambda = 0.01)
  expect_true(all(round(coef(lm_mod),2) == round(coef(reg_mod),2)))
  
})