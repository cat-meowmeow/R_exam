setwd("D:/hacxy/Downloads/Final Exam/Givens Rotations for Eigen Decompostion")
source("eigen_fun_My_Solutions.R")
library(testthat)

#####################
set.seed(1234567890)
d =3
z = matrix(rnorm(d^d),d,d)
X = z + t(z)
# X
eigen_fun(X)
eigen(X)

test_that("this is d =3 value test",{
  expect_equal(eigen_fun(X)$values,eigen(X)$values)
}
)

#####################
set.seed(1234567890)
d =2
z = matrix(rnorm(d^d),d,d)
X = z + t(z)
# X
eigen_fun(X)
eigen(X)

test_that("this is d =2 value test",{
  expect_equal(eigen_fun(X)$values,eigen(X)$values)
}
)
#####################
set.seed(1234567890)
d =4
z = matrix(rnorm(d^d),d,d)
X = z + t(z)
# X
eigen_fun(X)
eigen(X)

test_that("this is d =4 value test",{
  expect_equal(eigen_fun(X)$values,eigen(X)$values)
}
)
#####################
set.seed(1234567890)
d =5
z = matrix(rnorm(d^d),d,d)
X = z + t(z)
# X
eigen_fun(X)
eigen(X)

test_that("this is d =5 value test",{
  expect_equal(eigen_fun(X)$values,eigen(X)$values)
}
)

