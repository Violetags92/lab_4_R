test_that("residual() correctness",{
  expect_equal(as.vector(resid(lm(Petal.Length~Species,data=iris))), linreg(Petal.Length~Species,data=iris)$residual(), tol= 0.0001)
}
)

test_that("coeff() correctness",{
  expect_equal(as.vector(coef(lm(Petal.Length~Species,data=iris))), linreg(Petal.Length~Species,data=iris)$coeff(), tol= 0.0001)
}
)

test_that("pred correctness()",{
  expect_equal(as.vector(fitted(lm(Petal.Length~Species,data=iris))), linreg(Petal.Length~Species,data=iris)$pred(), tol= 0.0001)
}
)

