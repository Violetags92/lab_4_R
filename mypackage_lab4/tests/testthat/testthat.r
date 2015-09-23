
test_that("linreg is a function with differents methods",{
  expect_equal(as.vector(resid(lm(Petal.Length~Species,data=iris))), as.vector(linreg(Petal.Length~Species,data=iris)$resid), tol= 0.0001)
}
)
