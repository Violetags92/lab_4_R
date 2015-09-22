context("linreg print correctness")
test_that("linreg is a function with differents methods",{
  expect_that(print(Petal.Length~Species,data=iris), equals(c(1.462,2.798,4.09))) 
}
)

context("linreg print correctness")
test_that("linreg is a function with differents methods",{
  expect_that(print(Petal.Length~Species,data=iris), equals(lm(Petal.Length~Species,data=iris)))
}
)

context("linreg print correctness")
test_that("linreg is a function with differents methods",{
  m1 <- lm(Petal.Length~Species,data=iris)
  expect_that(print(Petal.Length~Species,data=iris), equals(m1))
}
)

context("linreg coef correctness")
test_that("linreg is a function with differents methods",{
  m1 <- lm(Petal.Length~Species,data=iris)
  expect_that(coef(Petal.Length~Species,data=iris), equals(m1$coef))
}
  )

context("linreg coef correctness")
test_that("linreg is a function with differents methods",{
  m1 <- lm(Petal.Length~Species,data=iris)
  expect_that(resid(Petal.Length~Species,data=iris), equals(m1$resid))
}
)
