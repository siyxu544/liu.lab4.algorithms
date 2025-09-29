test_that("lenreg rejects errounous input", {
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(linreg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})

test_that("class is correct", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  expect_s3_class(linreg_mod, "linreg")
})

test_that("print() works", {
  linreg_mod <- linreg(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

  expect_output(print(linreg_mod),"linreg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris\\)")
  expect_output(print(linreg_mod),"( )*\\(Intercept\\)( )*Sepal\\.Width( )*Sepal\\.Length")
})

test_that("plot() throws an error with corrupted or invalid input", {

  linreg_mod <- linreg(Petal.Length ~ Sepal.Width, data = iris)

  # Test for missing components
  mod_test_1 <- linreg_mod
  mod_test_1$e_hat <- NULL
  expect_error(plot(mod_test_1))

  mod_test_2 <- linreg_mod
  mod_test_2$y_hat <- NULL
  expect_error(plot(mod_test_2))

  mod_test_3 <- linreg_mod
  mod_test_3$v_hat <- NULL
  expect_error(plot(mod_test_3))

  # Test for non-numeric components
  mod_test_4 <- linreg_mod
  mod_test_4$e_hat <- "not numeric"
  expect_error(plot(mod_test_4))

  mod_test_5 <- linreg_mod
  mod_test_5$y_hat <- "not numeric"
  expect_error(plot(mod_test_5))

  mod_test_6 <- linreg_mod
  mod_test_6$v_hat <- "not numeric"
  expect_error(plot(mod_test_6))

  # Test for invalid properties of v_hat
  mod_test_7 <- linreg_mod
  mod_test_7$v_hat <- c(1, 2)
  expect_error(plot(mod_test_7))

  mod_test_8 <- linreg_mod
  mod_test_8$v_hat <- -1
  expect_error(plot(mod_test_8))
})
