test_that("if it throws error", {
  expect_error(preprojection(as.matrix(c(0,1)), as.matrix(c(3,1, 1))))
})
