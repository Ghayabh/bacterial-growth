test_that("Cheking the model modelPresserext", {
  expect_equal(modelPresserext(7,3,9,0.5),0.4949505)
  expect_equal(modelPresserext(3,4,9,0.03),0)
  expect_equal(modelPresserext(6,4,9,0.03),0.0296703)
  expect_equal(length(modelPresserext(c(7,5),3,9,0.5)),2)
})
