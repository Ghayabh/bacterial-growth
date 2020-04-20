test_that("Cheking the model Gamma_T", {
  expect_equal(Gamma_T(10,4,20,37,0.037), 0.02443207)
  expect_equal(Gamma_T(3,4,20,37,0.037),0)
  expect_equal(Gamma_T(20,4,20,37,0.037),0.037)
})

