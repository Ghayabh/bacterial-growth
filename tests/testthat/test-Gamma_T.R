test_that("Cheking the model Gamma_T", {
  expect_equal(Gamma_T(10,4,37,20,0.037),0.0244320652173913)
  expect_equal(Gamma_T(3,4,37,20,0.037),0)
  expect_equal(Gamma_T(20,4,37,20,0.037),0.037)
})

