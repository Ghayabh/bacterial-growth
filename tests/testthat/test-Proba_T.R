test_that("Cheking the model Proba_T",{
  expect_equal(Proba_T(10,-3.6,17.3,7.6), 0.3405331)
  expect_equal(Proba_T(20,-3.6,17.3,7.6),1)
  expect_equal(Proba_T(-4,-3.6,17.3,7.6),0)
})

