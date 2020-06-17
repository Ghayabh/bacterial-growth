test_that("Testing the model modBaran_without_lag", {
  expect_equal(length(modBaran_without_lag(seq(0,24,1), 0.5,3,5)),25)
  expect_equal(modBaran_without_lag(1, 0.5,3,5),3.21433897934848)
  expect_equal(modBaran_without_lag(temp=24, 0.5,3,5),4.99973590895757)
})
