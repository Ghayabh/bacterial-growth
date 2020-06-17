test_that("Testing the Zwietering model", {
  expect_equal(modZwite(5,3,9,7,0.5,0.4),0.362332241029305)
  expect_equal(modZwite(7,3,9,7,0.5,0.4),0.5)
  expect_equal(modZwite(9,3,9,7,0.5,0.4),0)
  expect_equal(length(modZwite(c(5,6),4,9,7,0.035,0.02)),2)
})
