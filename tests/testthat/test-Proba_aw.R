test_that("Checking the model Proba_aw", {
  expect_equal(Proba_aw(0.90,awinf=0.917,awsup=0.988), 0)
  expect_equal(Proba_aw(0.99,awinf=0.917,awsup=0.988),1)
  expect_equal(Proba_aw(0.96,0.917,0.988),0.6056338)
  expect_equal(length(Proba_aw(c(0.90,0.92,0.99),0.917,0.988)),3)
})
