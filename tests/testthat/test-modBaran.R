test_that("Checking the model modBaran", {
  expect_equal(modBaran(temp=0, mumax = 0.037, lag=4, log10N0 = 1, log10Nmax = 5 ),1)
  expect_equal(modBaran(temp=1, mumax = 0.037, lag=4, log10N0 = 1, log10Nmax = 5 ),1.01389185956989)
  expect_equal(modBaran(temp=2, mumax = 0.037, lag=4, log10N0 = 1, log10Nmax = 5 ),1.0278523300661)
})
