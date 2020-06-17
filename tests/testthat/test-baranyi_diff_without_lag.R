test_that("checking the differential form of the baranyi_without_lag model", {
  expect_equal(length(ode(y=c(y=15), times = seq(1,24), func = baranyi_diff_without_lag, parms = c(mumax=1, ymax=10000))),48)
})
