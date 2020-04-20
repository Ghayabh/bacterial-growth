test_that("Checking the model Proba_c", {
  expect_equal(Proba_c(10,Tinf=-3.6,Tsup=17.3,5,c=7.6,pHinf=-4.34,pHsup=5.93,0.95,awinf=0.917,awsup=0.988),0.158267534113097)
  expect_equal(Proba_c(-4,Tinf=-3.6,Tsup=17.3,c=7.6,5,pHinf=-4.34,pHsup=5.93,0.97,awinf=0.917,awsup=0.988),0)
  expect_equal(Proba_c(20,Tinf=-3.6,Tsup=17.3,c=7.6,5,pHinf=-4.34,pHsup=5.93,0.97,awinf=0.917,awsup=0.988),0.746439172578796)
  })
