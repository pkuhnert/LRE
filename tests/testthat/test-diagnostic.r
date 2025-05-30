test_that("diagnostic runs and returns a plot object", {
  burd <- ReadInDataFromR(x.C = burdRC, x.Q = burdRQ)
  loaddata <- CreateData(Q = burd$Q, Conc = burd$Conc,
                         date.range = list(model = c("1973-12-02", "2015-06-30"),
                                           pred = c("1973-12-02", "2015-06-30"),
                                           hour = FALSE),
                         samp.unit = "day", Ytype = "WY", Qflush = 0.9,
                         Reg = list(type = "none", rainfall = NULL, date = NULL))
  mod1 <- FitModel(x = loaddata$CQ, parms = list(flow = "quadratic", seasonal = TRUE,
                                                 RFlimb = FALSE,
                                                 MA = c(MA1day = FALSE, MA2days = FALSE,
                                                        MAweek = TRUE,  MAmonth = TRUE,
                                                        MA6months = TRUE, MA12months = TRUE),
                                                 trend = FALSE, correlation = FALSE))
  
  result <- diagnostic(mod1)
  
  # Expectations
  expect_true(is.list(result))
  expect_true(length(result) == 2 & !any(unlist(lapply(result, is.null))))
  expect_silent(diagnostic(mod1))
})
