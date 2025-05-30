test_that("plot method for an object of class fitmodel runs without error", {
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
  
  
  # Expectations
  
  # Call the plot method
  expect_silent(p <- plot(mod1, Qreg = loaddata$Qreg, data = loaddata$CQ))
  
  # Optional: check return class if applicable
  if (!is.null(p)) {
    expect_true(inherits(p, c("gg", "ggplot")) || is.list(p))
  }
  expect_type(p, "list")
  
})
