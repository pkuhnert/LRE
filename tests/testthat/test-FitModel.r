test_that("FitModel returns a valid object", {
  
  
  # Read in preloaded data from LRE package
  burd <- ReadInDataFromR(x.C = burdRC, x.Q = burdRQ)
  
  
  # Run through CreateData 
  date.rangeM <- c("1973-12-02", "2015-06-30")
  date.rangeP <- c("1973-12-02", "2015-06-30")
  loaddata <- CreateData(Q = burd$Q, Conc = burd$Conc,
                         date.range = list(model = date.rangeM, pred = date.rangeP, hour = FALSE),
                         samp.unit = "day", Ytype = "WY", Qflush = 0.9,
                         Reg = list(type = "none", rainfall = NULL, date = NULL))
  
  mod1 <- FitModel(x = loaddata$CQ, parms = list(flow = "quadratic", seasonal = TRUE,
                                                 RFlimb = FALSE,
                                                 MA = c(MA1day = FALSE, MA2days = FALSE,
                                                        MAweek = TRUE,  MAmonth = TRUE,
                                                        MA6months = TRUE, MA12months = TRUE),
                                                 trend = FALSE, correlation = FALSE))
  
  # Expectations
  # 1. Output should be a GAM object
  expect_s3_class(mod1, "gam")
  
  # 2. Should contain fitted values
  expect_true("fitted.values" %in% names(mod1))
  
  # 3. Fitted values should be numeric and not empty
  expect_type(mod1$fitted.values, "double")
  expect_gt(length(mod1$fitted.values), 10)
  
  # 4. Model formula should include the response variable
  expect_true(grepl("Conc", as.character(formula(mod1))[2]))
  
  # 5. Check summary can be generated without error
  expect_silent(summary(mod1))
})
