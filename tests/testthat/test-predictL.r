test_that("predictL returns expected output structure", {
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
  
  pred <- predictL(object = mod1, objfix = mod1$gam, x = loaddata,
                   flow.error = list(me = 0, ce = 0),
                   samp.unit = "day", pvalue = 0.2)
 
  
  # Expectations
  
  expect_type(pred, "list")
  expect_s3_class(pred$loadest, "PredictLoad")


})