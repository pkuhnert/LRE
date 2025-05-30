test_that("CreateData returns a valid object.", {
  
  
  # Read in preloaded data from LRE package
  burd <- ReadInDataFromR(x.C = burdRC, x.Q = burdRQ)
  
  
  # Run through CreateData 
  date.rangeM <- c("1973-12-02", "2015-06-30")
  date.rangeP <- c("1973-12-02", "2015-06-30")
  loaddata <- CreateData(Q = burd$Q, Conc = burd$Conc,
                         date.range = list(model = date.rangeM, pred = date.rangeP, hour = FALSE),
                         samp.unit = "day", Ytype = "WY", Qflush = 0.9,
                         Reg = list(type = "none", rainfall = NULL, date = NULL))
  
  # Expectations
  expect_s3_class(loaddata, "regdata")
  expect_true(is.list(loaddata))
  expect_true(all(names(loaddata) == c("CQ", "Qreg", "Q")))
})

