## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(LRE)  
library(ggplot2)
library(gratia)
library(patchwork)

## ----echo=TRUE, fig.align = "center", eval = FALSE----------------------------
# # Version 1: Read In Burdekin data from an external file
# burd <- ReadInData(dirnm = system.file("extdata", package = "LRE"), filenm = "/BurdRdaily", Cnames = "TSS",
#                    format = "%Y-%m-%d")

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5----
# Version 2: burdRC and burdRQ are already stored as part of the package
burd <- ReadInDataFromR(x.C = burdRC, x.Q = burdRQ)
plot(burd)
summary(burd)
ghist(burd)

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5----
date.rangeM <- c("1973-12-02", "2015-06-30")
date.rangeP <- c("1973-12-02", "2015-06-30")
loaddata <- CreateData(Q = burd$Q, Conc = burd$Conc,
                       date.range = list(model = date.rangeM, pred = date.rangeP, hour = FALSE),
                       samp.unit = "day", Ytype = "WY", Qflush = 0.9,
                       Reg = list(type = "none", rainfall = NULL, date = NULL))

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5----
regplots <- plot(loaddata, Type = "WY")
names(regplots) # terms we can plot

# Rising Falling Limb
regplots$p_RiseFallLimb

# Distributional Summary
regplots$p_DistSum

# Flow and Concentration summary
regplots$p_CQsum

# Smooth parameters
regplots$p_SmoothParms

## ----echo=TRUE, fig.align = "center", eval = FALSE----------------------------
# # Save output as a pdf file
# ggsave("p_SmoothParms.pdf", regplots$p_SmoothParms)

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE------------
summary(loaddata)

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5, cache = FALSE----
mod1 <- FitModel(x = loaddata$CQ, parms = list(flow = "quadratic", seasonal = TRUE,
                                               RFlimb = FALSE,
                                               MA = c(MA1day = FALSE, MA2days = FALSE,
                                                      MAweek = TRUE,  MAmonth = TRUE,
                                                      MA6months = TRUE, MA12months = TRUE),
                                               trend = FALSE, correlation = FALSE))
summary(mod1)
anova(mod1)
print(class(mod1))

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5----
mod1D <- diagnostic(mod1)
names(mod1D)
# Diagnostic Plots
mod1D$pD
# ACF of residuals
mod1D$pacf

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5----
library(gratia)    # exploring smoothers
library(patchwork) # package for arranging plots
p1 <- draw(mod1, select = "s(month)") 
p2 <- draw(mod1, select = "s(MAweek)") 
p3 <- draw(mod1, select = "s(MAmonth)") 
p4 <- draw(mod1, select = "s(MA6months)") 
p5 <- draw(mod1, select = "s(MA12months)") 
p1 + p2 + p3 + plot_layout(ncol = 3)
p4 + p5 + plot_layout(ncol = 2)


## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=8, fig.width=6, fig.asp=.5----
#Exploring predictions from model

mod1_expl <- plot(mod1, Qreg = loaddata$Qreg, data = loaddata$CQ) 
names(mod1_expl)
mod1_expl$pConcInt

## ----echo=TRUE, fig.align = "center", eval = FALSE----------------------------
# # Investigate predicted concentration using ggplotly
# #
# library(plotly)
# ggplotly(mod1I$pConc)

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, results = "hide"----
predLoad <- predictL(object = mod1, objfix = mod1$gam, x = loaddata, flow.error = list(me = 0, ce = 0),
                  samp.unit = "day", pvalue = 0.2)

## ----echo=TRUE, fig.align = "center", eval = TRUE, message = FALSE, fig.height=10, fig.width=8, fig.asp=.5----
mod1pL <- plot(predLoad$loadest, type = "annual", Conc = "TSS", scale = "Mt")
names(mod1pL)

# Annual loads
mod1pL$pL1

# Flow weighted concentrations
mod1pL$pFWC1

## ----echo=TRUE, fig.align = "center", eval = FALSE----------------------------
# # interactive flow weighted concentrations
# ggplotly(mod1pL$pFWC1)

## ----echo=TRUE, fig.align = "center", eval = TRUE-----------------------------
results <- predLoad$loadest
names(results)

## ----echo=TRUE, fig.align = "center", eval = TRUE-----------------------------
date.rangeM <- c("2011-01-01", "2015-06-30")
date.rangeP <- c("2011-01-01", "2015-06-30")
loaddata <- CreateData(Q = burdRNA$Q, Conc = burdRNA$Conc,
                       date.range = list(model = date.rangeM, pred = date.rangeP, hour = FALSE),
                       samp.unit = "day", Ytype = "WY", Qflush = 0.9,
                       Reg = list(type = "ss"))

## ----echo=TRUE, fig.align = "center", eval = TRUE-----------------------------
date.rangeM <- c("2011-01-01", "2015-06-30")
date.rangeP <- c("2011-01-01", "2015-06-30")
loaddata <- CreateData(Q = burdRNA$Q, Conc = burdRNA$Conc,
                       date.range = list(model = date.rangeM, pred = date.rangeP, hour = FALSE),
                       samp.unit = "day", Ytype = "WY", Qflush = 0.9,
                       Reg = list(type = "qrforest", rainfall = burd_rain$Rainfall,
                                  date = burd_rain$Date))

