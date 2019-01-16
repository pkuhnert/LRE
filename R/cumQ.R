#' @describeIn LRE-CreateData-Internal Cumulative Summaries of Flow
#'
cumQ <-function(dat){

       newdat <- with(dat, {
                 cumN <- cumsum(rep(1,length(pQ)))
                 cumX <- cumsum(pQ)
                 cumM <- cumX/cumN
                 cumX2 <- cumsum(pQ*pQ)
                 cumstdev <- sqrt((cumX2-cumX*cumX/cumN)/(cumN-1))
                 cbind(csQ=cumX, cmQ=cumM, csdQ=cumstdev)
              })
       newdat
}

