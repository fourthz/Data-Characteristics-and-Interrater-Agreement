
## This function computes the odds under each of two conditions, as well as the odds ratios
## in the two possible formats.

main_odds <- function(model_out){
  
  var <- c(paste(names(model_out$xlevels[1]),
                 model_out$xlevels[[1]][1]),
           paste(names(model_out$xlevels[1]),
                 model_out$xlevels[[1]][2]))
  
  odds <- exp(model_out$coefficients[1])
  
  cat(unname(noquote(paste("Odds",
                           var[1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[1]) * exp(model_out$coefficients[2])
  
  cat(unname(noquote(paste("Odds",
                           var[2],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[2])
  
  cat(unname(noquote(paste("Odds Ratio",
                           var[2],
                           "to",
                           var[1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- 1 / exp(model_out$coefficients[2])
  
  cat(unname(noquote(paste("Odds Ratio",
                           var[1],
                           "to",
                           var[2],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
}
