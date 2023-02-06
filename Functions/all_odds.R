
## This function computes all combinations of odds and odds ratios for two-factor interaction
## in logistic regression.

all_odds <- function(model_out){
  
  var_1 <- c(paste(names(model_out$xlevels[1]),
                   model_out$xlevels[[1]][1]),
             paste(names(model_out$xlevels[1]),
                   model_out$xlevels[[1]][2]))
  
  var_2 <- c(paste(names(model_out$xlevels[2]),
                   model_out$xlevels[[2]][1]),
             paste(names(model_out$xlevels[2]),
                   model_out$xlevels[[2]][2]))
  
  vars <- rbind(var_1, var_2)
  
  odds <- exp(model_out$coefficients[1])
  
  cat(unname(noquote(paste("Odds",
                           vars[1,1],
                           vars[2,1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[1]) * exp(model_out$coefficients[3])
  
  cat(unname(noquote(paste("Odds",
                           vars[1,1],
                           vars[2,2],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[1]) * exp(model_out$coefficients[2])
  
  cat(unname(noquote(paste("Odds",
                           vars[1,2],
                           vars[2,1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[1]) * exp(model_out$coefficients[2]) *
    exp(model_out$coefficients[3]) * exp(model_out$coefficients[4])
  
  cat(unname(noquote(paste("Odds",
                           vars[1,2],
                           vars[2,2],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[2])
  
  cat(unname(noquote(paste("Odds Ratio",
                           vars[1,2],
                           "to",
                           vars[1,1],
                           "with",
                           vars[2,1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[2]) * exp(model_out$coefficients[4])
  
  cat(unname(noquote(paste("Odds Ratio",
                           vars[1,2],
                           "to",
                           vars[1,1],
                           "with",
                           vars[2,2],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[3])
  
  cat(unname(noquote(paste("Odds Ratio",
                           vars[2,2],
                           "to",
                           vars[2,1],
                           "with",
                           vars[1,1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[3]) * exp(model_out$coefficients[4])
  
  cat(unname(noquote(paste("Odds Ratio",
                           vars[2,2],
                           "to",
                           vars[2,1],
                           "with",
                           vars[1,2],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[4])
  
  cat("\n")
  cat(unname(noquote(paste("Odds Ratio",
                           vars[1,2],
                           "to",
                           vars[1,1],
                           "with",
                           vars[2,2],
                           "to\n",
                           "Odds Ratio",
                           vars[1,2],
                           "to",
                           vars[1,1],
                           "with",
                           vars[2,1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
  
  odds <- exp(model_out$coefficients[4])
  
  cat("\n")
  cat(unname(noquote(paste("Odds Ratio",
                           vars[2,2],
                           "to",
                           vars[2,1],
                           "with",
                           vars[1,2],
                           "to\n",
                           "Odds Ratio",
                           vars[2,2],
                           "to",
                           vars[2,1],
                           "with",
                           vars[1,1],
                           "=",
                           format(round(odds, 2), nsmall = 2)))),
      "\n")
}
