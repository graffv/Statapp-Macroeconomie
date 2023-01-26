library(MASS)
library(haven)
library(ivreg)
#install.packages("ivreg", dependencies = TRUE)

data = read.csv("/Users/victorgraff/Documents/2022:2023 ENSAE/Cours/Statapps/data_StatApp/data_FR_enhanced_dummies.csv")
data_period1 = subset(data, period==1)
data_periodNot2 = subset(data, period!=2)
data_periodNot3 = subset(data, period!=3)
data_period4 = subset(data, period==4)

make_regression <- function(trade, df){
  regressand <- c("dy", "di", "de", "du", "ty", "ti", "te", "tu", "hy", "hi", "he", "hu", "sy", "si", "se", "su", "oy", "oi", "oe", "ou")
  # We store the values of the coefficients of the lr in the associated lists below
  hash_regressand1 <- c()
  hash_regressand2 <- c()
  hash_std_lm <- c()

  hash_regressand_iv1 <- c()
  hash_regressand_iv2 <- c()

  for (regres in regressand){
    # For each regressand, linear regression on the trade data selected (wt, lwt, lwm, lwx)
    model_regres <- lm(formula <- df[regres][[1]] ~ df[trade][[1]])
    # Same for the IV
    model_iv_complete <- ivreg(df[regres][[1]] ~ df[trade][[1]] | distance + adjacent + ling, data=data)

    
    # Store all values in lists
    hash_regressand1[regres] <- c(model_regres$coefficients[1])
    hash_regressand2[regres] <- c(model_regres$coefficients[2])
    
    hash_regressand_iv1[regres] <- c(model_iv_complete$coefficients[1])
    hash_regressand_iv2[regres] <- c(model_iv_complete$coefficients[2])
  }
  # Creating matrix of lists to gather all coefficients

  combi <- matrix(c(hash_regressand1, hash_regressand2, hash_regressand_iv1, hash_regressand_iv2), ncol=4, nrow=20)
  return(combi)
}

make_regression("lwt", data)
