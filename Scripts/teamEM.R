source("Scripts/calcLikelihood.R")

### PLEASE DO NOT TEST THIS YET, UNDER CONSTRUCTION, WONT WORK
teamEM <- function(data, epsilon = 1e-08, maxit = 1000){
  # Data - dataframe of observations. Must be define as the following format: 
  #       Col 1: lengths of fishes
  #       Col 2: Catagorical variable clasfification (age class k)
  #       Null is there is no classification
  # epsilon - itteration stopping point
  # maxit - max number of iterations
  
  #Design note, will best want to specify which columns and rows are used for observations and for k 

  # Input checks ------------------------------------------------------------
  # Need to check data is a dataframe of sufficent lengths and conditions 
  # Need checks for epsilon and maxit

  
  # Initalize the Data ------------------------------------------------------
  # Need value for k_numb which is the number of factors in K
  k_numb <- 3 #length(unique(data[,2]))-1 #Numeric values + NA
  k_table <- k.estimates(data,1)
  k_table$mu <- c(1,100,1000)
  k_table$sigma <- c(1,10,100)
  
  #Need to intialize df, to be completed. This is a temp one ot test 
  df <- data.frame("length" = c(1:1080), "p1" = rep(0.1,1080), "p2" = rep(0.2,1080), "p3" = rep(0.3,1080))
  
  print(k_table) #this is for testin gpurposes
  
  # Loop --------------------------------------------------------------------

  l2 = 0
  l1 = 10
  
  #Denote l2 as previous likelihood, l1 as newest likelihood
  while(abs(l2-l1) > epsilon & (maxit > 0) ){
    
    # Reassign exit conditions 
    maxit <- maxit - 1 
    l2 <- l1
    
    #Probailty calculations 
    df <- prob_expectations(df, k_table, k_numb)
    k_table <- max_Ests(df)

    # Likelihood calculation
    l1 <- Likelihood_Eval(data, k_table)
  }
  
  converged <- ifelse(abs(l2-l1) > epsilon, FALSE, TRUE)
  
  # Return conditions 
  output <- list(estimates = k_table, 
                inits = NA, 
                converged = converged,
                posterior = NA, #df,
                likelihood = l1)
  return(output)
}
