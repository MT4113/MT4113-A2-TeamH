source("Scripts/calcLikelihood.R")

### PLEASE DO NOT TEST THIS YET, UNDER CONSTRUCTION, WONT WORK
teamEM <- function(data, epsilon = 1e-08, maxit = 1000){
  # Data - dataframe of observations 
  # epsilon - itteration stopping point
  # maxit - max number of iterations
  

  # Input checks ------------------------------------------------------------
  # Need to check data is a dataframe of sufficent lengths and conditions 
  # Need checks for epsilon and maxit

  
  # Initalize the Data ------------------------------------------------------
  # Need value for k_numb which is the number of factors in K
  k_numb <- NA

  # Loop --------------------------------------------------------------------

  l2 = 0
  l1 = NA
  
  #Denote l2 as previous likelihood, l1 as newest likelihood
  while((abs(l2-l1) > epsilon) & (maxit >= 0) ){
    
    # Reassign exit conditions 
    maxit <- maxit - 1 
    l2 <- l1
    
    #Probailty calculations 
    df <- prob_expectations(df, k_table, k_numb)
    k_table <- max_Ests(df)
    
    # Likelihood calculation
    l1 <- likelihood()
  }
  
  converged <- ifelse(abs(l2-l1) > epsilon, FALSE, TRUE)
  
  # Return conditions 
  output <- list(estimates = k_table, 
                inits = k_init, 
                converged = converged,
                posterior = df,
                likelihood = l1)
  return(output)
}
