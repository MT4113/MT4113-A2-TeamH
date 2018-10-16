load("FishLengths.RData") 
teamEM <- function(data, epsilon = 1e-08, maxit = 1000, 
                   inc_known_k_init = FALSE, inc_known_k_iter = FALSE,
                   inc_known_as_unknown_init = FALSE, 
                   inc_known_as_unknown_iter = FALSE){
  
  source("functions.R", local = TRUE)
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). NA if unknown. k > 0  
  # epsilon:  convergence criterion
  # maxit: max iterations
  # inc_known_k_init: True if including the values of known age classes into the 
  #                   Initalization of mu, sigma, and lamda. Default is False 
  # inc_known_k_iter: True if including values of known age classes into the 
  #                   maximization of mu, sigma, and lamda. Default is False 
  # inc_known_as_unknown_iter: Includes the known values in the calculation of the 
  #                       posterior as unknown values in the init step
  #                       Will not work if inc_known_k_iter is 
  #                       also TRUE (this is due 
  #                       to double adding)
  # inc_known_as_unknown_init: Includes the known values in the calculation of the 
  #                       posterior as unknown values. Will not work if 
  #                       inc_known_k_init are also 
  #                       TRUE (this is due to double adding)
  # Error Checking ----------------------------------------------------------
  #Need something to ensure that inputs are correct
  #Also need to ensure ages are != -1 and are positive
  #Also need to ensure columns are named aptly, as we call by NAME
  
  source("ErrorChecks.R", local = TRUE)
  
  #data, dataframe with columns Age, and Lengths and approriately named
    #in Lengths, no null values
    #in Age, nonnull values are all >- 0. Cannot be all null value
      #Must be one unique non-null age class
  
  #Numeric Check
  if (!(numeric_Check(epsilon, F, T))){stop("Invalid arguments for epsilon")}
  if (!(numeric_Check(maxit, T, T))){stop("Invalid arguments for maxit")}
  
  #Boolean Checks
  if(!(boolean_check(inc_known_k_iter))){stop("Invalid arguments for inc_known_k_iter")}
  if(!(boolean_check(inc_known_k_init))){stop("Invalid arguments for inc_known_k_init")}
  if(!(boolean_check(inc_known_as_unknown_iter))){stop("Invalid arguments for inc_known_as_unknown_iter")}
  if(!(boolean_check(inc_known_as_unknown_init))){stop("Invalid arguments for inc_known_as_unknown_init")}
  # Data initalization ------------------------------------------------------
  
  #Sets Unknown ages to age 0. 
  data$Age[is.na(data$Age)] <- -1
  data$Age <- as.factor(data$Age)
  
  #Seperate known and unknown data
  unknown_dat <- data[data$Age == -1,] #this can be moved up and optimized earlier since it can be used
  known_dat <- data[data$Age != -1,]
  
  #Gets all the unique age catagories found in the dataset 
  uniq_ages <- unique(data$Age)
  uniq_ages <-uniq_ages[order(uniq_ages)]
  uniq_ages <- uniq_ages[uniq_ages != -1]
  k_numb <- length(uniq_ages) #Gives total numb of k values not including -1 
  
  #input - Dataframe data
  #Out - matrix containing the estimates of stdev, mu and lambida
  k_mat <- init_data_ests(data, uniq_ages)
  
  unknown_dat_case <- unknown_dat
  if(inc_known_as_unknown_init){unknown_dat_case <- rbind(unknown_dat_case, known_dat)}
  
  #input - dataframe data
  #output - lamda values 
  k_mat <- init_prob_ests(k_mat,k_numb, inc_known_k_init, 
                          unknown_dat_case ,known_dat, uniq_ages) #FOR TESTING ITS COMMENTED OUT
  k_mat_init <- k_mat

  
  # Loop --------------------------------------------------------------------
  
  maxit_Total <- maxit + 1
  ll_vec <- rep(NA, maxit_Total+1)
  maxit <- 2
  ll_vec[maxit] <- likelihood(unknown_dat, k_mat, k_numb)
  ll_vec[maxit - 1] <- ll_vec[maxit] + (2*epsilon)
  
  if(inc_known_as_unknown_iter){unknown_dat <- rbind(unknown_dat,known_dat)}

  while((abs(ll_vec[maxit]-ll_vec[maxit-1]) > epsilon) & maxit <= maxit_Total){
    # Reassign exit conditions 
    maxit <- maxit + 1 

    #input - the dataframe data 
    #output - dataframe of col1 - ID col2 - lengths, col3 onwards, one column for each inital probabilty labeled X#
    prob_table <- prob_ests(unknown_dat, k_mat, k_numb)
    
    #input - probabilty table and k_mat and K_numb
    #output, a new k_mat updated
    #For some reason i now fully trust this function with my 3rd nonexistent child  
    k_mat <- max_ests(prob_table, k_mat, k_numb, inc_known_k_iter, known_dat)
    #print(system.time(prob_ests(unknown_dat, k_mat, k_numb)))
    
    #Input - 
    #Output - values for likelihood in l2, this is LOG likelihood
    #THIS IS WHERE THE TIMING ERROR IS LOCATED
    ll_vec[maxit] <- likelihood(unknown_dat, k_mat, k_numb)

  }  
  
  
  converged <- ifelse(abs(ll_vec[maxit]-ll_vec[maxit-1]) > epsilon, FALSE, TRUE)
  
  # Return conditions 
  output <- list(estimates = k_mat, 
                 inits = k_mat_init, 
                 converged = converged,
                 posterior = head(prob_table), #This table needs to be cleaned
                 likelihood = ll_vec[c(3:maxit)]) 
  return(output)
}
