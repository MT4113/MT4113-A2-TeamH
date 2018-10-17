load("FishLengths.RData") 
teamEM <- function(data, epsilon = 1e-08, maxit = 1000, 
                   inc_known_k_init = FALSE, inc_known_k_iter = FALSE,
                   inc_known_as_unknown_init = FALSE, 
                   inc_known_as_unknown_iter = FALSE, 
                   roundAge = FALSE){
  
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
  # roundAge: Rounds age to nearest whole number if True. Default False
  # Error Checking ----------------------------------------------------------
  #Need something to ensure that inputs are correct
  #Also need to ensure ages are != -1 and are positive
  #Also need to ensure columns are named aptly, as we call by NAME
  
  source("ErrorChecks.R", local = TRUE)
  
  #data, dataframe with columns Age, and Lengths and approriately named
    #in Lengths, no null values
    #in Age, nonnull values are all >- 0. Cannot be all null value
      #Must be one unique non-null age class
  if(!df_check(data)){stop("Invalid dataset for data")}
  
  #Numeric Check
  if (!(numeric_Check(epsilon, F, T))){stop("Invalid arguments for epsilon")}
  if (!(numeric_Check(maxit, T, T))){stop("Invalid arguments for maxit")}
  
  #Boolean Checks
  if(!(boolean_check(inc_known_k_iter))){stop("Invalid arguments for inc_known_k_iter")}
  if(!(boolean_check(inc_known_k_init))){stop("Invalid arguments for inc_known_k_init")}
  if(!(boolean_check(inc_known_as_unknown_iter))){stop("Invalid arguments for inc_known_as_unknown_iter")}
  if(!(boolean_check(inc_known_as_unknown_init))){stop("Invalid arguments for inc_known_as_unknown_init")}
  
  #Checking to ensure that we are not including both known as known and unknown 
  if(inc_known_as_unknown_init & inc_known_k_init){
    stop("inc_known_as_unknown_init and inc_known_k_init are both True. Only one can be True.")
  }
  if(inc_known_as_unknown_iter & inc_known_k_iter){
    stop("inc_known_as_unknown_iter and inc_known_k_iter are both True. Only one can be True.")
  }
  
  # Data initalization ------------------------------------------------------
  
  #Sets Unknown ages to age -1. 
  data$Age[is.na(data$Age)] <- -1
  
  if(roundAge){data$age <- round(data$Age)} #Rounds ages if it should be rounded
  data$Age <- as.factor(data$Age) #preprocessing, grouping ages as a factor 
  
  data <- data.frame(Length = data$Length, Age = data$Age) #Removes any uneeded columns for memory issues

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
  
  if(length(k_mat$sigma[is.na(k_mat$sigma)]) > 0){stop("Insufficent known ages for initialization")}
  
  unknown_dat_case <- unknown_dat
  if(inc_known_as_unknown_init){unknown_dat_case <- rbind(known_dat, unknown_dat_case)}
  
  #input - dataframe data
  #output - lamda values 
  k_mat2 <- init_prob_ests(k_mat,k_numb, inc_known_k_init, 
                          unknown_dat_case ,known_dat, uniq_ages) #FOR TESTING ITS COMMENTED OUT
  k_mat_init <- k_mat2
  
  if(length(k_mat2$sigma[is.na(k_mat2$sigma)]) > 0){#insufficent estimates to get sigma inital estimate 
    k_mat2$sigma[is.na(k_mat2$sigma)] <- k_mat$sigma[is.na(k_mat2$sigma)]
  }

  
  # Loop --------------------------------------------------------------------
  
  maxit_Total <- maxit + 1
  ll_vec <- rep(NA, maxit_Total+1)
  maxit <- 2
  ll_vec[maxit] <- likelihood(unknown_dat, k_mat, k_numb)
  ll_vec[maxit - 1] <- ll_vec[maxit] + (2*epsilon)
  
  if(is.infinite(ll_vec[maxit])){#This is in case there is a numerical underflow 
    #problem from too few samples and ranges of data that are too large 
    stop("Likelihood estimates cannot be determined")}
  
  if(inc_known_as_unknown_iter){unknown_dat <- rbind(known_dat, unknown_dat)}

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
  
  # Formatting the outptu of the posterior
  prob_out <- prob_table[,c(-1,-2), drop = F]
  
  #Case where knowns are not used in the iteration step,
  if(inc_known_as_unknown_iter == FALSE){
    #Gets the posteriors for all KNOWN values (permitation of 1,0,0,...,0)
    known_post_prob <- sapply(uniq_ages, add_binary_cols, dat = known_dat$Age)
    known_lengths <- data.frame(known_post_prob)
    prob_out <- rbind(known_lengths, prob_out)
  }

  colnames(prob_out) <- uniq_ages
  rownames(prob_out) <- c() #Gets rid of row names, so not 100% sure if needed, but may be useful
  
  # Return conditions 
  output <- list(estimates = k_mat, 
                 inits = k_mat_init, 
                 converged = converged,
                 posterior = (prob_out), #To be fixed
                 likelihood = ll_vec[c(3:maxit)]) 
  return(output)
}
