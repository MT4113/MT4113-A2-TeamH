teamEM <- function(data, epsilon = 1e-08, maxit = 1000, 
                   inc_known_init = FALSE, inc_known_iter = FALSE,
                   inc_known_as_unknown_init = FALSE, 
                   inc_known_as_unknown_iter = FALSE, 
                   inc_known_in_ll = FALSE,
                   roundAge = FALSE){
  
  
  ###########################################################################
  # The purpose of this function is to implement the Expectation 
  # Maximization Algorithm described in the paper Do, C.B. and S. Batzoglou. 
  # 2008. What is the expectation maximization algorithm? Nature Biotechnology 
  # 26:897-899.
  # 
  # Arguments:
  # 
  #   data - Dataframe consisting of at least two columns named Age and Lengths.
  #          Column Length must contain numeric data with no null values in the 
  #          dataframe. Column Age must contain either positive numeric values 
  #          for known ages or NA for unknown ages.
  #   epsilon - A tolerance value to determine convergence of the expectation 
  #             maximization algorithm. Value must be a positive numeric value. 
  #             Default is 1e-08.
  #   inc_known_init - Boolean value when set to TRUE includes known fish 
  #                      ages in the initialization step of the algorithm when 
  #                      estimating parameters mu, sigma and lambda. Default is 
  #                      FALSE
  #   inc_known_iter - Boolean value when set to TRUE includes known fish ages
  #                      in the iteration and maximization step algorithm when 
  #                      estimating parameters mu, sigma and lambda. 
  #                      Default is FALSE
  #   inc_known_as_unknown_init - Boolean value when set to TRUE includes known 
  #                               fish ages in the initialization step of the 
  #                               algorithm when estimating parameters mu, sigma
  #                               and lambda. Values are treated as unknown 
  #                               values, and have probabilities estimated by 
  #                               the algorithm. Default is FALSE
  #   inc_known_as_unknown_iter - Boolean value when set to TRUE includes known 
  #                               fish ages in the iteration and maximization 
  #                               step of the algorithm when estimating  
  #                               parameters mu, sigma and lambda. Values are  
  #                               treated as unknown values, and have  
  #                               probabilities estimated by the algorithm.
  #                               Default is FALSE
  #   inc_known_in_ll - Boolean value when set to TRUE includes the values of 
  #                     known fish ages in the calculation of the log likelihood 
  #                     based on parameters estimated by the maximization 
  #                     function. Default is FALSE
  #   roundAge - Boolean value when set to TRUE rounds the value of Ages to the 
  #              nearest whole number during initialization. Can be used if  
  #              exact ages are specified in data, but discrete age categories 
  #              are to be used. Default is FALSE
  # 
  # Outputs: 
  #   estimates - Estimates of mu, sigma, and lambda from the expectation 
  #               maximization algorithm output in a dataframe where each age 
  #               category is represented by a row, and mu, sigma and lambda 
  #               estimates are represented by the columns.
  #   inits - initial estimates of mu, sigma, and lambda output in a dataframe
  #           where each age category is represented by a row, and mu, sigma 
  #           and lambda estimates are represented by the columns.
  #   converged - Boolean value indicating if the algorithm converged given 
  #               epsilon and maxit values
  #   posterior - List of posterior probabilities calculated based on 
  #               implementation used during the iteration step (the value of 
  #               inc_known_as_unknown_iter). Probabilities are listed first by 
  #               known and then by unknown values of the Age column from data 
  #               in the order the values appear in the dataframe. For values 
  #               with given age classes, they are assigned the posterior 
  #               probability value of 1 in the age class they are from and 0 
  #               otherwise.
  #   likelihood - Vector of log likelihood results for each iteration of the 
  #                expectation maximization algorithm. The first element is the 
  #                first log likelihood result and the last element is the log 
  #                likelihood at convergence.
  # 
  # Implementation Notes:
  #   * The output for posterior is ordered first by all known Age classes then 
  #     by all unknown age classes. Within these groupings the output is sorted 
  #     by the order of occurance. So the output would be ordered as 
  #     (Known Obvs 1, Known Obvs 2 ... Known Obvs n, Unknown Obvs 1, 
  #       Unknown Obvs 1 ... Unknown Obvs m). This ordering is for 
  #     inc_known_as_unknown_iter = FALSE as known data is treated as 
  #     unknown data when inc_known_as_unknown_iter = TRUE
  #   * The columns for estimates and inits are numbed based on the value for  
  #     the age category. 
  #   * If a sigma estimate cannot be obtained based on the known data, it is 
  #     set to 10 and a warning is produced.
  #   * If a sigma estimate cannot be obtained based on the first guesses to the 
  #     age category based on first estimates from known values, then the value 
  #     for sigma taken from the first initial estimate is used instead and a 
  #     warning is produced. 
    
  
  # Source all the functions located in functions.R
  source("functions.R", local = TRUE)
 
  # Error Checking ----------------------------------------------------------
  # Source all the functions located in ErrorChecks.R
  source("ErrorChecks.R", local = TRUE)
  
  # Dataframe Check (ensures inputs are valid)
  if(!df_check(data)){stop("Invalid dataset for data")}
  
  # Numeric Check (ensures inputs are valid)
  if (!(numeric_Check(epsilon, F, T))){stop("Invalid arguments for epsilon")}
  if (!(numeric_Check(maxit, T, T))){stop("Invalid arguments for maxit")}
  
  # Boolean Check (ensures inputs are valid)
  if(!(boolean_check(inc_known_iter))){
    stop("Invalid arguments for inc_known_iter")
    }
  if(!(boolean_check(inc_known_init))){
    stop("Invalid arguments for inc_known_init")
    }
  if(!(boolean_check(inc_known_as_unknown_iter))){
    stop("Invalid arguments for inc_known_as_unknown_iter")
    }
  if(!(boolean_check(inc_known_as_unknown_init))){
    stop("Invalid arguments for inc_known_as_unknown_init")}
  
  # Checks to ensure there are no conflicts including known values as both 
  # known and unknown values in calculations
  if(inc_known_as_unknown_init & inc_known_init){
    stop(paste("inc_known_as_unknown_init and inc_known_init are both TRUE",
               "Only one can be TRUE"))
  }
  if(inc_known_as_unknown_iter & inc_known_iter){
    stop(paste("inc_known_as_unknown_iter and inc_known_iter are both TRUE",
               "Only one can be TRUE"))
  }
  if(inc_known_in_ll & inc_known_as_unknown_iter){
    stop(paste("inc_known_in_ll and inc_known_as_unknown_iter are both TRUE",
               "only one can be TRUE"))
  }
  
  # Data initalization ------------------------------------------------------
  
  # Sets Unknown ages from data to equal -1. 
  data$Age[is.na(data$Age)] <- -1
  
  #Round ages if ages based on roundAge
  if(roundAge){data$age <- round(data$Age)} 
  
  # Preprocessing, grouping ages as a factor
  data$Age <- as.factor(data$Age) 
  
  #Removes any uneeded columns to reduce possible RAM burden 
  data <- data.frame(Length = data$Length, Age = data$Age) 

  #Seperate known and unknown data into two variables
  unknown_dat <- data[data$Age == -1,] 
  known_dat <- data[data$Age != -1,]
  
  # Gets all the unique age catagories found in the dataset 
  uniq_ages <- unique(data$Age)
  uniq_ages <-uniq_ages[order(uniq_ages)]
  uniq_ages <- uniq_ages[uniq_ages != -1]
  
  # Determine the number of age catagories in the dataset
  k_numb <- length(uniq_ages) 
  

  # Obtain intial estimates for mu, sigma and lambda from known data
  k_mat <- init_data_ests(data, uniq_ages)
  
  # Check case for if sigma cannot be determined due to insufficent 
  # inital parameters (ex, only one known observation for Age = 1)
  if(length(k_mat$sigma[is.na(k_mat$sigma)]) > 0){ #If sigma cant be determined
    k_mat$sigma[is.na(k_mat$sigma)] <- 10
    warning(paste("Insuffient inital observations to generate an estimate for" ,
                  "all values of sigma.\n  Undertermined values of Sigma are", 
                  "set to 10 during initalization."))
  }

  # Check case for if known data needs to be included due to the value of 
  # inc_known_as_unknown_init
  unknown_dat_case <- unknown_dat
  if(inc_known_as_unknown_init){unknown_dat_case <- rbind(known_dat, 
                                                          unknown_dat_case)}
  
  # Recalculates estimates for mu, sigma and lambda based on inital guesses of
  # the age of unknown data. Assigned to a new variable in case we need to 
  # revert back to previous estimates
  k_mat2 <- init_prob_ests(k_mat,k_numb, inc_known_init, 
                          unknown_dat_case ,known_dat, uniq_ages) 
  k_mat_init <- k_mat2

  # Check case if sigma cannot be determined due to insufficent estimates 
  if(length(k_mat2$sigma[is.na(k_mat2$sigma)]) > 0){
    
    # NA values are replaced with values determined from previous estimates
    k_mat2$sigma[is.na(k_mat2$sigma)] <- k_mat$sigma[is.na(k_mat2$sigma)]
    
    #Generated warning
    warning(paste("Insuffient estimated observations to generate an estimate ",  
              "for sigma.\n  Undertermined values of Sigma are set to ", 
              "initally calculated estimates shown in $init"))
  }

  
  # Loop --------------------------------------------------------------------
  
  # Initalizes the vector to store the values of calculated log likelihoods
  # and intializes values for the iterative part of the algorithim
  maxit_Total <- maxit + 1 #Sets endpoint when to terminate loop
  ll_vec <- rep(NA, maxit_Total+1) #Generates a log likelihood vector
  maxit <- 2 #sets intial starting point
  
  # Initalizes the 2nd value of the log likelihood vector
  ll_vec[maxit] <- likelihood(unknown_dat, k_mat, k_numb)
  
  # Case when inc_known_in_ll is TRUE
  if(inc_known_in_ll){
    ll_vec[maxit] <- ll_vec[maxit] + likelihood(known_dat, k_mat, k_numb)
  }
  
  # Initalizes first value of the log likelihood vector to ensure that  
  # the iteration beings 
  ll_vec[maxit - 1] <- ll_vec[maxit] + (2*epsilon)
  
  # Check case in case there is a numerical overflow due to inital estimates
  # generating likelihoods that are too unlikely, thus causing a -Inf
  # to be generated for the sum. Terminates the program if it occurs
  if(is.infinite(ll_vec[maxit])){
    stop("Numerical overflow - Likelihood estimates cannot be determined")}
  
  # Check case for when inc_known_as_unknown_iter and values needed to be added
  # for the iteration step
  if(inc_known_as_unknown_iter){unknown_dat <- rbind(known_dat, unknown_dat)}

  
  # Iteration of the expectation maximization algorithim 
  while((abs(ll_vec[maxit]-ll_vec[maxit-1]) > epsilon) & maxit <= maxit_Total){
    
    # Reassign exit conditions 
    maxit <- maxit + 1 

    # Calculate posterior probabilites
    prob_table <- prob_ests(unknown_dat, k_mat, k_numb)
    
    # Calculate new parameter estimates
    k_mat <- max_ests(prob_table, k_mat, k_numb, inc_known_iter, known_dat)

    # Calculate new likelihood 
    ll_vec[maxit] <- likelihood(unknown_dat, k_mat, k_numb)
    if(inc_known_in_ll){ll_vec[maxit] <- likelihood(known_dat, k_mat, k_numb)}

  }  
  
  #Test for convergence 
  converged <- ifelse(abs(ll_vec[maxit]-ll_vec[maxit-1]) > epsilon, FALSE, TRUE)
  
  # Formatting the output of the posterior
  prob_out <- prob_table[,c(-1,-2), drop = F]
  
  #Corner case that throws an error for some reason only for a single value
  if (k_numb == 1){ 
    prob_out <- data.frame("X1" = rep(1, length(data$Length)))
  } else {
    prob_out <- prob_table[,c(-1,-2), drop = F]
  }
  
  # Case where posterior knowns are not used in the iteration step,
  if(inc_known_as_unknown_iter == FALSE & (k_numb != 1)){
    
    #Gets the posteriors for all KNOWN values (permitation of 1,0,0,...,0)
    known_post_prob <- sapply(uniq_ages, add_binary_cols, dat = known_dat$Age)
    known_lengths <- data.frame(known_post_prob)
    
    # Assigns to the top of the posterior probabilites
    prob_out <- rbind(known_lengths, prob_out)
  } 
  
  # Formatting col/row names
  colnames(prob_out) <- uniq_ages
  rownames(prob_out) <- c() 
  rownames(k_mat) <- uniq_ages
  rownames(k_mat_init) <- uniq_ages
  
  # Return values 
  output <- list(estimates = k_mat, 
                 inits = k_mat_init, 
                 converged = converged,
                 posterior = (prob_out), 
                 likelihood = ll_vec[c(3:maxit)]) 
  return(output)
}
