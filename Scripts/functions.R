
init_data_ests <- function(data, ages){
  # The purpose of this function is to generate initial estimates of mu, sigma 
  # and lambda for each age class based on observations of known age class.
  # 
  # Arguments:
  #   data - Dataframe containing the Age and Length of known observations based 
  #           on the required datafame.
  #   ages - vector containing all the unique ages in the dataset
  # 
  # Output:
  #   A table containing initial estimates of mu, sigma and lambda for 
  # each age class.
  
  # Count of known values
  N <- length(data$Length[(data$Age != -1)]) 
  
  # Generates inital eestimates for each age group
  init_ests <- mapply(
    function(i, data){
      return(c(
        mean(data$Length[(data$Age == i)]), 
        sd(data$Length[(data$Age == i)]),
        length(data$Length[(data$Age == i)])
      ))
    }, ages, MoreArgs = list(data = data))
  
  return(data.frame("mu" = init_ests[1,], "sigma" = init_ests[2,], "lambda" = init_ests[3,]/N))
  
}

init_prob_ests <- function(k_table, k_numb, flag, 
                           unknown_dat, known_dat, ages){
  # The purpose of this function is to determine initial probability estimates 
  # for elements with unknown age classes (or if inc_known_init is TRUE, also 
  # elements with known age classes) which are then used to generate estimates 
  # of mu, sigma and lambda for each age class.
  # 
  # Arguments:
  #   k_table - Initial estimates of mu, sigma and lambda based on known data 
  #             from init_data_ests
  #   k_numb - Number of age classes present in the original dataset
  #   flag -  Boolean value of inc_known_init to determine if elements with
  #           known age classes should also be estimated and then used to  
  #           compute estimates of mu, sigma and lambda
  #   unknown_dat - Dataframe containing Age and Lengths of all observations 
  #                 that are of unknown age class from input
  #   known_dat - Dataframe containing Age and Lengths of all observations 
  #               that have a known age from input
  #   ages - unique list of all age classes in the dataset.
  #   
  # Outputs:
  #   An updated table of estimates of mu, sigma and lambda for each age class
  
  # Based on mu, sigma and lambda, assign probabilites to each length
  p_mat <- mapply(function(mu, sigma, dat){
    return(dnorm(dat, mu, sigma))
  },k_table$mu, k_table$sigma ,MoreArgs = list(dat = unknown_dat$Length))
  
  # Find most suitable age class for each probabilty value 
  unknown_dat$Age <- apply(p_mat, 1, function(x){ages[which.max(x)]})
  
  # Combines estimated ages and actual known ages (if needed)
  allAge_dat <- unknown_dat
  if(flag){allAge_dat <- rbind(allAge_dat, known_dat)}
  
  # Total observations in the dataset
  N <- length(allAge_dat$Age)
  
  # Generate estimates for mu, sigma and lambda 
  init_ests <- mapply(
    function(i, data){
      return(c(
        mean(data$Length[data$Age == i]), 
        sd(data$Length[data$Age == i]),
        length(data$Age[data$Age == i]))
      )
    }, ages, MoreArgs = list(data = allAge_dat))

  # Return calculated estimates
  return(data.frame("mu" = init_ests[1,], "sigma" = init_ests[2,], 
                    "lambda" = init_ests[3,]/N))
  
}

prob_ests <- function(data, k_table, k_numb){
  # The purpose of this function is to generate posterior probabilities for 
  # each observation and age class.
  # 
  # Arguments:
  #   data - Dataframe containing the columns Age and Length
  #   k_table - Table containing estimates of mu, sigma and lambda for each 
  #             age class in the dataset.
  #   k_numb - Number of age classes in the original initialized dataframe 
  #   
  # Output:
  #   A Dataframe containing the Age and Length of each observation as well as 
  #   the calculated posterior probabilities for each observation for each age 
  #   class. One column for each age class
  
  # Total number of unknown ages
  N <- length(data$Age) 
  
  # Compute numerator of posteior probabilites
  p_mat <- mapply(function(mu, sigma, data, lambda){
                  return(dnorm(data, mu, sigma) * lambda)},
                  k_table$mu, k_table$sigma, k_table$lambda,
                  MoreArgs = list(data=data$Length))
  
  # Compute denominator which standardizes the probabilities for each row 
  denomSums <- rowSums(p_mat)
  
  # Calculate posteior probabilites 
  if (k_numb == 1){
    p_mat <- p_mat/denomSums #Unique case for k_numb == 1. Gives error otherwise 
  } else {
    p_mat <- apply(p_mat, 2, function(x,ds){return(x/ds)}, ds = denomSums) 
  }
  
  # Return all posterior probabilites calculated in correct format  
  output <- data.frame(data[,c(1:2)], p_mat)
  return(output)
}

add_binary_cols <- function(x, dat){
  # The purpose of this function is to generate an array of vectors of 
  # posterior probabilities for observations with already known age classes. 
  # Probabilities are 1 for the age class the observation is known under and 
  # 0 otherwise
  # 
  # Arguments:
  #   x - value of age class being tested
  #   dat - vector of ages based on the dataset
  # 
  # Output:
  #   Matrix of Probabilities that are 1 for the age class the observation is 
  #   known under and 0 otherwise
  # 
  # Implementaiton notes:
  #   To be called only in an apply function or similar passing through data. 
  
  return(ifelse(x == dat, 1, 0))
}

max_ests <- function(prob_table, k_table, k_numb, flag, known_dat){
  # The purpose of this function is to generate estimates for mu, sigma and 
  # lambda in the iterative portion of the expectation maximization algorithm.
  # 
  # Arguments:
  #   prob_table - Table of probabilities generated from prob_ests .
  #   k_table - table containing estimates of mu, sigma and lambda for each age 
  #             class in the dataset.
  #   k_numb - number of age classes in the original dataframe initialized
  #   flag - Boolean True/False flag based on inc_known_iter , which indicates 
  #          if known values should be included when calculating estimates for 
  #          mean, sigma, and lambda.
  #   known_dat - dataframe containing the _Age_ and Length of observations of a 
  #               known age class.
  # 
  # Output:
  #   A table containing estimates for mean, sigma, and lambda for each age  
  #   class in the dataset.
  
  # Simplify datasets to only probabilites and only Lengths
  probs <- prob_table[,c(-1,-2)]  
  lengths <- prob_table$Length 
  
  # Check case for inc_known_iter, add lengths of observations of known age to 
  # lengths and posterior probabilites to probs
  if(flag){   
    # Generate matrix to store values
    prob_table_known <- matrix(rep(0,length(known_dat$Age) * k_numb),
                               ncol = k_numb)
    
    # Get unique age catagories
    unique_Ages <- unique(known_dat$Age)
    unique_Ages <- unique_Ages[order(unique_Ages)]
    
    # Obtain posterior probabilites for known data 
    prob_table_known <- sapply(unique_Ages, add_binary_cols, 
                               dat = known_dat$Age)
    # Format and add posterior probabilites for known data to probs
    format <- data.frame(prob_table_known)
    probs <- rbind(format, probs)
    
    # Add lengths for known data to probs 
    lengths <- c(known_dat$Length, lengths)
  }
  
  # Get total length of unknowns
  N <- length(lengths)
  
  #Fixes a problem wiht k_numb == 1
  if(k_numb == 1){
    tmp <- data.frame("probs" = probs ,"x2" = probs)
    probs <- tmp[,1, drop = F]
  }
  
  #sums of all probs based on age class
  baseProbSum<- colSums(probs) 
  
  #Mean Calculations
  numerMeans <- apply(probs, 2, function(x,y){return(sum(x*y))}, y = lengths)
  k_table$mu <- numerMeans/baseProbSum
  
  
  #Sd Calculations
  k_table$sigma <- mapply(function(prob, mu, baseProbSumVal, lengths){
                  return(sqrt(sum( (prob * ((lengths-mu)^2) / baseProbSumVal))))
                  },probs, k_table$mu, baseProbSum, 
                  MoreArgs = list(lengths = lengths))
  
  #Lambida Calculations 
  k_table$lambda <- baseProbSum/N
  
  #Return Calculation results
  return(k_table)
}

likelihood <- function(unknown_dat, k_table, k_numb){
  # The purpose of this function is to calculate the log likelihood based on 
  # estimated values for mean, sigma, and lambda.
  # 
  # Arguments:
  #   unknown_dat - Table of probabilities produced from prob_ests
  #   k_table - table containing estimates of mu, sigma and lambda for each 
  #             age class in the dataset.
  #   k_numb - number of age classes in the original dataframe initialized
  # Output:
  #   A single value of the log likelihood. 
  
  # Calcuale the likelihood values for each individual unknown length  
  # observation based on estmiates for mu, sigma and lambda
  init <- 0 
  init <- apply(k_table, 1, normalFunc, x = unknown_dat$Length)
  
  # Take likelihood for each element and convert to log likelihoods
  temp <- rowSums(init)
  temp <- log(temp)
  
  #Return overall log likelihood 
  return(sum(temp))
}

normalFunc <- function(k_table, x){
  # The purpose of this function is to return a vector of values for the log 
  # likelihood estimates based on estimated values for mean, sigma, and lambda. 
  # Used in likelihood() to generate the likelihood values before summation.
  # 
  # Arguments:
  #   k_table - vector containing mu, sigma and lambda estimates in the  
  #             specified order
  #   x - vector containing all lengths used to calculate the log likelihood
  # 
  # Outputs:
  #   Vector of results containing the log likelihood (not summed) for all the 
  #   elements given values from k_table.
  # 
  # Implementation Notes:
  #   only to be used in the apply function in likelihood() function
  
  return((k_table[3]*dnorm(x, k_table[1], k_table[2])))
}

