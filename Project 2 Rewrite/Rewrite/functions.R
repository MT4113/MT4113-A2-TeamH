
init_data_ests <- function(data, ages){
  # Initalizes the data, returns table of mu, sigma and lambda estimates based on known values
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown  
  
  N <- length(data$Length[(data$Age != -1)]) # numb of values that are known
  
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
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown 

  #based on k_table, assign probabilites 
  p_mat <- mapply(function(mu, sigma, dat){
    return(dnorm(dat, mu, sigma))
  },k_table$mu, k_table$sigma ,MoreArgs = list(dat = unknown_dat$Length))

  #Find most suitable class for each value
  unknown_dat$Age <- apply(p_mat, 1, function(x){which.max(x)})
  
  #Combines estimated ages and actual known ages
  allAge_dat <- unknown_dat
  if(flag){allAge_dat <- rbind(allAge_dat, known_dat)}
  N <- length(allAge_dat$Age)

  init_ests <- mapply(
    function(i, data){
      return(c(
        mean(allAge_dat$Length[allAge_dat$Age == i]), 
        sd(allAge_dat$Length[allAge_dat$Age == i]),
        length(allAge_dat$Age[allAge_dat$Age == i]))
      )
    }, ages, MoreArgs = list(data = allAge_dat))
  
  return(data.frame("mu" = init_ests[1,], "sigma" = init_ests[2,], 
                    "lambda" = init_ests[3,]/N))
  
}

prob_ests <- function(data, k_table, k_numb){
  # Data: - ONLY UNKNOWN DATA IS INPUT
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown 
  
  N <- length(data$Age) #Total number of unknown ages in the thing
  
  p_mat <- mapply(function(mu, sigma, data, lambda){
                  return(dnorm(data, mu, sigma) * lambda)},
                  k_table$mu, k_table$sigma, k_table$lambda,
                  MoreArgs = list(data=data$Length))
  
  denomSums <- rowSums(p_mat)

  if (k_numb == 1){
    p_mat <- p_mat/denomSums
  } else {
    p_mat <- apply(p_mat, 2, function(x,ds){return(x/ds)}, ds = denomSums) 
  }
  

  output <- data.frame(data[,c(1:2)], p_mat)
  return(output)
}

add_binary_cols <- function(x, dat){
  return(ifelse(x == dat, 1, 0))
}

max_ests <- function(prob_table, k_table, k_numb, flag, known_dat){
  # Data: - ONLY UNKNOWN DATA IS INPUT
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 to 3+k_numb - probs it is in each age class k
  probs <- prob_table[,c(-1,-2)] #simply to only probabilites 
  lengths <- prob_table$Length #Can add lengths and stuff to the bottom if we want to extend for true values
  
  if(flag){  #Possible to optimize by storing the code and then just appending the df 
    prob_table_known <- matrix(rep(0,length(known_dat$Age) * k_numb), ncol = k_numb)
    
    unique_Ages <- unique(known_dat$Age)
    unique_Ages <- unique_Ages[order(unique_Ages)]
    
    prob_table_known <- sapply(unique_Ages, add_binary_cols, dat = known_dat$Age)
    
    format <- data.frame(prob_table_known)
    probs <- rbind(format, probs)
    lengths <- c(known_dat$Length, lengths)
  }
  
  N <- length(lengths)
  
  #Fixes a problem wiht k_numb == 1
  if(k_numb == 1){
    tmp <- data.frame("probs" = probs ,"x2" = probs)
    probs <- tmp[,1, drop = F]
  }
  
  baseProbSum<- colSums(probs) #sums of all probs based on age class
  
  #Mean Calculations
  numerMeans <- apply(probs, 2, function(x,y){return(sum(x*y))}, y = lengths)
  k_table$mu <- numerMeans/baseProbSum
  
  
  #Sd Calculations
  k_table$sigma <- mapply(function(prob, mu, baseProbSumVal, lengths){
                  return( sqrt(sum( (prob * ((lengths-mu)^2) / baseProbSumVal))) )
                  },probs, k_table$mu, baseProbSum, MoreArgs = list(lengths = lengths))
  
  #Lambida Calculations 
  k_table$lambda <- baseProbSum/N
  
  return(k_table)
}

likelihood <- function(unknown_dat, k_table, k_numb){
  # Data: - ONLY UNKNOWN DATA IS INPUT
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown  
  
  init <- 0 
  init <- apply(k_table, 1, normalFunc, x = unknown_dat$Length)
  temp <- rowSums(init)
  temp <- log(temp)
  return(sum(temp))
}

normalFunc <- function(k_table, x){
  return((k_table[3]*dnorm(x, k_table[1], k_table[2])))
}

