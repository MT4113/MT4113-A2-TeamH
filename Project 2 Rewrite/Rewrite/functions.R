load("FishLengths.RData") 

init_data_ests <- function(data){
  # Initalizes the data, returns table of mu, sigma and lambda estimates based on known values
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown  
  
  ages <- unique(data$Age)
  ages <- ages[ages != -1]  #removes unknown ages
  ages <- ages[order(ages)] #Ensures lowest age is first to highest
  
  mu <- rep(NA, length(ages))
  sigma <- rep(NA, length(ages))
  lambda <- rep(NA, length(ages))
  
  for (i in c(1:length(ages))){
    mu[i] <- mean(data[(data$Age == ages[i]),2])
    sigma[i] <- sd(data[(data$Age == ages[i]),2])
    lambda[i] <- length(data[(data$Age == ages[i]),2])
  }
  
  lambda <- lambda/length(data[(data[,3] != -1),2])
  
  return(data.frame("mu" = mu, "sigma" = sigma, "lambda" = lambda))
  
}

init_prob_ests <- function(data, k_table, k_numb){
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown 
  
  unknown_dat <- data[data$Age == -1,] #Sifts for only unknown data
  N <- length(unknown_dat$Age) #total number of unknown data
  
  p_mat <- matrix(rep(NA,k_numb*N), ncol = k_numb) #Matrix of probabilites
  
  #based on k_table, assign probabilites 
  for (i in c(1:k_numb)){
    p_mat[,i] <- dnorm(unknown_dat$Length, k_table[i,"mu"], k_table[i,"sigma"])
  }
  
  #Find most suitable class for each value
  unknown_dat$Age <- apply(p_mat, 1, function(x){which.max(x)})
  
  ages <- unique(unknown_dat$Age)
  ages <- ages[order(ages)]
  
  
  for (i in c(1:length(ages))){
    k_table$lambda[i] <- length(unknown_dat$Age[unknown_dat$Age == ages[i]])/N
    k_table$mu[i] <- mean(unknown_dat$Length[unknown_dat$Age == ages[i]])
    k_table$sigma[i] <- sd(unknown_dat$Length[unknown_dat$Age == ages[i]])
  }
  
  return(k_table)
}

prob_ests <- function(data, k_table, k_numb){
  # Data: - ONLY UNKNOWN DATA IS INPUT
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown 
  
  N <- length(data$Age) #Total number of unknown ages in the thing
  p_mat <- matrix(rep(NA,k_numb*N), ncol = k_numb) #Matrix of probabilites
  
  for (i in c(1:k_numb)){ 
    p_mat[,i] <- dnorm(data$Length, k_table[i,"mu"], k_table[i,"sigma"])
    p_mat[,i] <- (p_mat[,i] * k_table[i,"lambda"])
  }
  
  denomSums <- apply(p_mat, 1, sum)
  
  for (i in c(1:k_numb)){ #Can change this to an apply function 
    p_mat[,i] <- p_mat[,i]/denomSums
  }
  
  output <- data.frame(data[,c(1:2)], p_mat)
  return(output)
}

max_ests <- function(prob_table, k_table, k_numb){
  # Data: - ONLY UNKNOWN DATA IS INPUT
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 to 3+k_numb - probs it is in each age class k
  
  N <- length(prob_table$Length)
  
  probs <- prob_table[,c(-1,-2)] #simply to only probabilites 
  lengths <- prob_table[,"Length"] #Can add lengths and stuff to the bottom if we want to extend for true values
  
  
  baseProbSum<- apply(probs, 2, sum) #sums of all probs based on age class
  
  
  #Mean Calculations
  numerMeans <- apply(probs, 2, function(x,y){return(sum(x*y))}, y = lengths)
  k_table$mu <- numerMeans/baseProbSum
  
  
  #Sd Calculations
  for(i in c(1:k_numb)){
    k_table$sigma[i] <- sqrt( sum( (probs[,i] * ((lengths - k_table$mu[i])^2)) / baseProbSum[i]))
    
    #print(sqrt( sum( (probs[,i] * ((lengths - k_table$mu[i])^2)) / baseProbSum)))
    #print(length((probs[,i] * ((lengths - k_table$mu[i])^2))))
  }
  
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
  k_table_logged <- k_table
  k_table_logged$lambda <- log(k_table_logged$lambda)
  
  init <- sapply(unknown_dat$Length, normalFunc, k_table = k_table_logged)
  return(sum(init))
}

normalFunc <- function(x, k_table){
  return(sum(k_table$lambda + dnorm(x, k_table$mu, k_table$sigma, log = TRUE)))
}

