load("FishLengths.RData") 

init_data_ests <- function(data){
  # Initalizes the data, returns table of mu, sigma and lambda estimates based on known values
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown  
  
  ages <- unique(data$Age) #Gets all the unique Ages used in k
  ages <- ages[ages != -1]  #removes unknown ages
  ages <- ages[order(ages)] #Ensures lowest age is first to highest
  N <- length(data[(data[,3] != -1),2]) # numb of values that are known
  
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

init_prob_ests <- function(data, k_table, k_numb, flag){
  # Data:
  #   Col1 - the fish IDs
  #   Col2 - The Fish lengths 
  #   Col3 - The age class they are in (k = ...?). -1 if unknown 
  
  unknown_dat <- data[data$Age == -1,] #Sifts for only unknown data
  known_dat <- data[data$Age != -1,] #gets all the known data
  
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
  
  ages <- unique(allAge_dat$Age)
  ages <- ages[order(ages)]

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
  
  denomSums <- apply(p_mat, 1, sum)
  
  p_mat <- apply(p_mat, 2, function(x,ds){return(x/ds)}, ds = denomSums)

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
  k_table_logged <- k_table
  k_table_logged$lambda <- log(k_table_logged$lambda)
  
  init <- sapply(unknown_dat$Length, normalFunc, k_table = k_table_logged)
  return(sum(init))
}

normalFunc <- function(x, k_table){
  return(sum(k_table$lambda + dnorm(x, k_table$mu, k_table$sigma, log = TRUE)))
}

