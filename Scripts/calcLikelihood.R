likelihood <- function(x){

}

#---------------------------------Initialising:---------------------------------

# Set Up
load("FishLengths.RData")

fishdata <- x
# fishdata$Age <- factor(fishdata$Age)
knownage <- na.omit(fishdata)                 # Where real ages have been given.
fishdata[is.na(fishdata)] <- 0
unknownage <- fishdata[which(fishdata$Age == 0),]


# Function to assign unkownage[,2] with age category using dist of known:
prob.category <- function(x, category){
  #Purpose - Calcualtes the probability observations x are of age category  
  dist <- knownage[knownage$Age == category,]
  mu <- mean(dist[,2]) 
  sd <- var(dist[,2])^.5
  prob <- dnorm(x, mu, sd)
  return(prob)
}

initials.for.unknown <- function(x){
  # Input intended to be unknownage[,2]. 
  # Output to give dataframe of fish lengths against expected ages.
  
  ### Needs to be generalized for any set of dataframe
  initials <- data.frame("Length" = x, "Age" = rep(0, 900))
  probs <- matrix(rep(0, 2700),nrow = 900, ncol = 3)
  
  # Calculates the probabilites for each category 
  probs[,1] <- prob.category(x,1)
  probs[,2] <- prob.category(x,2)
  probs[,3] <- prob.category(x,3)
  
  # This is the most efficent method, double check this works (Bryant)
  initials[,2] <- apply(probs[,c(1:3)], 1, function(x){which.max(x)})
  return(initials)
}

k.estimates <- function(){
  # Input of the "initials" data frame from function "initials.for.unknown".
  # Output will give table of sigmas, mus and lambdas for each class k for age.
  
  initials <- initials.for.unknown(unknownage[,2]) # Initialising vectors 
  initials1 <- initials[which(initials[,2] == 1),]
  initials2 <- initials[which(initials[,2] == 2),]
  initials3 <- initials[which(initials[,2] == 3),]
  N <- 900                                        #Excluding known ages.
  
  #Calculating mean and standard deviation values for each age class:
  mu <- c(mean(initials1[,1]), mean(initials2[,1]), mean(initials3[,1]))
  variance <- c(var(initials1[,1]), var(initials2[,1]), var(initials3[,1]))
  sigma <- sqrt(variance)
  lambda <- c(length(initials1[,1]), length(initials2[,1]), length(initials3[,1]))/N
  
  return(data.frame("mu" = mu, "sigma" = sigma, "lambda" = lambda))
}
test <- k.estimates() 

prob_expectations <- function(df, k_table, k_numb){
  # Purpose - Calculate the probabilites for each observations
  # Inputs
  #       df - VECTOR containing fish lengths
  #       t_table - dataframe containing estimates of mu, sigma and lambda
  #       k_numb - single value dictating the total number of factors for k
  # Outputs
  #         Matrix of results wiht probabiltes for each observation to be in each age class (k)
  
  # Generates empty matrix of probabilities 
  prob_df <- matrix(rep(0, k_numb*length(df)),nrow = length(df), ncol = k_numb)
  
  # Gets the prior probaility (lambida), and generates the likelihood P(x|x in k) for all age classes
  for (i in c(1:k_numb)) {
    prob_df[,i] <- sapply(df, function(x){dnorm(x, k_table[i, "mu"], k_table[i, "sigma"])})
    prob_df[,i] <- prob_df[,i]*k_table[i, "lambda"]
  }
  
  #Cacluates the posterior (LHS of equation)
  
  apply(prob_df, 1, sum)
  standardized_whole <- apply(prob_df, 1, sum)
  prob_df <- prob_df/standardized_whole
  
  return(prob_df)
}

max_Ests <- function(df){
  # Purpose _ to generate MLE estimates for a given dataframe
  # Inputs - Dataframe containing lengths in col 1 and probabilites in columns 2-4 
  # outputs - dataframe of mean, stdev and lambida estimates 
  

  #Sum of all probabilites based on age class prediction
  denoms <- apply(df[,c(2:4)], 2, sum)
  print(denoms)
  
  # Estimates for the mean 
  mu <- apply(df[,c(2:4)], 2, function(x){return(sum(x * df[,1]))})
  mu <- mu/denoms
  print(mu)
  #Estimates for stdev. THIS IS NOT OPTIMZED YET, Not sure if i can get rid of the for loop 
  stdev <- c(NA, NA, NA)
  for (i in c(1:3)){
    stdev[i] <- sum(df[,i+1]* ((df[,1]-mu[i])^2))
    stdev[i] <- sqrt(stdev[i]/denoms[1])
  }
  print(stdev)
  
  # Estimates for Lambida
  lambda <- denoms/length(df[,1])
  print(lambda)
  
  return(data.frame("mu" = mu, "sigma" = stdev, "lambda" = lambda))
}

