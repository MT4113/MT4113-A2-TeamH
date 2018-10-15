#--------------------------------Calc Likelihood:-------------------------------
likelihood <- function(x){
  return(22)
}

#-------------------------------Data Exploration:-------------------------------
# Set Up

load("FishLengths.RData")                           # The data frame is called x

known <- function(dat){
  # To split known data from unknown data assuming the known/unknown occurs in 
  # the final column.
  # Input:
  #       data: a Data frame with a mix of known and unkown
  # Output: 
  #       knownx: A dataframe with all the known values
  knownx <- na.omit(dat)
  return(knownx)
}
unknown <- function(dat){
  # To split known data from unknown data assuming the known/unknown occurs in 
  # the final column.
  # Input:
  #       dat: a Data frame
  # Output: 
  #       unknownx: A dataframe with all the unknown values
  n <- length(dat[1,]) # Counting number of columns for use to access final col 
  dat[is.na(dat)] <- 0
  unknownx <- dat[which(dat[,n] == 0),]
  return(unknownx)
  
}

#---------------------------------Initialising:---------------------------------


# Function to assign unkownage[,2] with age category using distribution of the 
# fish lenghts already known:

prob.category <- function(y, dat = x, column = 2, category = 1){
  # Gives the probability of a value "y" appearing in a given a certain category 
  # for the final column. E.g.category could be from {1, 2, 3, ...}. 
  # Limited to known sub set of "dat" data frame.
  #Inputs
  #     y: specific value in column "column" that you want to find probability
  #     dat: data frame with the column you want distribution of the known vals
  #     column: the column in that data frame you want to investigate
  #     category: specific category that you want to check, e.g. age class, in
  #               final column.
  #Outputs
  #     prob: probability of value y occuring in a category, assuming normal for
  #           each category.
  x <- known(dat)
  n <- length(x[1,])              # For selecting the final column
  dist <- x[x[,n] == category,]   # Subsetting to required value in final column
  mu <- mean(dist[,column])       # Measuring the parameters from the colum of 
  sd <- var(dist[,column])^.5     # of interest.
  prob <- dnorm(y, mu, sd)
  
  return(prob)
}

initials.for.unknown <- function(dat = x, column = 2){
  # The aim of this function is to use data from the known data frame to
  # estimate the values for the unknown sub-frame's final column. In this 
  # exercise, the "Response variable" is the length with the "Est variable"  
  #  being the Age category.
  # Inputs: 
  #       dat: data frame
  #       column: column to be investigated (e.g. 2 goes to [,2] gives Fish 
  #               Lengths)
  # Outputs:
  #       initials: a data frame to give dataframe of fish lengths against 
  #                 expected ages.
  y <- unknown(dat)     # Only need to set initial values where they are unknown
  
  initials <- data.frame("Response variable" = y[,column], 
                         "Est variable" = rep(0, 900))
  probs <- matrix(rep(0, 2700),nrow = 900, ncol = 3)
  
  probs[,1] <- prob.category(dat, column, 1)
  probs[,2] <- prob.category(dat, column, 2)
  probs[,3] <- prob.category(dat, column, 3)

  # Assigning age category
  initials[,2] <- apply(probs[,c(1:3)], 1, function(x){which.max(x)})
  return(initials)
}

#THIS IS THE FUNCTION YOU CALL TO INIT THE DATA
k.estimates <- function(dat, column){
  # Aims to create a data frame of the required parameters when considering the 
  # initial assignments given to unknown sub-frame.
  # Inputs:
  #        dat: data frame
  #        column: column of data frame that you want to investigate.
  # Outputs: 
  #        will give table of sigmas, mus and lambdas for each class k for age.
  
  initials <- initials.for.unknown(dat, column)           # Initialising vectors 
  initials1 <- initials[which(initials[,2] == 1),]
  initials2 <- initials[which(initials[,2] == 2),]
  initials3 <- initials[which(initials[,2] == 3),]
  N <- 900                                                # Excluding known ages
  
  #Calculating mean and standard deviation values for each age class:
  mu <- c(mean(initials1[,1]), mean(initials2[,1]), mean(initials3[,1]))
  variance <- c(var(initials1[,1]), var(initials2[,1]), var(initials3[,1]))
  sigma <- sqrt(variance)
  lambda <- c(length(initials1[,1]), length(initials2[,1]), 
              length(initials3[,1]))/N
  
  return(data.frame("mu" = mu, "sigma" = sigma, "lambda" = lambda))
}

#-------------------------------Post Initiation---------------------------------

prob_expectations <- function(df, k_table, k_numb){
  # Purpose - Calculate the probabilites for each observations
  # Inputs
  #       df - dataframe containing the following
  #            length of fish for observations
  #            results wiht probabiltes for each observation to be in each age class (k)
  #       t_table - dataframe containing estimates of mu, sigma and lambda
  #       k_numb - single value dictating the total number of factors for k
  # Outputs
  #         Matrix of results wiht probabiltes for each observation to be in each age class (k)
  

  # Gets the prior probaility (lambida), and generates the likelihood P(x|x in k) for all age classes

  
  for (i in c(2:(k_numb+1))){
    df[,i] <- dnorm(df[,1], k_table[i-1,"mu"], k_table[i-1,"sigma"]) #I have no clue WHY nans can appear. 
    # They shouldnt but they DO appear in this step
    df[is.nan(df[,i]),i] <- 0
    
  }
  
  #Cacluates the posterior (LHS of equation)
  standardized_whole <- apply(df[,-1], 1, sum)
  
  for (i in c(2:(k_numb+1))){
    df[,i] <- (df[,i]*k_table[i-1, "lambda"])/standardized_whole
    df[is.nan(df[,i]),i] <- 0
  }
  return(df)
}

#This shoudlnt be broken but it is for some reason 
max_Ests <- function(df){
  # Purpose _ to generate MLE estimates for a given dataframe
  # Inputs - Dataframe containing lengths in col 1 and probabilites in the remaining columns
  # outputs - dataframe of mean, stdev and lambida estimates 
  
  k_val <- length(df[1,])
  
  #Sum of all probabilites based on age class prediction
  denoms <- apply(df[,-1], 2, sum)
  
  # Estimates for the mean 
  mu <- apply(df[,c(2:k_val)], 2, function(x){return(sum((x * df[,1])))})
  mu <- mu/denoms

  mu[is.nan(mu)] <- 0
  
  #Estimates for stdev. THIS IS NOT OPTIMZED YET, Not sure if i can get rid of the for loop 
  stdev <- rep(NA, k_val-1)
  for (i in c(1:(k_val-1))){
    stdev[i] <- sum(df[,i+1]* ((df[,1]-mu[i])^2))
    stdev[i] <- sqrt(stdev[i]/denoms[1])
  }

  # Estimates for Lambida
  lambda <- denoms/length(df[,1])
  toReturn <- data.frame("mu" = mu, "sigma" = stdev, "lambda" = lambda)
  return(toReturn)
}

#_____Likelihood_______


Likelihood_Eval <- function(dat, df) {
  #Inputs:
  #dat - the obsevations, which you want to test, in our case it is x[,2] -> lengths
  #df - dataframe with means, sds and lambdas, in our case it is max_Ests()
  #n - number of iterations
  #Output:
  #vector of likelihoods
  
  ####for checking df I was using k.estimates(x,2) and dat <- x

  vector1 <- NULL
  for (i in 1:length(dat)) {
    exp_prob = 0 
    for (k in 1:nrow(df)) {
      prob <- df[k,3]*dnorm(dat[i], df[k,1], df[k,2])
      exp_prob = exp_prob+prob
    }
    vector1 <- c(vector1, exp_prob)
  }
  return(vector1)
}


Iteration_likelihood <- function(dat,df) {
  val = sum(log(Likelihood_Eval(dat,df)))
  return(val)
}



#Iteration_likelihood(x[,2],k.estimates(x,2))



