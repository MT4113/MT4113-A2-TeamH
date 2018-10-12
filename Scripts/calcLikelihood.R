#--------------------------------Calc Likelihood:-------------------------------
likelihood <- function(x){
  
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
unkown <- function(dat){
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

# Plot for Visualisation of Overlap between Categories:
plot.understanding <- function(dat){
  # The aim of these plots is to demonstrate the difference in distribution in 
  # the known data and all the data, as well as the boxplots for the three 
  # categories.
  
  par(mfrow = c(3,1))
  hist(known(dat)[,2], xlab = "Fish Length (cm)", ylab = "Frequency", 
       main = "Histogram of Known")
  hist(dat[,2], xlab = "Fish Length (cm)", ylab = "Frequency", 
       main = "Histogram of All")
  
  plot(known(dat)$Age, known(dat)$Length, xlab = "Age Category", 
       ylab = "Fish Length")
  
  abline(h = max(known(dat)[known(dat)[,3] ==1,2]), col = "blue")
  abline(h = min(known(dat)[known(dat)[,3] ==1,2]), col = "blue", , lty = 3)
  abline(h = max(known(dat)[known(dat)[,3] ==2,2]), col = "red")
  abline(h = min(known(dat)[known(dat)[,3] ==2,2]), col = "red", , lty = 3)
  abline(h = max(known(dat)[known(dat)[,3] ==3,2]), col = "green")
  abline(h = min(known(dat)[known(dat)[,3] ==3,2]), col = "green", lty = 3)
  
  par(mfrow = c(1,1))
}

# Required Plots
plot.required <- function(dat){
  # The aim here is to return the plots that are required in Task 1: Data Expl.
  
  # Histogram
  par(mfrow = c(2,1))
  hist(dat[,2], xlab = "Fish Length (cm)", ylab = "Frequency", 
       main = "Histogram of Fish Lengths")
  # Scatter Plot
  plot(dat[,3], dat[,2], xaxt = "n", xlab = "Fish Age Category", 
       ylab = "Fish Length (cm)",
       main = "Plot of Age against Length in Fish")
  axis(1, at = c(1,2,3))
  par(mfrow = c(1,1))
}


#---------------------------------Initialising:---------------------------------


# Function to assign unkownage[,2] with age category using distribution of the 
# fish lenghts already known:

prob.category <- function(dat, column, category = 1){
  # Gives the parameters of the column of interest given a certain category for 
  # the final column. E.g.category could be from {1, 2, 3, ...}. 
  # Limited to known sub set of "dat" data frame.
  #Inputs
  #     dat: data frame with the column you want distribution of the known vals
  #     column: the column in that data frame you want to investigate
  #     category: specific category that you want to check, e.g. age class, in
  #               final column.
  #Outputs
  #     prob: a vector of length, mean and standard deviation. I've included the 
  #           length to see how many values fall into certain categories.
  
  x <- known(dat)
  n <- length(x[1,]) #sets n as the last column
  dist <- x[x[,n] == category,]   # Subsetting to required value in final column
  mu <- mean(dist[,column])       # Measuring the parameters from the colum of 
  sd <- var(dist[,column])^.5     # of interest.
  y <- unkown(dat)
  
  prob <- dnorm(y[,1], mu, sd)
  return(prob)
}

initials.for.unknown <- function(dat, column){
  # The aim of this function is to use data from the known data frame to
  # estimate the values for the unknown sub-frame. In this exercise, the 
  # "Response variable" is the length with the "Est variable" being the  
  # Age category.
  # Inputs: 
  #       dat: data frame
  #       column: column to be investigated (e.g. 2 goes to [,2] gives Fish 
  #               Lengths)
  # Outputs:
  #       initials: a data frame to give dataframe of fish lengths against 
  #                 expected ages.
  y <- unkown(dat)     # Only need to set initial values where they are unknown
  
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
  # Inputs - Dataframe containing lengths in col 1 and probabilites in the remaining columns
  # outputs - dataframe of mean, stdev and lambida estimates 
  
  k_val <- length(df[1,])

  #Sum of all probabilites based on age class prediction
  denoms <- apply(df[,c(2:k_val)], 2, sum)

  # Estimates for the mean 
  mu <- apply(df[,c(2:k_val)], 2, function(x){return(sum(x * df[,1]))})
  mu <- mu/denoms
  #Estimates for stdev. THIS IS NOT OPTIMZED YET, Not sure if i can get rid of the for loop 
  stdev <- rep(NA, k_val-1)
  for (i in c(1:(k_val-1))){
    stdev[i] <- sum(df[,i+1]* ((df[,1]-mu[i])^2))
    stdev[i] <- sqrt(stdev[i]/denoms[1])
  }

  # Estimates for Lambida
  lambda <- denoms/length(df[,1])

  return(data.frame("mu" = mu, "sigma" = stdev, "lambda" = lambda))
}

