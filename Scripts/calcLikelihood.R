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
  #Purpose - Calcualtes the probability observations x are 
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
#k.estimates() 

