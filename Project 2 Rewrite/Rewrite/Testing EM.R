source("ErrorChecks.R", local = TRUE)
source("teamEM.R", local = TRUE)

# Generating Testing DataFrames:

gen.test.data <- function(continuous = FALSE){
  # Aim is to generate a random data frame of numeric values.
  # Inputs:
  #       "continuous": Giving a choice between continuous age values and 
  #       category style.
  # Outputs:
  #       "A": a data frame of random dimensions, randomly selected key columns
  #       and randomly generated values in those column. All values should be 
  #       numeric.
  
  #generate number of dimensions:
  n <- sample(3:100, 2, replace = T) ### should be :100
  #assign labels of FishID Length and Age to random columns:
  p <- sample(1:n[2], 3, replace = F) #replace false to avoid duplicate labels
  
  #create empty data frame using generations above:
  size <- n[1]*n[2]
  A <- data.frame(matrix(rep(0, size),nrow = n[1], ncol = n[2]))
  colnames(A)[p[1]] <- ("FishID")
  colnames(A)[p[2]] <- ("Length")
  colnames(A)[p[3]] <- ("Age")
  
  #data to fill important columns:
  if (continuous == FALSE){x <- sample(1:15, n[1], replace = T)}
  else{x <- runif(n[1], 1, 15)}
  y <- runif(n[1], 1, 100)
  z <- sample(1:1000, n[1], replace = T)
  
  #placing in data frame A:
  A[p[1]] <- z
  A[p[2]] <- y
  A[p[3]] <- x
  
  #putting in NAs into Age:
  num.of.na <- sample(ceiling(n[1]/10):floor(n[1]*9/10), 1)
  print(n[1])
  print(num.of.na)
  naselection <- sample(1:n[1], num.of.na, replace  = F)
  print(naselection)
  A[naselection, p[3]] <- NA
  print(A[naselection[1], p[3]])
  return(A)
}

#gen.test.data()


# Function for testing the Algorith Implementation:

imp.test.em <- function(A){
  # Currently assuming discrete data for Age values
  result <- teamEM(A)
  
  #output class checks: perfect score is 5
  class.check <- rep(0, 5)
  if (class(result$estimates) != numeric){class.check[1] = FALSE}
  if (class(result$inits) != numeric){class.check[2] = FALSE}
  if (class(result$posterior) != numeric){class.check[3] = FALSE}
  if (class(result$likelihood) != numeric){class.check[4] = FALSE}
  if (class(result$converged)!= boolean){class.check[5] = FALSE}
  class.check = sum(class.check)
  class.result <- 0
  if (class.check == 5){class.result = "All outputs in form expected."}
  else{class.result = "One or more outputs in a form unexpected."}
  
  #behavior testing, are A's results as expected?:
  #range testing:
  within.range.age <- round(seq(min(A$Age), max(A$Age))) #Age bounds, rounded
  within.range.length <- c(min(A$Length), max(A$Length)) #Length bounds
  within.range.ID <- c(min(A$FishID), max(A$FishID))
  num.of.cat <- abs(within.range.age[2]-within.range.age[1])
  behaviour.range <- matrix(rep(0, num.of.cat*2), nrow = num.of.cat, ncol = 2)
  
  for (i in range(num.of.cat)){
    if (result$estimates$mu[i] <= within.range.length[2] && 
        result$estimates$mu[i] >= within.range.length[1]){behaviour.range[i,1] = TRUE}
    if (result$inits$mu[i] <= within.range.length[2] && 
        result$inits$mu[i] >= within.range.length[1]){behaviour.range[i,2] = TRUE}
    #check known data to compare to initials and final estimates
    
  }

  
  #shape testing:
  #normality test on each of the age categories and display
  
  conclusion <- list(classCheck = class.result, behaviorCheck = behaviour)
  
  return(conclusion)
}
