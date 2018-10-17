source("ErrorChecks.R", local = TRUE)
source("teamEM.R", local = TRUE)
source("functions.R", local = TRUE)

#-------------------------Generating Testing DataFrames-------------------------

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

#---------------Function for testing the Algorith Implementation----------------

imp.test.em <- function(A){
  # Aiming to show for each input data frame, "A", that the outputs given by the 
  # teamEM function are as would be expected for the data frame.
  # Input:
  #       "A": the data frame that you want to trial.
  # Outputs:
  #       "conclusion": a list that displays the results of the tests on 
  #                     features of the output of teamEM(A).
  
  result <- teamEM(A)
  
  uniq_ages <- unique(A$Age)         
  uniq_ages <-uniq_ages[order(uniq_ages)]
  uniq_ages <- uniq_ages[uniq_ages != -1]
  k_numb <- length(uniq_ages)
  
  #output class checks: perfect score is 5
  class.check <- data.frame(check = rep(0,5), row.names =  c("estimates", "inits", "posterior", "likelihood", "converged"))
  if (is.numeric(data.matrix(result$estimates)) == TRUE){class.check[1,1] <- 1}
  if (is.numeric(data.matrix(result$inits)) == TRUE){class.check[2,1] <- 1}
  if (is.numeric(data.matrix(result$posterior)) == TRUE){class.check[3,1] <- 1}
  if (is.numeric(result$likelihood) == TRUE){class.check[4,1] <- 1}
  if (is.logical(result$converged) == TRUE){class.check[5,1] <- 1}
  print(class.check)
  class.check.sum = sum(class.check)
  class.result <- 0
  if (class.check.sum == 5){class.result = "All outputs in form expected."}
  else{class.result = "One or more outputs in a form unexpected."}

  #behavior testing, are A's results as expected?:

  behaviour <- matrix(rep(0, k_numb * 2), nrow = k_numb, ncol = 2)
  
  #check known data to compare to initials and final estimates
  change.from.inits <- abs(result$estimates$mu-result$inits$mu)/result$inits$mu
  
  # shape testing specific
  result <- teamEM(x)
  y <- mapply(function(mu,sigma,lambda, base){return(lambda*dnorm(base,mu,sigma))}, result$estimates$mu, 
              result$estimates$sigma, result$estimates$lambda, 
              MoreArgs=list(base = seq(0, 100, by = .1)))
  z <- mapply(function(mu,sigma,lambda, base){return(lambda*dnorm(base,mu,sigma))}, result$inits$mu, 
              result$inits$sigma, result$inits$lambda, 
              MoreArgs=list(base = seq(0, 100, by = .1)))
  
  par(mfrow = c(1,1))
  xdata <- x$Length
  
  plot(base, y = rowSums(y), col = "red", type =  "l", xlab = " Length ", ylab = " Probability Density ", main = " Comparison from Initial to Final Estimates")
  lines(base, y = rowSums(z), col = "blue" )
  legend(0, 0.03, legend = c("Final Estimates", "Initial Estimates"), 
         col = c("red", "blue"), lty = 1:1, cex = .75)
  
  par(mfrow = c(1,1))
  
  difference = abs(sum((y1+y2+y3) - (z1+z2+z3)))
  relative.diff <- difference/sum(z1+z2+z3)
  #normality test on each of the age categories and display
  
  conclusion <- list(classCheck = class.result, behaviorCheck = behaviour, differenceToEnd = relative.diff)
  
  return(conclusion)
}


