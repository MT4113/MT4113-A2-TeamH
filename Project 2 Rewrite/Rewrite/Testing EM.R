

#-------------------------Generating Testing DataFrames-------------------------

# This is the generating function, I've included plots visualisation of the 
# distribution. For testing use third output, the data frame "sim.data.frame"
# can be used in teamEM, e.g. teamEM(gen.test.data()$simData)

gen.test.data <- function(continuous = FALSE){
  # Aim to produce three similar data sets similar to FishID, Length and Age, on
  # the basis of the pdf of the combination of three distributions.
  # Inputs:
  #       no inputs required.
  # Outputs:
  #       A list including a data frame of the mus, sigmas and lambdas of the 
  #       three simulated distributions. These have been included so that they  
  #       can be used in imp.test.em to verify the final estimates that are 
  #       given by the EM algo. The second item on the list is a vector of Fish  
  #       Lengths that has been used to create the third item, the data frame, 
  #       "sim.data.frame", that can be put through the teamEM function for testing.
  
  #creating parameters for simulation: 
  mu <- runif(3, 5, 100)
  sigma <- runif(3, 2, 10)
  lambda <- rep(0, 3)
  lambda[1] <- runif(1, .1, .4)
  lambda[2] <- runif(1, .1, .4)
  lambda[3] <- 1- (lambda[1]+lambda[2])
  n <- 1000
  
  X1 <- rnorm(round(lambda[1]*n), mu[1], sigma[1]) 
  X2 <- rnorm(round(lambda[2]*n), mu[2], sigma[2])
  X3 <- rnorm(round(lambda[3]*n+1), mu[3], sigma[3])
  
  sim.fish.lengths <- c(X1, X2, X3)
  sim.fish.lengths <- sim.fish.lengths[1:n]
  
  #plotting to demonstrate the combination of three different normal distribtion:
  par(mfrow = c(2,1))
  hist(sim.fish.lengths, breaks = seq(-50, 150, 5))
  x.plot <- seq(1, n, 1)
  plot(x.plot, sim.fish.lengths)
  par(mfrow = c(2,1))
  
  k_table <- data.frame(mu = mu, sigma = sigma, lambda = lambda)
  
   #creating data frame using simulated fish lengths:
  #choose lambda*100 places from the distribution:
  sim.data.frame <- data.frame("FishID" = rep(0, 1000), 
                               "Length" = sim.fish.lengths, "Age" = rep(NA, 1000))
  index.known <- rep(0, 100)
  index.known1 <- sample(1:round(lambda[1]*n), round(lambda[1]*100))
  index.known2 <- sample(1:round(lambda[2]*n), round(lambda[2]*100))
  index.known3 <- sample(1:round(lambda[3]*n), round(lambda[3]*100))
  
  #assigning Age categories:
  sim.data.frame$Age[index.known1] <- 1
  sim.data.frame$Age[round(lambda[1]*n)+index.known2] <- 2
  sim.data.frame$Age[round(lambda[1]*n) + round(lambda[2]*n) + index.known3] <- 3
  
  return(list(k_table = k_table, simLengths = sim.fish.lengths, simData = sim.data.frame))
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
  #                     "classCheck": confirming values belong to expected class.
  #                                   If 1 then that segment of teamEM returned
  #                                   in the class expected.
  #                     "behaviourCheck": comment on variation between initial 
  #                                       estimates and final estimates
  #                     "differencePercentage": diff in the closed integral of 
  #                                       each curve as a percentage of closed 
  #                                       integral of the initial estiamted pdf.
  
  source("teamEM.R", local = TRUE)

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
  
  base <- seq(0, 100, by = .1) 
  plot(base, y = rowSums(y), col = "red", type =  "l", ylim = c(0,.04), xlab = " Length ", ylab = " Probability Density ", main = " Comparison from Initial to Final Estimates")
  lines(base, y = rowSums(z), col = "blue" )
  legend(0, 0.03, legend = c("Final Estimates", "Initial Estimates"), 
         col = c("red", "blue"), lty = 1:1, cex = .75)
  
  par(mfrow = c(1,1))
  
  #check known data to compare to initials and final estimates
  difference = abs(sum(rowSums(y) - rowSums(z)))
  relative.diff <- difference/sum(z)*100
  
  behaviour <- 0
  if (relative.diff > 10){behaviour = "Large variation between initial estimated distribtion and final estimated distribution."}
  if(relative.diff <= 10){behaviour = "Small variation between initial estimated distribtion and final estimated distribution."}
  
  
  conclusion <- list(classResult = class.result, classCheck = class.check, behaviourCheck = behaviour, differencePercentage = c(relative.diff, "%"))
  
  return(conclusion)
}





