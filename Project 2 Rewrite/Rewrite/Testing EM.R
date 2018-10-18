source("ErrorChecks.R", local = TRUE)
source("teamEM.R", local = TRUE)
source("functions.R", local = TRUE)

#-------------------------Generating Testing DataFrames-------------------------

# This is the new function, mmore direct approach, I've included plots for my own
# understanding, I'll probably be commented out.

gen.test.data <- function(continuous = FALSE){
  # Aim to produce three similar data sets similar to FishID, Length and Age, on
  # the basis of the pdf of the combination of three distributions.
  # Inputs:
  #       no inputs required.
  # Outputs:
  #       A list including a data frame of the mus, sigmas and lambdas of the three
  #       simulated distributions. These have been included so that they can be used 
  #       in imp.test.em to verify the final estimates that are given by the EM algo.
  #       The second item on the list is a vector of Fish Lengths to be used to 
  #        create a data frame that can be put through the teamEM function for testing.
 
  mu <- runif(3, 1, 100)
  sigma <- runif(3, 1, 10)
  lambda <- rep(0, 3)
  lambda[1] <- runif(1, .1, .5)
  lambda[2] <- runif(1, .1, .5)
  lambda[3] <- 1- (lambda[1]+lambda[2])
  n <- 1000
  
  X1 <- lambda[1]*rnorm(ceiling(n/3), mu[1], sigma[1])
  X2 <- lambda[2]*rnorm(ceiling(n/3)-1, mu[2], sigma[2])
  X3 <- lambda[3]*rnorm(ceiling(n/3)-1, mu[3], sigma[3])
  
  sim.fish.lengths <- c(X1, X2, X3)
  
  par(mfrow = c(2,1))
  hist(sim.fish.lengths, w = 5)
  x.plot <- seq(1., 100.9, .1)
  plot(x.plot, sim.fish.lengths)
  par(mfrow = c(2,1))
  
  k_table <- data.frame(mu = mu, sigma = sigma, lambda = lambda)
  
  return(list(k_table = k_table, sim.fish.lengths))
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
  
  plot(base, y = rowSums(y), col = "red", type =  "l", ylim = c(0,.04), xlab = " Length ", ylab = " Probability Density ", main = " Comparison from Initial to Final Estimates")
  lines(base, y = rowSums(z), col = "blue" )
  legend(0, 0.03, legend = c("Final Estimates", "Initial Estimates"), 
         col = c("red", "blue"), lty = 1:1, cex = .75)
  
  par(mfrow = c(1,1))
  
  #check known data to compare to initials and final estimates
  difference = abs(sum((y1+y2+y3) - (z1+z2+z3)))
  relative.diff <- difference/sum(z1+z2+z3)*100
  
  behaviour <- 0
  if (relative.diff > 10){behaviour = "Large variation between initial estimated distribtion and final estimated distribution."}
  if(relative.diff <= 10){behaviour = "Small variation between initial estimated distribtion and final estimated distribution."}
  
  
  conclusion <- list(classResult = class.result, classCheck = class.check, behaviourCheck = behaviour, differencePercentage = c(relative.diff, "%"))
  
  return(conclusion)
}





