

#-------------------------Generating Testing DataFrames-------------------------

# This is the generating function, I've included plots visualisation of the 
# distribution. For testing use third output, the data frame "sim.data.frame"
# can be used in teamEM, e.g. teamEM(gen.test.data()$simData)

gen.test.data <- function(n = 1000, known = c(20,46,34), mu = c(24,42,68), 
                          sigma = c(4, 5.5, 8.15), age = c(2,3,4), 
                          continuous = FALSE){
  # Aim to produce three similar data sets similar to FishID, Length and Age, on
  # the basis of the pdf of the combination of three distributions.
  # Defaults are set to be similar to FishLengths.R
  # Inputs:
  #       n - Number of random lengths.
  #       known - vector of number of known observations for each age class
  #       mu - means for simulated data
  #       sigma - sigma values for simluated data
  #       age - the age catagory for observations
  #       continuous - flag to indicate if ages should be continuious or not, 
  # Outputs:
  #       A list including a data frame of the mus, sigmas and lambdas of the 
  #       three simulated distributions. These have been included so that they  
  #       can be used in imp.test.em to verify the final estimates that are 
  #       given by the EM algo. The second item on the list is a vector of Fish  
  #       Lengths that has been used to create the third item, the data frame, 
  #       "sim.data.frame", that can be put through the teamEM function for testing.
  
  #Calculate lambda
  lambda <- known/sum(known)
  
  #Generate Random Lengths
  gen_Temp <- NULL
  for (i in c(1:length(known))){
    gen_Temp <- c(gen_Temp, rnorm(known[i], mu[i], sigma[i]))
  }
  
  #Generate Unknown random lengths
  rng <- sample(c(1:length(known)), n-sum(known), replace = T)
  unk_lengths <- rnorm(known[rng], mu[rng], sigma[rng])
  sim.fish.lengths <- c(gen_Temp,unk_lengths)
  
  # # Plots to illustrate the values are sufficently randomly generated from
  # # specified means and sds. They follow format of a plot of known random
  # # lengths and a plot all random lengths in a histogram and scatterplot
  # par(mfrow = c(2,1))
  # hist(gen_Temp)
  # hist(sim.fish.lengths)
  # x.plot <- seq(1, n, 1)
  # plot(seq(1, sum(known), 1), gen_Temp)
  # for (i in cumsum(known)){abline(v=i+0.5)}
  # plot(x.plot, sim.fish.lengths)
  # for (i in cumsum(known)){abline(v=i+0.5)}
  # par(mfrow = c(1,1))
  
  #k_table format of u, sigma and lambda
  k_table <- data.frame(mu = mu, sigma = sigma, lambda = lambda)
  rownames(k_table) <- age

  #Add NAs for ages that are unknown
  vec_Ages <- NULL
  for (i in c(1:length(age))){
    vec_Ages <- c(vec_Ages, rep(age[i], known[i]))
  }
  vec_Ages <- c(c(vec_Ages, rep(NA, n-sum(known)))) 
  
  # Build dataframe for ouput
  sim.data.frame <- data.frame("Length" = sim.fish.lengths, "Age" = vec_Ages)
  
  # Jitter ages if testing for non-continuious ages
  if (continuous){sim.data.frame$Age <- jitter(sim.data.frame$Age, factor = .5)}
  
  return(list(k_table = k_table, simData = sim.data.frame))
}

gen.test.data()


#---------------Function for testing the Algorith Implementation----------------

# Further additions to come here, going to quantify behaviour differently, can 
# simuulated sets from above against their real parameters, if there is existence 
# of the real data "behaviour" segment will be in comparison to that.
# If there isn't any real data, as in the case of the original dataset, will only
# compare to initial.

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
  
  source("Scripts/teamEM.R", local = TRUE)

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

test_ErrorChecks <- function(x){
  #Funciton to iterate some of the testing for booleans and numeric values
  
  source("Scripts/ErrorChecks.R", local = TRUE)
  bool <- boolean_check(x)
  numbPos <- numeric_Check(x, int_flag = F, pos_flag = T)
  numbInt <- numeric_Check(x, int_flag = T, pos_flag = F)
  return(c(bool, numbPos, numbInt))
}
testing_ErrorChecks <- function(){
  # Testing to ensure error checks are handled properly
  # Should output:
  # [1] FALSE  TRUE  TRUE
  # [1] FALSE FALSE  TRUE
  # [1] FALSE  TRUE FALSE
  # [1] FALSE FALSE FALSE
  # [1]  TRUE FALSE FALSE
  # [1]  TRUE FALSE FALSE
  # [1] TRUE
  # [1] TRUE
  # [1] FALSE
  # [1] FALSE
  # [1] FALSE
  # [1] FALSE
  # [1] FALSE
  # [1] FALSE
  # [1] FALSE
  # [1] FALSE
  
  source("Scripts/ErrorChecks.R", local = TRUE)
  
  tmp <- c(1,-1,1.1,-1.1)
  for (i in tmp){
    print(test_ErrorChecks(i))
  }
  tmp <- c(T, F)
  for (i in tmp){
    print(test_ErrorChecks(i))
  }
  
  df <- data.frame(Age = c(1,2,1,2,1,NA), Length = c(1,2,3,1,2,1))
  print(df_check(df)) #True
  
  df <- data.frame(Age = c(1,22,1,22,1,NA), Length = c(1,2,3,1,2,1))
  print(df_check(df)) #True
  
  df <- data.frame(Age = c(1,22,1,22,1,22), Length = c(1,2,3,1,2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Age = c(1,22,1,22,1,NA), Length = c(NA,2,3,1,2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Bacon = c(1,22,1,22,1,NA), Length = c(1,2,3,1,2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Age = c(1,22,1,22,1,NA), Strips = c(1,2,3,1,2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Age = c("1","22","1","22","1","22"), Length=c(1,2,3,1,2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Age = c(1,22,1,22,1,NA), Length = c(1,2,3,"1",2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Age = c(-1, -1, -1, -1, -1, NA), Length = c(1,2,3,1,2,1))
  print(df_check(df)) #False
  
  df <- data.frame(Age = as.factor(c(1,22,1,22,1,NA)), Length = c(1,2,3,1,2,1))
  print(df_check(df)) #False
}
#testing_ErrorChecks()

test_functions <- function(){
  # Tests to ensure the inputs and outputs are of the correct form 
  # Outputs: Various tables for visual inspection of data 
  
  source("Scripts/functions.R", local = TRUE)
  #This is to be replaced by a generated dataframe 
  load("FishLengths.RData")
  x$Age[is.na(x$Age)] <- -1
  x$FishID <- NULL
  
  ages <- c(1,2,3)
  
  # Prints length(ages)x3 matrix of estimates
  # values in matrix should be slightly different due to different arguments 
  # for each function
  
  # Prints inital estimates
  k_tab <- init_data_ests(x,ages)
  print(k_tab) 
  
  # Prints estimates for both inc_known_init cases. Should be different from
  # each other but not too signifigant
  print(init_prob_ests(k_tab, length(ages), T, x[x$Age == -1, ], 
                       x[x$Age != -1, ], ages))
  print(init_prob_ests(k_tab, length(ages), F, x[x$Age == -1, ], 
                       x[x$Age != -1, ], ages))
  
  # Contains two Columns of data one is Length and len(age) cols of probabilites
  # There are apply errors when only one column is specified.
  # First case is known ages, 2nd case is all ages, 3rd is all unknown ages
  print(head(prob_ests(x[x$Age != -1, ], k_tab, length(ages))))
  print(head(prob_ests(x, k_tab, length(ages))))
  prob_tab_test <- prob_ests(x[x$Age == -1, ], k_tab, length(ages))
  print(head(prob_tab_test))
  
  # Prints length(ages)x3 matrix of estimates
  # values in matrix should be slightly different due to different arguments 
  # for each function
  print(max_ests(prob_tab_test, k_tab, length(ages), F, x[x$Age != -1, ]))
  print(max_ests(prob_tab_test, k_tab, length(ages), T, x[x$Age != -1, ]))
  
  # Prints a likelihood, should be negatively values 
  print(likelihood(x[x$Age != -1, ], k_tab, length(ages)))
}
#test_functions()



