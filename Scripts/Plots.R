library(ggplot2)
load("FishLengths.RData")
dat <- x

dat$Age <- as.factor(dat$Age)
ggplot(data = dat[!is.na(dat$Age),]) + geom_boxplot(aes(Age, Length, color = Age))

# Required Plots
plot.required <- function(dat = x){
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
  legend(1, 80, legend = c("Category 1", "Category 2", "Category 3"), 
         col = c("blue", "red", "green"), lty = 1:1, cex = .7)

  # Some guidelines:
  abline(h = max(known(dat)[known(dat)[,3] ==1,2]), col = "blue")
  abline(h = min(known(dat)[known(dat)[,3] ==1,2]), col = "blue", , lty = 3)
  abline(h = max(known(dat)[known(dat)[,3] ==2,2]), col = "red")
  abline(h = min(known(dat)[known(dat)[,3] ==2,2]), col = "red", , lty = 3)
  abline(h = max(known(dat)[known(dat)[,3] ==3,2]), col = "green")
  abline(h = min(known(dat)[known(dat)[,3] ==3,2]), col = "green", lty = 3)
  
  par(mfrow = c(1,1))
}


plot.required()
