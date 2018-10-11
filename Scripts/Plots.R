library(ggplot2)
load("FishLengths.RData")
dat <- x

dat$Age <- as.factor(dat$Age)
ggplot(data = dat[!is.na(dat$Age),]) + geom_boxplot(aes(Age, Length, color = Age))

# Plot for Visualisation of Overlap between Categories:
plot.understanding <- function(){
  # In function to keep Run clean.
  par(mfrow = c(3,1))
  hist(knownage[,2], xlab = "Fish Length (cm)", ylab = "Frequency", 
       main = "Histogram of Known")
  hist(fishdata[,2], xlab = "Fish Length (cm)", ylab = "Frequency", 
       main = "Histogram of All")
  
  plot(knownage$Age, knownage$Length, xlab = "Age Category", 
       ylab = "Fish Length")
  
  abline(h = max(knownage[knownage$Age ==1,2]), col = "blue")
  abline(h = min(knownage[knownage$Age ==1,2]), col = "blue", , lty = 3)
  abline(h = max(knownage[knownage$Age ==2,2]), col = "red")
  abline(h = min(knownage[knownage$Age ==2,2]), col = "red", , lty = 3)
  abline(h = max(knownage[knownage$Age ==3,2]), col = "green")
  abline(h = min(knownage[knownage$Age ==3,2]), col = "green", lty = 3)
  
  par(mfrow = c(1,1))
}
plot.understanding()
