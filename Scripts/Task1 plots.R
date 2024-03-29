library(dplyr)
library(ggplot2)

#Renaming 'x' dataset to Fish_Length for convenience 
load("FishLengths.RData") 
Fish_Length <- x

#Scatterplot between Fish Length and Age
Plot1 <- ggplot(Fish_Length, aes(x = Age,y = Length)) +
         geom_point(aes(colour = Age), size = 5) +
         xlab(" Age Category " ) + ylab(" Fish Length in Cm ") +
         theme_dark() + ggtitle(" Scatterplot between Fish Length and Age ") +
         stat_summary(fun.y="mean",
                      colour = "white", geom = "line",group = 1) +
  scale_fill_discrete(name = "Age Category", breaks=c("1","2","3"))

Plot1

#Box-Plot between Fish Length and Age

Fish_Length$Age <- as.factor(Fish_Length$Age)
         
Plot2 <- ggplot(data = Fish_Length[!is.na(Fish_Length$Age),]) + 
          geom_boxplot(aes(Age, Length, fill = "Age"))+
          scale_x_discrete(name = " Age ") + theme_dark()+
          scale_y_continuous(name = "Fish Length in Cm")+
          ggtitle("Boxplot of Fish length and Age")+
          theme(legend.position = "None")
Plot2

#Histogram of Fish Length and it's count 
Plot3 <- ggplot(Fish_Length, aes(x = Length)) +
         geom_histogram(binwidth = 3) +
         ggtitle(" Frequency histogram of Fish Length ") +
         xlab(" Fish Length in Cm")+ ylab(" Count ")

Plot3

#Creating X,Y to get the Normal Curve of Fish Length
X <- seq(14, 90, length.out=1000)
Y <- with(Fish_Length, dnorm(X, mean(Length), sd(Length)))

#Density Histogram of Fish Length
Plot4 <- ggplot(Fish_Length, aes(x = Length, y = ..density.. )) +
         geom_histogram(binwidth = (5), colour = "black", fill = "steelblue") +
         geom_line(data = Fish_Length, aes(x = X, y = Y), color = "white") +
         ggtitle(" Density histogram of Fish Length ") +
         xlab(" Fish Length in Cm")+ ylab(" Density ")+ theme_dark() 

Plot4        
#Density histogram of mixture components superimposed     
#Using the teamEM fucntion results to plot 
source("Scripts/teamEM.R", local = TRUE)

pdf <- function(x, k_tab){
  rowSums(mapply(function(mu,sigma,lambda,x){return(lambda*dnorm(x,mu,sigma))},
                 k_tab$mu, k_tab$sigma, k_tab$lambda, MoreArgs = list(x = x)))
  
}
#For some reason Fish_Length is assigned to be a factor when we require it
#to be numeric, so we use x instead of Fish_Length
results <- teamEM(x) 
ests <- results$estimates 

Plot5 <- ggplot(Fish_Length, aes(x = Length)) + 
         geom_histogram(aes(y = ..density..), binwidth = (3), colour = "black", 
                       fill = " steelblue ") +
        stat_function(fun = pdf, args = list(k_tab = ests), color = "white")+ 
        ggtitle(" Density histogram of Fish Length ") +
        xlab(" Fish Length in Cm")+ ylab(" Density ")+ theme_dark()

Plot5


