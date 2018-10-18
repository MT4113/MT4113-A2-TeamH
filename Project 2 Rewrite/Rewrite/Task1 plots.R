

library(dplyr)
#Renaming 'x' dataset to Fish_Length for convenience 
Fish_Length <- x

library(ggplot2)
#Scatterplot between Fish Length and Age
Plot1 <- ggplot(Fish_Length, aes(Age, Length))+
         geom_point(aes(colour = factor(Age)))+
         xlab(" Age" ) + ylab(" Fish Length in Cm ")+
         theme_dark() + ggtitle(" Scatterplot between Fish Length and Age")+
         stat_summary(fun.y="mean", geom="point")
Plot1

#Histogram of Fish Length and it's count 
Plot2 <- ggplot(Fish_Length, aes(x = Length)) +
         geom_histogram(binwidth = 3) +
         ggtitle(" Frequency histogram of Fish Length ") +
         xlab(" Fish Length in Cm")+ ylab(" Count ")

Plot2

#Creating X,Y to get the Normal Curve of Fish Length
X <- seq(14, 90, length.out=1000)
Y <- with(Fish_Length, dnorm(X, mean(Length), sd(Length)))
lines(X, Y, col = "red")

#Density Histogram of Fish Length
Plot3 <- ggplot(Fish_Length, aes(x = Length, y = ..density.. )) +
         geom_histogram(binwidth = (5), colour = " black ", fill = " steelblue ") +
         geom_line(data = Fish_Length, aes(x = X, y = Y), color = "white") +
         ggtitle(" Density histogram of Fish Length ") +
         xlab(" Fish Length in Cm")+ ylab(" Density ")+ theme_dark() 

Plot3        
     

      
        
      

                 