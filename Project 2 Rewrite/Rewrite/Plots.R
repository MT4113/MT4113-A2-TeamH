

library(dplyr)
#Renaming 'x' dataset to Fish_Length for convenience 
Fish_Length <- x

library(ggplot2)
#BarPlot between Fish Length and Age
Plot1 <- ggplot(data = Fish_Length, aes(x = Age, y = Length)) +
         geom_bar(stat= "identity", fill = " steelblue ") +
         xlab(" Age ") + ylab(" Fish Length in Cm ")+ 
         theme_minimal() + ggtitle(" Barplot between Fish Length and Age")
Plot1
#Scatterplot between Fish Length and Age
Plot2 <- ggplot(Fish_Length, aes(Age, Length))+
         geom_point(aes(colour = factor(Age)))+
         xlab(" Age" ) + ylab(" Fish Length in Cm ")+
         theme_dark() + ggtitle(" Scatterplot between Fish Length and Age")
Plot2

#Histogram of Fish Length and it's count 
Plot2 <- ggplot(Fish_Length, aes(x = Length)) +
         geom_histogram(binwidth = 3) +
         ggtitle(" Frequency histogram of Fish Length ") +
         xlab(" Fish Length in Cm")+ ylab(" Count ")
Plot2
#Density Histogram of Fish Length          
Plot3 <- ggplot(Fish_Length, aes(x = Length)) +
         geom_histogram(binwidth = (5), colour = "black", fill = "white")
         ggtitle(" Frequency histogram of Fish Length ") +
         xlab(" Fish Length in Cm")+ ylab(" Count ")+ theme_dark()
        
Plot3        
        
      
                 