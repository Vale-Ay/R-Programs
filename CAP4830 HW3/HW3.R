#=============================================================================
# PROGRAMMER: Valeria Aybar
#
# CLASS: CAP4830
# SECTION: RVC 1231
# SEMESTER: Spring 2023
# CLASSTIME: Online
# CERTIFICATION: I understand FIU’s academic policies, and I certify that this
# work is my own and that none of it is the work of any other person
#=============================================================================

rm(list = ls()) #clearing all objects from session

library(triangle)

#1 create data frame named inputs with column names (q1, q2, p,s)
inputs <- data.frame(q1 = rtriangle(2000, a = 0, b = 1500, c = 1200),
                     q2 = rtriangle(2000, a = 0, b = 3500, c = 1000),
                     p = rtriangle(2000, a = 10, b = 17.50, c = 12.50),
                     s = rexp(2000, 10))

#2 plot histograms in a single window
par(mfrow=c(4, 1)) 
options(scipen = 999)

hist(inputs$q1)
hist(inputs$q2)
hist(inputs$p)
hist(inputs$s)

#3 create data frame named outputs with a column name value

outputs<- data.frame(value = (2700 - inputs$q1-inputs$q2) *inputs$p - (inputs$s*inputs$p))

#4 run a MonteCarlo Simulation 100 times

for(i in 1:1000){
  
  # store the results with outputs 
  q1 <- sample(inputs$q1 , 1)
  q2 <- sample(inputs$q2 , 1) 
  p <- sample(inputs$p , 1) 
  s <- sample(inputs$s , 1) 
  
  value <- ((2700 - q1 - q2) * p - (s*p))
  
  # store the results within outputs
  outputs <- rbind(outputs, data.frame(value))
}

#5 plot histogram outputs$value
hist(outputs$value)

#6 create empirical CDF of outputs$value and plot 

empiricalCDF <- ecdf(outputs$value)
plot(empiricalCDF)

#7 calculate the P0,P10... and output values on R-Console
quantile(outputs$value, probs = c(seq(0,1,by = 0.1)))

#8 find P20 to P80 interval values 

px <- data.frame (values = quantile(outputs$value, probs = c(seq(0,1,by = 0.1))))

sprintf("Interval of Interest: [ %.2f , %.2f ]",  px$values[3], px$values[9])

#9 create matrix named storage that stores 100 samples

storage <- matrix(ncol = 100, nrow = 250)

for(i in 1:100){
  
  outputData <- data.frame(value = double())
  for(j in 1:250){
    q1 <- sample(inputs$q1 , 1)
    q2 <- sample(inputs$q2 , 1) 
    p <- sample(inputs$p , 1) 
    s <- sample(inputs$s , 1) 
    
    value <- ((2700 - q1 - q2) * p - (s*p))
    
    outputData <- rbind(outputData, data.frame(value))
  }
  storage[ , i] <- outputData$value
}

#10 convert storage matrix into data frame storage 

storage <- data.frame(storage)

#11 create a data frame named cltData

cltData <- data.frame(mean = colMeans(storage))

#12 plot histograms of cltData 

par(mfrow=c(1,1))
hist(cltData$mean)

#13 Use Shapiro test to check if cltdata is normally distributed 

shapiro.test(cltData$mean)

#14 state if the data in cltData is normally distributed 

sprintf("Considering that the p-value is %.4f it is less than 0.5, which means its significantly different from normal distribution.", 
        shapiro.test(cltData$mean)$p)

#15 calculate the 80% confidence interval and output the lower and upper bounds 

n <- nrow(cltData)
cltMean <- mean(cltData$mean)
cltSD <-sd(cltData$mean)

margin <- qt(0.80, df = n-1)* cltSD/sqrt(n)

lowerBound <- cltMean - margin
higherBound <- cltMean + margin

sprintf("Interval of Interest: [ %.2f ,  %.2f  ]", lowerBound, higherBound)








