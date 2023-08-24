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


# 1) Read excel file and store into modelData
  library("xlsx")   
  modelData <-read.xlsx(file.choose(), 1) 



#2) Output the names of the modelData
  names(modelData)



#3) create model1
  model1 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH 
              + DCOILWTICO_PCH + PCOPPUSDM_PCH +  PCE_PCH 
              + WPU101_PCH +  GPDIC1_PCH +  RRVRUSQ156N_PCH, data = modelData)

  summary(model1)



#5) Plot model1's residual Density Function
  library(e1071)
  plot(density(model1$residuals), 
      main="Density Plot: Model 1 Residuals", 
      ylab="Frequency", 
      sub=paste("Skewness:", round(e1071::skewness(model1$residuals), 2))) 



#6) Check model1's residual normality using Shapiro Test
  shapiro.test(model1$residuals)
  summary(model1)


#7)Create model2 removing regressors with p-value >0.55
  model2 <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH + DCOILWTICO_PCH
             +PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH, data = modelData)

  summary(model2)
             

#9) Calculate prediction accuracy and error rate of model2

  set.seed(100)  
  
  model2Data <- data.frame(modelData[ , c("UNRATE_PCH" ,"DFII10_PCH", "XTEITT01CNM156S_PCH",
                                     "DCOILWTICO_PCH", "PCOPPUSDM_PCH", "PCE_PCH", "WPU101_PCH", "GPDIC1_PCH")] )

  # row indices for training data
  trainingRowIndex <- sample(1:nrow(model2Data), 0.8*nrow(model2Data)) 

  # model training data
  trainingData <- model2Data[trainingRowIndex, ] 

  # test data
  testData  <- model2Data[-trainingRowIndex, ] 

  # build the model
  trainingModel <- lm(UNRATE_PCH ~ DFII10_PCH + XTEITT01CNM156S_PCH + DCOILWTICO_PCH
                    + PCOPPUSDM_PCH + PCE_PCH + WPU101_PCH + GPDIC1_PCH, data = modelData)


  # predict trainingModel on testing data
  distPred <- predict(trainingModel, testData)  

  # make actuals_predicted data frame
  actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                  actuals= testData$UNRATE_PCH,
                                  predicteds=distPred))  



  summary(actuals_preds)
  cor(actuals_preds$actuals,actuals_preds$predicteds) 

#10) Create model3 with only 3 regressors

  model3 <- lm(UNRATE_PCH ~ PCOPPUSDM_PCH + WPU101_PCH + GPDIC1_PCH, data = modelData)
  
  summary(model3)


#11) Create model4 using manual sampling with a training set of 60% of the data and a testing set of 40%

  # setting seed to reproduce results of random sampling
  set.seed(100)  
  
  # row indices for training data
  model4RowIndex <- sample(1:nrow(finalData), 0.6*nrow(finalData)) 
  
  # model training data
  trainingData <- modelData[model4RowIndex , ] 
  
  # test data
  testData  <- modelData[-model4RowIndex , ] 
  
  # building model4
  model4 <- lm(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH 
               + DCOILWTICO_PCH + PCOPPUSDM_PCH +  PCE_PCH 
               + WPU101_PCH +  GPDIC1_PCH +  RRVRUSQ156N_PCH, data = trainingData)
  
  summary(model4)
  

#12) Using model4, predict the values on the 40% testing set and store results in the distPred variable
  
  # predict model4 on testing data
  distPred <- predict(model4, testData)
  
  # print beginning of distPred
  head(distPred)


#13) Using model4 calculate prediction accuracy and error rates then use ggplot to show actual vs predicted

  # make actuals_predicted data frame
  actuals_preds <- data.frame(cbind(index = seq(1: nrow(testData)), 
                                    actuals= testData$UNRATE_PCH,
                                    predicteds=distPred))  
  
  
  # summary of actuals_preds
  summary(actuals_preds)
  
  # If the correlation accuracy is higher, it means that the actuals and predicted 
  # values have similar directional movement
  cor(actuals_preds$actuals,actuals_preds$predicteds) 
  
  library(ggplot2)
  
  # create gg plot with actuals and predicted values
  gg <- ggplot(data = actuals_preds, aes(index))  + 
    geom_point(aes(y = actuals), color = "red") + 
    geom_point(aes(y = predicteds), color = "blue") +
    labs( title = "Actual vs Predicted Values")
  
  # print out gg plot
  gg
  

#14) Run a k-fold cross validation with k=10

  library(caret)
  
  # create the control with the 10 k-fold
  controlled <- trainControl(method = "cv", number = 10)
  
  #model using the controlled k-fold
  k10_model <- train(UNRATE_PCH ~ DFII10_PCH + CPILFESL_PCH + XTEITT01CNM156S_PCH 
                  + DCOILWTICO_PCH + PCOPPUSDM_PCH +  PCE_PCH 
                  + WPU101_PCH +  GPDIC1_PCH +  RRVRUSQ156N_PCH, 
                  data = modelData, method = "lm", trControl = controlled)
  
  # print the k model
  print(k10_model)

  
  
  
  
