install.packages("mlbench")
install.packages("alr4")
library(mlbench)
library(alr4)
library(quantreg)
library(caret)
library(quantregForest)

#First please load all functions:
# Noise function ----------------------------------------------------------
add_noise <- function(data, exclude_var) {
  set.seed(1234)
  noisy_data <- data
  for (col in colnames(data)) {
    if (col != exclude_var) {
      noisy_data[[paste0(col, "_noisy")]] <- sample(data[[col]])
    }
  }
  return(noisy_data)
}
# Loss function -----------------------------------------------------------
#loss function as described in Meinshaussen (2006)
LossFunction <- function(actual, predicted, tau) {
  residuals <- actual - predicted
  loss <- ifelse(residuals >= 0, tau * abs(residuals), (1-tau) * abs(residuals))
  return(mean(loss))
}

# Add to formula function ----------------------------------------------------
#necessary for QQR to add interaction term to formula
addToFomula <- function(original, interactionTerm){
  asText<- as.character(original)
  terms <- asText[3]
  #add interaction term to formula 
  terms <- terms(terms, interactionTerm, sep = " + ")
  asTextNew <- paste(asText[2], "~", terms)
  newformula <-as.formula(asTextNew)
  return(newformula)
}

# QRF Function----------------------------------------------------------------
evaluationQRF <- function(dataset, dependentvariable){
  LossMatrix <- matrix(c(quantiles), nrow = length(quantiles), ncol = 1)
  for (fold in folds) {
    losses <-list()
    #train and test data
    train_data <- dataset[fold, ]
    test_data <- dataset[-fold, ]
    #build qrf model
    X_train <- train_data[, -which(names(train_data) == dependentvariable)]
    y_train <- train_data[, dependentvariable]
    qrf_model <- quantregForest(x = X_train, y = y_train, ntree =1000, 
                                nodesize= 10)
    
    X_test <- test_data[, -which(names(train_data) == dependentvariable)]
    y_test <- test_data[, dependentvariable]
    
    # Predict for each quantile
    for(q in quantiles){
      predictions <- predict(qrf_model, newdata = X_test, what = q)
      loss <- LossFunction(actual = y_test, predicted = predictions, tau = q)
      losses <- c(losses,as.numeric(loss))
    }
    LossMatrix <- cbind(LossMatrix,as.numeric(losses))
  }
  #get average loss
  row_avg <- rowMeans(LossMatrix[, -1], na.rm = TRUE)
  LossMatrixQRF <- cbind(LossMatrix, Row_Average = row_avg)
  colnames(LossMatrixQRF) <- 
    c("Quantiles", "fold1", "fold2", "fold3", "fold4", "fold5", "average_Loss")
  return(LossMatrixQRF)
}


# LQR Function ----------------------------------------------------------------
evaluationLQR <- function(dataset, dependentvariable){
  LossMatrix <- matrix(c(quantiles), nrow = length(quantiles), ncol = 1)
  for (fold in folds){
    losses <-list()
    #train and test data
    train_data <- dataset[fold, ]
    test_data <- dataset[-fold, ]
    y_test <- test_data[, dependentvariable]

    for(q in quantiles){
      formulaText <- as.formula(paste(dependentvariable, "~ ."))
      LinQuantReg <- rq(formulaText, data = train_data, tau = q)
      predictions <- predict(LinQuantReg,newdata = test_data)
      loss <- LossFunction(actual = y_test, predicted = predictions, tau = q)
      losses <- c(losses,as.numeric(loss))
    }
    LossMatrix <- cbind(LossMatrix,as.numeric(losses))
  }
  row_avg <- rowMeans(LossMatrix[, -1], na.rm = TRUE)
  LossMatrixLQR <- cbind(LossMatrix, Row_Average = row_avg)
  colnames(LossMatrixLQR) <- 
    c("Quantiles", "fold1", "fold2", "fold3", "fold4", "fold5", "average_Loss")
  return(LossMatrixLQR)
}

# QQR Function ---------------------------------------------------------------------
evaluationQQR <-function(dataset, dependentvariable){
 
  #take random 4/5 of dataset to fit QQR on
  set.seed(1234)
  random_fold <- sample(folds, 1)
  dataset_subset <- dataset[random_fold[[1]], ]
  variable <- dependentvariable
  #find original LQR errors
  LQRResult <-evaluationLQR(dataset_subset, variable)
  
  #create list of allpossible interactionterms
  AllPredictors <- names(dataset_subset)[names(dataset_subset) != variable]
  AllPossibleInteractions <- combn(AllPredictors, 2, function(x) paste(x, collapse = ":")) 
  AllPossibleInteractions <- as.list(AllPossibleInteractions)
  
  
  #create folds for crossvalidation qqr, size of fold is trainingSet
  set.seed(1234)
  foldsQQR <- createFolds(dataset_subset[,variable], k=5, list = TRUE, returnTrain = TRUE)
  original <-  as.formula(paste(dependentvariable, "~ ."))
  QQRresults <- matrix(nrow =0, ncol = 7)
  
  i<-0
  for(q in quantiles){
    i <- i+1
    currentFormula <- original
    originalError <-as.numeric(LQRResult[i, 7])
    BestError<-originalError
    bestFormula <- original
    bestLoss <- as.numeric(LQRResult[i,c(2,3,4,5,6)])
    termToadd <- "NA"
    print(q)
    print(originalError)
    for(k in 1:length(AllPossibleInteractions)){
      print(currentFormula)
      for(interaction in AllPossibleInteractions){
        newFormula <- addToFomula(currentFormula, interaction)
        LossMatrix <- list()
        for (fold in foldsQQR){
          #separate training from test data
          train_data <- dataset_subset[fold, ]
          test_data <- dataset_subset[-fold, ]
          y_test <- test_data[,variable]
          LinQuantReg <- rq(newFormula, data = train_data, tau =q)
          predictions <- predict(LinQuantReg,newdata = test_data)
          loss <- LossFunction(actual = y_test, predicted = predictions, tau = q)
          LossMatrix <- c(LossMatrix,loss)
        }
        if(mean(as.numeric(LossMatrix)) < BestError){
          BestError <-  mean(as.numeric(LossMatrix))
          bestFormula <- newFormula
          termToadd <- interaction
          bestLoss <- LossMatrix
        }
      }
      # if no improvement in error break
      if(currentFormula == bestFormula){
        break
      }
      # add best interaction term based on CV to formula
      currentFormula <- bestFormula
      
    }
    #test qqr on full dataset
    print(bestFormula)
    losses <-list()
    for(foldsALL in folds){
      train_data <- dataset[foldsALL, ]
      test_data <- dataset[-foldsALL, ]
      y_test <- test_data[, dependentvariable]
      LinQuantReg <- rq(bestFormula, data = train_data, tau = q)
      predictions <- predict(LinQuantReg,newdata = test_data)
      loss <- LossFunction(actual = y_test, predicted = predictions, tau = q)
      losses <- c(losses,as.numeric(loss))
    }
    print(losses)
    
    
    QQRresults <- rbind(QQRresults, c(q,bestFormula, unlist(losses)))
  }
  return(QQRresults)
}
# bootstrapping --------------------------------------------------
#bootstrap function for error terms
bootstrapCI <- function(losses) {
  boot_means <- replicate(1000, {
    sample_losses <- sample(losses, length(losses), replace = TRUE)
    mean(sample_losses)
  })
  lower <- quantile(boot_means, 0.05 / 2)
  upper <- quantile(boot_means, 1 - 0.05 / 2)
  return(c(lower, upper))
}
# Boston housing ----------------------------------------------------------
#dependent variable is medv
data("BostonHousing", package = "mlbench")
any(is.na(BostonHousing))
BostonHousingNoisy <- add_noise(BostonHousing, "medv")

quantiles <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)
#create CV folds, fold is trainingset
set.seed(1234)
folds <- createFolds(BostonHousing$medv, k=5, list = TRUE, returnTrain = TRUE)

#QRF evaluation
BostonQRF <-evaluationQRF(BostonHousing, "medv")
write.csv(BostonQRF, file ="BostonQRF_CrossValidationResults.csv")
BostonNoisyQRF <-evaluationQRF(BostonHousingNoisy, "medv")
write.csv(BostonNoisyQRF, file ="BostonNoisyQRF_CrossValidationResults.csv")

#LQR evaluation
BostonLQR <-evaluationLQR(BostonHousing, "medv")
write.csv(BostonLQR, file ="BostonLQR_CrossValidationResults.csv")
BostonNoisyLQR <-evaluationLQR(BostonHousingNoisy, "medv")
write.csv(BostonNoisyLQR, file ="BostonNoisyLQR_CrossValidationResults.csv")

#QQR evaluation
BostonQQR <- evaluationQQR(BostonHousing, "medv")
write.csv(BostonQQR,file ="BostonQQR_CrossValidationResults.csv")
BostonNoisyQQR <- evaluationQQR(BostonHousingNoisy, "medv")
write.csv(BostonNoisyQQR, file ="BostonNoisyQQR_CrossValidationResults.csv" )

# replace BostonLQR with all variations of lqr and qqr
intervals <- matrix(ncol =2 , nrow=0)
for(i in 1:7){
  boot_cis <- bootstrapCI(BostonQQR[i,2:6])
  intervals <- rbind(intervals,boot_cis)
}

# BigMac ------------------------------------------------------------------
#load data, dependent variable is BigMac
data("BigMac2003", package = "alr4")
any(is.na(BigMac2003))
BigMac2003Noisy <- add_noise(BigMac2003, "BigMac")
quantiles <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)

#create folds, size of fold is trainingSet
set.seed(1234)
folds <- createFolds(BigMac2003$BigMac, k=5, list = TRUE, returnTrain = TRUE)

#QRF evaluation
BigMacQRF <- evaluationQRF(BigMac2003, "BigMac")
write.csv(BigMacQRF, file ="BigMacQRF_CrossValidationResults.csv")
BigMacQRFNoisy <- evaluationQRF(BigMac2003Noisy, "BigMac")
write.csv(BigMacQRFNoisy, file ="BigMacNoisyQRF_CrossValidationResults.csv")

#LQR evaluation
BigMacLQR <- evaluationLQR(BigMac2003, "BigMac")
write.csv(BigMacLQR, file ="BigMacLQR_CrossValidationResults.csv")
BigMacLQRNoisy <- evaluationLQR(BigMac2003Noisy, "BigMac")
write.csv(BigMacLQRNoisy, file ="BigMacNoisyLQR_CrossValidationResults.csv")

#QQR evaluation 
BigMacQQR <- evaluationQQR(BigMac2003, "BigMac")
write.csv(BigMacQQR, file ="BigMacQQR_CrossValidationResults.csv")
BigMacQQRNoisy <- evaluationQQR(BigMac2003Noisy, "BigMac")
write.csv(BigMacQQRNoisy, file ="BigMacNoisyQQR_CrossValidationResults.csv")

intervals <- matrix(ncol =2 , nrow=0)
for(i in 1:7){
  boot_cis <- bootstrapCI(BigMacLQRNoisy[i,2:6])
  intervals <- rbind(intervals,boot_cis)
}

# Abalone -----------------------------------------------------------------
#dependentvariable is rings

abalone_data <- read.table("C:/Users/josie/Downloads/abalone.data", sep=",", 
                           header=FALSE)
colnames(abalone_data) <- 
  c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight",
    "Viscera_weight", "Shell_weight", "Rings")
#take 500 random rows
set.seed(1234)
randomRows <- sample(1: nrow(abalone_data),500)
abalone_subSet <- abalone_data[randomRows,]
abalone_subSetNoisy <- add_noise(abalone_subSet, "Rings")

quantiles <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)

#create folds
set.seed(1234)
folds <- createFolds(abalone_subSet$Rings, k=5, list = TRUE, returnTrain = TRUE)


#QRF evaluation
abalone_subSetQRF <- evaluationQRF(abalone_subSet, "Rings")
write.csv(abalone_subSetQRF, file = "abalone_subSetQRF_CrossValidationResults.csv")
abalone_subSetQRFNoisy <- evaluationQRF(abalone_subSetNoisy, "Rings")
write.csv(abalone_subSetQRFNoisy, file = "abalone_subSetQRFNoisy_CrossValidationResults.csv")

#LQR evaluation
abalone_subSetLQR <- evaluationLQR(abalone_subSet, "Rings")
write.csv(abalone_subSetLQR, file = "abalone_subSetLQR_CrossValidationResults.csv")
abalone_subSetLQRNoisy <- evaluationLQR(abalone_subSetNoisy, "Rings")
write.csv(abalone_subSetLQRNoisy, file = "abalone_subSetLQRNoisy_CrossValidationResults.csv")

#QQR evaluation
abalone_subSetQQR <- evaluationQQR(abalone_subSet, "Rings")
write.csv(abalone_subSetQQR, file = "abalone_subSetQQR_CrossValidationResults.csv")
abalone_subSetQQRNoisy <- evaluationQQR(abalone_subSetNoisy, "Rings")
write.csv(abalone_subSetQQRNoisy, file = "abalone_subSetQQRNoisy_CrossValidationResults.csv")

intervals <- matrix(ncol =2 , nrow=0)
for(i in 1:7){
  boot_cis <- bootstrapCI(abalone_subSetLQRNoisy[i,2:6])
  intervals <- rbind(intervals,boot_cis)
}
# Ozone -------------------------------------------------------------------
#dependentvariable is V4
Ozone_clean<- read.csv("C:/Users/josie/Downloads/ozone.csv")
Ozone_cleanNoisy <- add_noise(Ozone_clean, "ozoneReading")

quantiles <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)

#create folds, size of fold is trainingSet
set.seed(1234)
folds <- createFolds(Ozone_clean$ozoneReading, k=5, list = TRUE, returnTrain = TRUE)


#QRF evaluation
Ozone_cleanQRF <- evaluationQRF(Ozone_clean, "ozoneReading")
write.csv(Ozone_cleanQRF, file = "ozone_cleanQRF_CrossValidationResults.csv")
ozone_cleanQRFNoisy <- evaluationQRF(Ozone_cleanNoisy, "ozoneReading")
write.csv(ozone_cleanQRFNoisy, file = "ozone_cleanQRFNoisy_CrossValidationResults.csv")

#LQR evaluation
ozone_cleanLQR <- evaluationLQR(Ozone_clean, "ozoneReading")
write.csv(ozone_cleanLQR, file = "ozone_cleanLQR_CrossValidationResults.csv")
ozone_cleanLQRNoisy <- evaluationLQR(Ozone_cleanNoisy, "ozoneReading")
write.csv(ozone_cleanLQRNoisy, file = "ozone_cleanLQRNoisy_CrossValidationResults.csv")

#QQR evaluation
ozone_cleanQQR <- evaluationQQR(Ozone_clean, "ozoneReading")
write.csv(ozone_cleanQQR, file = "ozone_cleanQQR_CrossValidationResults.csv")
ozone_cleanQQRNoisy <- evaluationQQR(Ozone_cleanNoisy, "ozoneReading")
write.csv(ozone_cleanQQRNoisy, file = "ozone_cleanQQRNoisy_CrossValidationResults.csv")



intervals <- matrix(ncol =2 , nrow=0)
for(i in 1:7){
  boot_cis <- bootstrapCI(ozone_cleanLQRNoisy[i,2:6])
  intervals <- rbind(intervals,boot_cis)
}
# Fuel --------------------------------------------------------------------
data("fuel2001", package = "alr4")
#calculate ratio
totalMilesDriven <- fuel2001$MPC*fuel2001$Pop
totalGallonsSold <- fuel2001$FuelC*1000
fuel2001$ratio <-totalGallonsSold/totalMilesDriven
fuel2001$FuelC <- NULL
fuel2001$MPC <- NULL
fuel2001Noisy <- add_noise(fuel2001, "ratio")

quantiles <- c(0.005, 0.025, 0.05, 0.5, 0.95, 0.975, 0.995)
#create folds, size of fold is trainingSet
set.seed(1234)
folds <- createFolds(fuel2001$ratio, k=5, list = TRUE, returnTrain = TRUE)

#QRF evaluation
fuel2001QRF <- evaluationQRF(fuel2001, "ratio")
write.csv(fuel2001QRF, file = "fuel2001QRF_CrossValidationResults.csv")
fuel2001QRFNoisy <- evaluationQRF(fuel2001Noisy, "ratio")
write.csv(fuel2001QRFNoisy, file = "fuel2001QRFNoisy_CrossValidationResults.csv")

#LQR evaluation
fuel2001LQR <- evaluationLQR(fuel2001, "ratio")
write.csv(fuel2001LQR, file = "fuel2001LQR_CrossValidationResults.csv")
fuel2001LQRNoisy <- evaluationLQR(fuel2001Noisy, "ratio")
write.csv(fuel2001LQRNoisy, file = "fuel2001LQRNoisy_CrossValidationResults.csv")

#QQR evaluation
#werkt nog niet?
fuel2001QQR <- evaluationQQR(fuel2001, "ratio")
write.csv(fuel2001QQR,file ="fuel2001QQR_CrossValidationResults.csv")
fuel2001QQRNoisy <- evaluationQQR(fuel2001Noisy, "ratio")
write.csv(fuel2001QQRNoisy, file ="fuel2001NoisyQQR_CrossValidationResults.csv" )


intervals <- matrix(ncol =2 , nrow=0)
for(i in 1:7){
  boot_cis <- bootstrapCI(fuel2001LQRNoisy[i,2:6])
  intervals <- rbind(intervals,boot_cis)
}

