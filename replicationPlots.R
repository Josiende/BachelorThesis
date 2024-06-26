install.packages("mlbench")
install.packages("xlsx")
install.packages("quantregForest")
install.packages("alr4")
library(mlbench)
library(quantreg)
library(caret)
library(randomForest)
library(quantregForest)
library(ggplot2)
library(reshape2)
library(alr4)
# function to get 95% prediction interval ---------------------------------
#Get list of all predictions made
allpredictions <- function(dataset, dependentvariable){
  allpredictionsList <- matrix(nrow = 0, ncol = 4)
  for (fold in folds) {
    #separate training from test data
    train_data <- dataset[fold, ]
    test_data <- dataset[-fold, ]
    #build the QRF model
    X_train <- train_data[, -which(names(train_data) == dependentvariable)]
    y_train <- train_data[, dependentvariable]
    qrf_model <- quantregForest(x = X_train, y = y_train, ntree =1000, nodesize= 10)
    
    X_test <- test_data[, -which(names(train_data) == dependentvariable)]
    y_test <- test_data[, dependentvariable]
    predictions <- predict(qrf_model, newdata = X_test, what = quantiles)
    predictionslist<- cbind(y_test,predictions)
    allpredictionsList <- rbind(allpredictionsList,predictionslist )
  }
  allpredictionsList <- as.data.frame(allpredictionsList)
  allpredictionsList$y_pred_interval <- allpredictionsList$`quantile= 0.975`- allpredictionsList$`quantile= 0.025`
  intervals <-allpredictionsList$y_pred_interval
  sort_idx <- order(intervals)
  allpredictionsList <- allpredictionsList[sort_idx, ]
  allpredictionsList$idx <- seq_len(nrow(allpredictionsList))
  
  mean_interval <- (allpredictionsList$`quantile= 0.025` + allpredictionsList$`quantile= 0.975`)/2
  # Center the data as by Meinshaussen (2006)
  allpredictionsList$y_test_centered <- allpredictionsList$y_test - mean_interval
  allpredictionsList$`quantile= 0.5_centered` <- allpredictionsList$`quantile= 0.5` - mean_interval
  allpredictionsList$`quantile= 0.025_centered` <- allpredictionsList$`quantile= 0.025` - mean_interval
  allpredictionsList$`quantile= 0.975_centered` <- allpredictionsList$`quantile= 0.975` - mean_interval
  return( allpredictionsList)
}




# Boston ------------------------------------------------------------------
#dependent variable is "medv"
data("BostonHousing", package = "mlbench")
any(is.na(BostonHousing))
set.seed(1234)  # For reproducibility
quantiles <- c(0.025, 0.975 ,0.5)

#create folds
folds <- createFolds(BostonHousing$medv, k=5, list = TRUE, returnTrain = TRUE)

bostonhousingcentereddata <-allpredictions(BostonHousing, "medv")

#median vs actual plot
ggplot(bostonhousingcentereddata, aes(x = `quantile= 0.5`,y = y_test)) + 
  geom_errorbar(aes(ymin =`quantile= 0.025`, ymax = `quantile= 0.975`), width = .2,color = 'gray55') +
  geom_point(color = "firebrick2") + labs( y = "Observed Values", x ="Fitted values (Conditional mean)")+
  theme_classic()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

#prediction Interval plot
ggplot(bostonhousingcentereddata, aes(x = idx,y = y_test_centered)) + 
  geom_errorbar(aes(ymin =`quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), width = .3,color = 'gray55') +
  geom_ribbon(aes(ymin = `quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), fill = 'gray55', alpha = 0.8) +  # Fill between error bars
  geom_point(color = "firebrick2") + labs( y = "Observed Values and prediction intervals centered", x ="ordered data")+
  theme_classic()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

#download results from provided csv file
data_long <- read.csv("C:/Users/josie/Downloads/Thesis results - Sheet11.csv")
ggplot(data_long, aes(x = Method, y = Loss, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(data = data_long, aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ Quantile, ncol = 7, scales = "free") +
  geom_hline(data = subset(data_long, Method == "QRF"), aes(yintercept = Loss), linetype = "solid", color = "black") +
  labs(
    x = "Method",
    y = "Average Pinball Loss") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("QRF" = "black", "LQR" = "green4", "QQR" = "green4"))+
  labs( y = "Boston") +
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add border around each facet
    strip.background = element_blank(), # Remove background from facet labels
    strip.text.x = element_text() # Add margin to facet titles
  )
# BigMac2003 --------------------------------------------------------------
#load data
data("BigMac2003", package = "alr4")
any(is.na(BigMac2003))
set.seed(1234)  # For reproducibility
quantiles <- c(0.025, 0.975 ,0.5)
#create folds
folds <- createFolds(BigMac2003$BigMac, k=5, list = TRUE, returnTrain = TRUE)
BigMac2003centered <-allpredictions(BigMac2003, "BigMac")

ggplot(BigMac2003centered, aes(x = idx,y = y_test_centered)) + 
  geom_ribbon(aes(ymin = `quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), fill = 'gray55', alpha = 0.8) +  # Fill between error bars
  geom_errorbar(aes(ymin =`quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), width = .3,color = 'gray55') +
  geom_point(color = "firebrick2") + labs( y = "Observed Values and prediction intervals centered", x ="ordered data")+
  theme_classic()+ 
  labs(title = "BigMac")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

#download results from provided csv file
data_long <- read.csv("C:/Users/josie/Downloads/Thesis results - Sheet11.csv")

ggplot(data_long, aes(x = Method, y = Loss, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(data = data_long, aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ Quantile, ncol = 7, scales = "free") +
  geom_hline(data = subset(data_long, Method == "QRF"), aes(yintercept = Loss), linetype = "solid", color = "black") +
  labs(
    x = "Method",
    y = "Average Pinball Loss") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("QRF" = "black", "LQR" = "green4", "QQR" = "green4"))+
  labs( y = "Bigmac") +
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add border around each facet
    strip.background = element_blank(), # Remove background from facet labels
    strip.text.x = element_text() # Add margin to facet titles
  )



# Ozone -------------------------------------------------------------------
#dependentvariable is V4
data("Ozone", package = "mlbench")
any(is.na(Ozone))
Ozone_clean <- na.omit(Ozone)
any(is.na(Ozone_clean))
set.seed(1234)  # For reproducibility
quantiles <- c(0.025, 0.975 ,0.5)
#create folds
folds <- createFolds(Ozone_clean$V4, k=5, list = TRUE, returnTrain = TRUE)
Ozone_cleanCentered <- allpredictions(Ozone_clean, "V4")

ggplot(Ozone_cleanCentered, aes(x = idx,y = y_test_centered)) + 
  geom_ribbon(aes(ymin = `quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), fill = 'gray55', alpha = 0.8) +  # Fill between error bars
  geom_errorbar(aes(ymin =`quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), width = .3,color = 'gray55') +
  geom_point(color = "firebrick2") + labs( y = "Observed Values and prediction intervals centered", x ="ordered data")+
  theme_classic()+ 
  labs(title = "Ozone")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

#download results from provided csv file
data_long <- read.csv("C:/Users/josie/Downloads/Thesis results - Sheet11.csv")
ggplot(data_long, aes(x = Method, y = Loss, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(data = data_long, aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ Quantile, ncol = 7, scales = "free") +
  geom_hline(data = subset(data_long, Method == "QRF"), aes(yintercept = Loss), linetype = "solid", color = "black") +
  labs(
    x = "Method",
    y = "Average Pinball Loss") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("QRF" = "black", "LQR" = "green4", "QQR" = "green4"))+
  labs( y = "Ozone") +
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add border around each facet
    strip.background = element_blank(), # Remove background from facet labels
    strip.text.x = element_text() # Add margin to facet titles
  )


# Fuel --------------------------------------------------------------------
data("fuel2001", package = "alr4")
# Most of the examples in ALR3 that use these data first 
# transform several of the columns
totalMilesDriven <- fuel2001$MPC*fuel2001$Pop
totalGallonsSold <- fuel2001$FuelC*1000
fuel2001$ratio <-totalGallonsSold/totalMilesDriven
set.seed(1234)  # For reproducibility
quantiles <- c(0.025, 0.975 ,0.5)
#create folds
folds <- createFolds(fuel2001$ratio , k=5, list = TRUE, returnTrain = TRUE)


fuel2001transformedcentered <- allpredictions(fuel2001, "ratio")
ggplot(fuel2001transformedcentered, aes(x = idx,y = y_test_centered)) + 
  geom_ribbon(aes(ymin = `quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), fill = 'gray55', alpha = 0.8) +  # Fill between error bars
  geom_errorbar(aes(ymin =`quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), width = .3,color = 'gray55') +
  geom_point(color = "firebrick2") + labs( y = "Observed Values and prediction intervals centered", x ="ordered data")+
  theme_classic()+ 
  labs(title = "Fuel")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))


#download results from provided csv file
data_long <- read.csv("C:/Users/josie/Downloads/Thesis results - Sheet11.csv")
ggplot(data_long, aes(x = Method, y = Loss, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(data = data_long, aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ Quantile, ncol = 7, scales = "free") +
  geom_hline(data = subset(data_long, Method == "QRF"), aes(yintercept = Loss), linetype = "solid", color = "black") +
  labs(
    x = "Method",
    y = "Average Pinball Loss") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("QRF" = "black", "LQR" = "green4", "QQR" = "green4"))+
  labs( y = "Fuel") +
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add border around each facet
    strip.background = element_blank(), # Remove background from facet labels
    strip.text.x = element_text() # Add margin to facet titles
  )



# Abalone -----------------------------------------------------------------
#dependent variable is rings
abalone_data <- read.table("C:/Users/563435jd/Downloads/abalone.data", sep=",", header=FALSE)
colnames(abalone_data) <- c("Sex", "Length", "Diameter", "Height", "Whole_weight", "Shucked_weight", "Viscera_weight", "Shell_weight", "Rings")
#take 500 random rows
set.seed(1234)
randomRows <- sample(1: nrow(abalone_data),500)
abalone_subSet <- abalone_data[randomRows,]

set.seed(1234)  # For reproducibility
quantiles <- c(0.025, 0.975 ,0.5)
#create folds
folds <- createFolds(abalone_subSet$Rings, k=5, list = TRUE, returnTrain = TRUE)
abalone_subSetCentered <- allpredictions(abalone_subSet, "Rings")

ggplot(abalone_subSetCentered, aes(x = idx,y = y_test_centered)) + 
  geom_ribbon(aes(ymin = `quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), fill = 'gray55', alpha = 0.8) +  # Fill between error bars
  geom_errorbar(aes(ymin =`quantile= 0.025_centered`, ymax = `quantile= 0.975_centered`), width = .3,color = 'gray55') +
  geom_point(color = "firebrick2") + labs( y = "Observed Values and prediction intervals centered", x ="ordered data")+
  labs(title = "Abalone")+
  theme_classic()+ 
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

#download results from provided csv file
data_long <- read.csv("C:/Users/josie/Downloads/Thesis results - Sheet11.csv")
ggplot(data_long, aes(x = Method, y = Loss, color = Method)) +
  geom_point(size = 3) +
  geom_errorbar(data = data_long, aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  facet_wrap(~ Quantile, ncol = 7, scales = "free") +
  geom_hline(data = subset(data_long, Method == "QRF"), aes(yintercept = Loss), linetype = "solid", color = "black") +
  labs(
    x = "Method",
    y = "Average Pinball Loss") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_color_manual(values = c("QRF" = "black", "LQR" = "green4", "QQR" = "green4"))+
  labs( y = "Boston") +
  theme_classic() +
  theme(
    legend.position = "none", # Remove legend
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add border around each facet
    strip.background = element_blank(), # Remove background from facet labels
    strip.text.x = element_text() # Add margin to facet titles
  )
