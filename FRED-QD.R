install.packages("remotes")
remotes::install_github("cykbennie/fbi")

install.packages("ggplot2")
install.packages("tseries")
install.packages("forecast")
install.packages("quantreg")
install.packages("quantregForest")
install.packages("dplyr")

library(fbi)
library(ggplot2)
library(tseries)
library(forecast)
library(quantreg)
library(quantregForest)
library(dplyr)
library(tsDyn)
library(lubridate)

# Data --------------------------------------------------------------------
# 50% and 95% prediction interval and median point forecast
quantiles <- c(0.025,0.25,0.75, 0.975 ,0.5)

#load and transform the data from Federal Reserve
dfqua <- fredqd(file = "https://files.stlouisfed.org/files/htdocs/fred-md/quarterly/2024-04.csv",
                #transforms data as described in (McCracken & Ng, 2020)
                transform = FALSE, 
                date_start =as.Date("1966-03-01"))



#calculate missing values
totalvalues <- 233*245
missingValues <- sum(is.na(dfqua[45:nrow(dfqua),]))
precetageMissing <-missingValues/totalvalues*100

#impute missing values as by Coulombe (2020)
dates <- dfqua$date
df_imputed <- dfqua
df_imputed$date <- NULL
colnames <- colnames(df_imputed)
df_imputed <- tw_apc(X = df_imputed,
                     kmax = 0,
                     center = TRUE,
                     standardize = TRUE)[['data']]
df_imputed <- data.frame(df_imputed)
colnames(df_imputed) <- colnames


df_imputed$withDate <- dates

#four lags of dependent variable
GDP_lagged <- df_imputed%>%select(GDPC1) %>%mutate(
  GDPC1_1 = lag(GDPC1,1),
  GDPC1_2 = lag(GDPC1,2),
  GDPC1_3 = lag(GDPC1,3),
  GDPC1_4 = lag(GDPC1,4))
df_PredictGDP <- cbind(df_imputed, GDP_lagged[,c(2,3,4,5)])

CPIAUCSL_lagged <- df_imputed%>%select(CPIAUCSL) %>%mutate(
  CPIAUCSL_1 = lag(CPIAUCSL,1),
  CPIAUCSL_2 = lag(CPIAUCSL,2),
  CPIAUCSL_3 = lag(CPIAUCSL,3),
  CPIAUCSL_4 = lag(CPIAUCSL,4))
df_PredictCPIAUCSL <- cbind(df_imputed, CPIAUCSL_lagged[,c(2,3,4,5)])


CPILFESL_lagged <- df_imputed%>%select(CPILFESL) %>%mutate(
  CPILFESL_1 = lag(CPILFESL,1),
  CPILFESL_2 = lag(CPILFESL,2),
  CPILFESL_3 = lag(CPILFESL,3),
  CPILFESL_4 = lag(CPILFESL,4))
df_PredictCPILFESL <- cbind(df_imputed, CPILFESL_lagged[,c(2,3,4,5)])

# test for stationarity 
adf_testhead <- adf.test(df_imputed$CPIAUCSL, alternative = "stationary")
print(adf_testhead)
adf_testGDP <- adf.test(df_imputed$GDPC1, alternative = "stationary")
print(adf_testGDP)
adf_testcore <- adf.test(df_imputed$CPILFESL, alternative = "stationary")
print(adf_testcore)

#plot the dependent variables
ggplot(data = df_imputed, aes(x= date, y= CPIAUCSL))+geom_line(color = "pink4")+
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

ggplot(data = df_imputed, aes(x= withDate, y= GDPC1))+geom_line(color = "steelblue4")+
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

ggplot(data = df_imputed, aes(x= date, y= CPILFESL))+geom_line(color = "orange4")+
  theme_classic()+ 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))

df_imputed$date <- NULL

# AR4 ---------------------------------------------------------------------
AR4Fred <-function(horizon,y_variable,confidence){
  df<- cbind(date = dates, df_imputed)
  if (horizon !=1) {
    df <- df[(17-horizon+1):nrow(df_imputed),]
  } else {
    df <- df[17:nrow(df_imputed),]
  }
  # Initialize parameters
  n_train <- 120
  n_test <- 96
  total_obs <- nrow(df)
  
  # Extract the target variable
  y <- df[,y_variable]
  
  # Initialize a list to store predictions
  predictions <- list(
    median = numeric(n_test),
    low = numeric(n_test),
    high = numeric(n_test)
  )
  
  # Starting date for predictions
  start_date <- ymd("2000-03-01")
  
  for (i in 1:n_test) {
    train_start <- i
    train_end <- i + n_train - 1
    # Train the AR(4) model
    train_data <- y[train_start:train_end]
    ar_model <- Arima(train_data, order = c(4, 0, 0))
    prediction <-  forecast(ar_model, h = horizon, level = confidence)
    
    if (horizon !=1) {
      forecast_median <- prediction$mean[horizon]
      forecast_low<- prediction$lower[horizon]
      forecast_up <- prediction$upper[horizon]
      
    } else {
      forecast_median <- prediction$mean
      forecast_low<- prediction$lower
      forecast_up <- prediction$upper
    }
    
    predictions$median[i] <- forecast_median
    predictions$low[i] <- forecast_low
    predictions$high[i] <- forecast_up
  }
  pred_df <- data.frame(
    date = seq.Date(start_date, by = "quarter", length.out = n_test),
    actual = y[(n_train + horizon):(n_train + n_test+horizon-1)],
    median =predictions$median,
    low =predictions$low,
    high =predictions$high
  )
  return(pred_df)
}
AR4H1GDP <- AR4Fred(horizon = 1,"GDPC1", confidence =50)
#write.csv(AR4H1GDP, file = "AR4H1GDP.csv")
AR4H2GDP <- AR4Fred(horizon =2,"GDPC1", confidence =50)
#write.csv(AR4H2GDP, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H2GDP.csv")
AR4H4GDP<- AR4Fred(horizon = 4,"GDPC1",  confidence =50)
#write.csv(AR4H4GDP, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H4GDP.csv")

AR4H1headlineInflation <- AR4Fred(horizon = 1,"CPIAUCSL", confidence =50)
#write.csv(AR4H1headlineInflation, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H1headlineInflation.csv")
AR4H2headlineInflation <- AR4Fred(horizon =2,"CPIAUCSL", confidence =50)
#write.csv(AR4H2headlineInflation, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H2headlineInflation.csv")
AR4H4headlineInflation<- AR4Fred(horizon = 4,"CPIAUCSL",  confidence =50)
#write.csv(AR4H4headlineInflation, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H4headlineInflation.csv")

AR4H1coreInflation <- AR4Fred(horizon = 1,"CPILFESL", confidence =50)
#write.csv(AR4H1coreInflation, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H1coreInflation.csv")
AR4H2coreInflation <- AR4Fred(horizon =2,"CPILFESL", confidence =50)
#write.csv(AR4H2coreInflation, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H2coreInflation.csv")
AR4H4coreInflation<- AR4Fred(horizon = 4,"CPILFESL",  confidence =50)
#write.csv(AR4H4coreInflation, file = "C:/Users/josie/OneDrive/studie/year 4/Thesis/output R/AR4model/AR4H4coreInflation.csv")

# linear quantile regression LASSO ----------------------------------------------
LQRFredLasso <- function(dataset, dependentvariable){
  allpredictionsMatrix <- matrix(nrow = 0, ncol = (length(quantiles)+1))
  #30 year of training data
  trainRows <- ((2000-1970) * 4)
  predictions <- 96
  for(i in 1:predictions){
    #separate training from test data
    train_data <- dataset [i:(trainRows+i-1),]
    test_data <- dataset[(trainRows+i), ]
    y_test <- test_data[, dependentvariable]
    formulaText <- as.formula(paste(dependentvariable, "~ ."))
    #NOG UITVOGELEN WAT DIT BETEKENT
    LinQuantReg <- rq(formulaText, data = train_data, tau = quantiles, method = "lasso")
    predictions <- predict(LinQuantReg,newdata = test_data)
    predictionslist<- cbind(y_test,predictions)
    allpredictionsMatrix <- rbind(allpredictionsMatrix,predictionslist )
    print( dataset[(trainRows+i), dependentvariable])
  }
  return(allpredictionsMatrix)
}

# Linear quantile regression PCA -----------------------------
LQRFredPCA <- function(dataset, dependentvariable){
  allpredictionsMatrix <- matrix(nrow = 0, ncol = (length(quantiles)+1))
  #30 year of training data
  trainRows <- ((2000-1970) * 4)
  predictions <- 96
  for(i in 1:predictions){
    #separate training from test data
    train_data <- dataset [i:(trainRows+i-1),]
    test_data <- dataset[(trainRows+i), ]
    y_test <- test_data[, dependentvariable]
    formulaText <- as.formula(paste(dependentvariable, "~ ."))
    #NOG UITVOGELEN WAT DIT BETEKENT
    LinQuantReg <- rq(formulaText, data = train_data, tau = quantiles)
    predictions <- predict(LinQuantReg,newdata = test_data)
    predictionslist<- cbind(y_test,predictions)
    allpredictionsMatrix <- rbind(allpredictionsMatrix,predictionslist )
    print( dataset[(trainRows+i), dependentvariable])
  }
  return(allpredictionsMatrix)
}


# quantile regression forest ----------------------------------------------

qrfFred <- function(X_set, y_set, dependentvariable){
  allpredictionsList <- matrix(nrow = 0, ncol = (length(quantiles)+1))
  #30 years of training data
  trainRows <- ((2000-1970) * 4)
  predictions <- 96
  for(i in 1:predictions){
    #train the QRF model
    X_train <- X_set[i:(trainRows+i-1),]
    y_train <- y_set[i: (trainRows+i-1)]
    qrf_model <- quantregForest(x = X_train, y = y_train, ntree =1000, nodesize= 10)
    X_test <- X_set[(trainRows+i),]
    y_test <- y_set[(trainRows+i)]
    predictions <- predict(qrf_model, newdata = X_test, what = quantiles)
    predictionslist<- cbind(y_test,predictions)
    allpredictionsList <- rbind(allpredictionsList,predictionslist )
    print( y_set[(trainRows+i)])
  }
  return(allpredictionsList)
}

# Horizon 1 ------------------------------------------------
#GDP
df_toUseGDP <- df_PredictGDP
#lag data by one quarter for one step ahead forecasts
df_laggedGDP <- df_toUseGDP
df_laggedGDP[, paste0(names(df_laggedGDP), "_lag", 1)] <- lapply(df_laggedGDP, function(x) lag(x, 1))
df_laggedGDP <- cbind(date = dfqua$date, df_laggedGDP)
#use data from 1970
df_laggedGDP <- df_laggedGDP[17:nrow(df_laggedGDP),]
#for qrf
X_setGDP <- df_laggedGDP[, c(1, 251:ncol(df_laggedGDP))]
y_setGDP <- df_laggedGDP[, c("GDPC1")]
#for linear quantile regression
GDPData <- cbind(GDPC1 =y_setGDP,X_setGDP)
#PCA 
factorsPCAgdp <- data.frame(prcomp( X_setGDP[,-1], rank =10, scale. = TRUE)$x)
pcadatagdp <- cbind(GDPC1 =y_setGDP, factorsPCAgdp)


qrfh1GDP <- qrfFred(X_setGDP, y_setGDP, "GDPC1")
qrfh1GDP <- as.data.frame(qrfh1GDP)
qrfh1GDP$date <- dates[137:(nrow(dfqua)-1) ]
write.csv(qrfh1GDP, file = "qrfh1GDP.csv")

lqrh1GDPpca <- LQRFredPCA(pcadatagdp,"GDPC1")
lqrh1GDPpca <- as.data.frame(lqrh1GDPpca)
lqrh1GDPpca$date <- dates[137:(nrow(dfqua)-1) ]
write.csv(lqrh1GDPpca, file = "lqrh1GDPpca.csv")

lqrh1GDPlasso <- LQRFredLasso(GDPData,"GDPC1")
lqrh1GDPlasso <- as.data.frame(lqrh1GDPlasso)
lqrh1GDPlasso$date <- dates[137:(nrow(dfqua)-1) ]
write.csv(lqrh1GDPlasso, file = "lqrh1GDPlasso.csv")

# headline inlfation
df_toUseHead <- df_PredictCPIAUCSL
#lag data by one quarter for one step ahead forecasts
df_laggedCPIAUCSL <- df_toUseHead
df_laggedCPIAUCSL[, paste0(names(df_laggedCPIAUCSL), "_lag", 1)] <- lapply(df_laggedCPIAUCSL, function(x) lag(x, 1))
df_laggedCPIAUCSL <- cbind(date = dfqua$date, df_laggedCPIAUCSL)
#use data from 1970
df_laggedCPIAUCSL <- df_laggedCPIAUCSL[17:nrow(df_laggedCPIAUCSL),]
X_setCPIAUCSL <- df_laggedCPIAUCSL[, c(1, 251:ncol(df_laggedCPIAUCSL))]
y_setCPIAUCSL <- df_laggedCPIAUCSL[, c("CPIAUCSL")]

#for linear quantile regression
CPIAUCSLData <- cbind(CPIAUCSL =y_setCPIAUCSL,X_setCPIAUCSL)
factorsPCACPIAUCSL <- data.frame(prcomp( X_setCPIAUCSL[,-1], rank =10, scale. = TRUE)$x)
CPIAUCSLPCA <- cbind(CPIAUCSL =y_setCPIAUCSL, factorsPCACPIAUCSL)

qrfh1head <- qrfFred(X_setCPIAUCSL, y_setCPIAUCSL, "CPIAUCSL")
qrfh1head <- as.data.frame(qrfh1head)
qrfh1head$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh1head, file = "qrfh1head.csv")

lqrh1headpca <- LQRFredPCA(CPIAUCSLPCA,"CPIAUCSL")
lqrh1headpca <- as.data.frame(lqrh1headpca)
lqrh1headpca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh1headpca, file = "lqrh1headpca.csv")

lqrh1headlasso <- LQRFredLasso(CPIAUCSLData,"CPIAUCSL")
lqrh1headlasso <- as.data.frame(lqrh1headlasso)
lqrh1headlasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh1headlasso, file = "lqrh1headlasso.csv")


#core inflation 
df_toUsecore<- df_PredictCPILFESL
#lag data by one quarter for one step ahead forecasts
df_laggedCPILFESL <- df_toUsecore
df_laggedCPILFESL[, paste0(names(df_laggedCPILFESL), "_lag", 1)] <- lapply(df_laggedCPILFESL, function(x) lag(x, 1))
df_laggedCPILFESL <- cbind(date = dfqua$date, df_laggedCPILFESL)
#use data from 1970
df_laggedCPILFESL <- df_laggedCPILFESL[17:nrow(df_laggedCPILFESL),]
X_setCPILFESL <- df_laggedCPILFESL[, c(1, 251:ncol(df_laggedCPILFESL))]
y_setCPILFESL <- df_laggedCPILFESL[, c("CPILFESL")]

#for linear quantile regression
CPILFESLData <- cbind(CPILFESL =y_setCPILFESL,X_setCPILFESL)
factorsPCACPILFESl <- data.frame(prcomp( X_setCPILFESL[,-1], rank =10, scale. = TRUE)$x)
CPILFESlPCA <- cbind(CPILFESL =y_setCPILFESL, factorsPCACPILFESl)

qrfh1core <- qrfFred(X_setCPILFESL, y_setCPILFESL, "CPILFESL")
qrfh1core <- as.data.frame(qrfh1core)
qrfh1core$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh1core, file = "qrfh1core.csv")

lqrh1corepca <- LQRFredPCA(CPILFESlPCA,"CPILFESL")
lqrh1corepca <- as.data.frame(lqrh1corepca)
lqrh1corepca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh1corepca, file = "lqrh1corepca.csv")

lqrh1corelasso <- LQRFredLasso(CPILFESLData,"CPILFESL")
lqrh1corelasso <- as.data.frame(lqrh1corelasso)
lqrh1corelasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh1corelasso, file = "lqrh1corelasso.csv")

# Horizon 2 ---------------------------------------------------------------
#GDP
df_toUseGDP <- df_PredictGDP
#lag data by one quarter for one step ahead forecasts
df_laggedGDP <- df_toUseGDP
df_laggedGDP[, paste0(names(df_laggedGDP), "_lag", 2)] <- lapply(df_laggedGDP, function(x) lag(x, 2))
df_laggedGDP <- cbind(date = dfqua$date, df_laggedGDP)
#use data from 1970
df_laggedGDP <- df_laggedGDP[17:nrow(df_laggedGDP),]
#for qrf
X_setGDP <- df_laggedGDP[, c(1, 251:ncol(df_laggedGDP))]
y_setGDP <- df_laggedGDP[, c("GDPC1")]
#for linear quantile regression
GDPData <- cbind(GDPC1 =y_setGDP,X_setGDP)
factorsPCAgdp <- data.frame(prcomp( X_setGDP[,-1], rank =10, scale. = TRUE)$x)
pcadatagdp <- cbind(GDPC1 =y_setGDP, factorsPCAgdp)


qrfh2GDP <- qrfFred(X_setGDP, y_setGDP, "GDPC1")
qrfh2GDP <- as.data.frame(qrfh2GDP)
qrfh2GDP$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh2GDP, file = "qrfh2GDP.csv")

lqrh2GDPpca <- LQRFredPCA(pcadatagdp,"GDPC1")
lqrh2GDPpca <- as.data.frame(lqrh2GDPpca)
lqrh2GDPpca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh2GDPpca, file = "lqrh2GDPpca.csv")

lqrh2GDPlasso <- LQRFredLasso(GDPData,"GDPC1")
lqrh2GDPlasso <- as.data.frame(lqrh2GDPlasso)
lqrh2GDPlasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh2GDPlasso, file = "lqrh2GDPlasso.csv")

# headline inlfation
df_toUseHead <- df_PredictCPIAUCSL
#lag data by one quarter for one step ahead forecasts
df_laggedCPIAUCSL <- df_toUseHead
df_laggedCPIAUCSL[, paste0(names(df_laggedCPIAUCSL), "_lag", 2)] <- lapply(df_laggedCPIAUCSL, function(x) lag(x, 2))
df_laggedCPIAUCSL <- cbind(date = dfqua$date, df_laggedCPIAUCSL)
#use data from 1970
df_laggedCPIAUCSL <- df_laggedCPIAUCSL[17:nrow(df_laggedCPIAUCSL),]
X_setCPIAUCSL <- df_laggedCPIAUCSL[, c(1, 251:ncol(df_laggedCPIAUCSL))]
y_setCPIAUCSL <- df_laggedCPIAUCSL[, c("CPIAUCSL")]

#for linear quantile regression
CPIAUCSLData <- cbind(CPIAUCSL =y_setCPIAUCSL,X_setCPIAUCSL)
factorsPCACPIAUCSL <- data.frame(prcomp( X_setCPIAUCSL[,-1], rank =10, scale. = TRUE)$x)
CPIAUCSLPCA <- cbind(CPIAUCSL =y_setCPIAUCSL, factorsPCACPIAUCSL)

qrfh2head <- qrfFred(X_setCPIAUCSL, y_setCPIAUCSL, "CPIAUCSL")
qrfh2head <- as.data.frame(qrfh2head)
qrfh2head$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh2head, file = "qrfh2head.csv")

lqrh2headpca <- LQRFredPCA(CPIAUCSLPCA,"CPIAUCSL")
lqrh2headpca <- as.data.frame(lqrh2headpca)
lqrh2headpca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh2headpca, file = "lqrh2headpca.csv")

lqrh2headlasso <- LQRFredLasso(CPIAUCSLData,"CPIAUCSL")
lqrh2headlasso <- as.data.frame(lqrh2headlasso)
lqrh2headlasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh2headlasso, file = "lqrh2headlasso.csv")


#core inflation 
df_toUsecore<- df_PredictCPILFESL
#lag data by one quarter for one step ahead forecasts
df_laggedCPILFESL <- df_toUsecore
df_laggedCPILFESL[, paste0(names(df_laggedCPILFESL), "_lag", 2)] <- lapply(df_laggedCPILFESL, function(x) lag(x, 2))
df_laggedCPILFESL <- cbind(date = dfqua$date, df_laggedCPILFESL)
#use data from 1970
df_laggedCPILFESL <- df_laggedCPILFESL[17:nrow(df_laggedCPILFESL),]
X_setCPILFESL <- df_laggedCPILFESL[, c(1, 251:ncol(df_laggedCPILFESL))]
y_setCPILFESL <- df_laggedCPILFESL[, c("CPILFESL")]

#for linear quantile regression
CPILFESLData <- cbind(CPILFESL =y_setCPILFESL,X_setCPILFESL)
factorsPCACPILFESl <- data.frame(prcomp( X_setCPILFESL[,-1], rank =10, scale. = TRUE)$x)
CPILFESlPCA <- cbind(CPILFESL =y_setCPILFESL, factorsPCACPILFESl)

qrfh2core <- qrfFred(X_setCPILFESL, y_setCPILFESL, "CPILFESL")
qrfh2core <- as.data.frame(qrfh2core)
qrfh2core$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh2core, file = "qrfh2core.csv")

lqrh2corepca <- LQRFredPCA(CPILFESlPCA,"CPILFESL")
lqrh2corepca <- as.data.frame(lqrh2corepca)
lqrh2corepca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh2corepca, file = "lqrh2corepca.csv")

lqrh2corelasso <- LQRFredLasso(CPILFESLData,"CPILFESL")
lqrh2corelasso <- as.data.frame(lqrh2corelasso)
lqrh2corelasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh2corelasso, file = "lqrh2corelasso.csv")

# Horizon 4 ---------------------------------------------------------------

#GDP
df_toUseGDP <- df_PredictGDP
#lag data by one quarter for one step ahead forecasts
df_laggedGDP <- df_toUseGDP
df_laggedGDP[, paste0(names(df_laggedGDP), "_lag", 4)] <- lapply(df_laggedGDP, function(x) lag(x, 4))
df_laggedGDP <- cbind(date = dfqua$date, df_laggedGDP)
#use data from 1970
df_laggedGDP <- df_laggedGDP[17:nrow(df_laggedGDP),]
#for qrf
X_setGDP <- df_laggedGDP[, c(1, 251:ncol(df_laggedGDP))]
y_setGDP <- df_laggedGDP[, c("GDPC1")]
#for linear quantile regression
GDPData <- cbind(GDPC1 =y_setGDP,X_setGDP)
factorsPCAgdp <- data.frame(prcomp( X_setGDP[,-1], rank =10, scale. = TRUE)$x)
pcadatagdp <- cbind(GDPC1 =y_setGDP, factorsPCAgdp)


qrfh4GDP <- qrfFred(X_setGDP, y_setGDP, "GDPC1")
qrfh4GDP <- as.data.frame(qrfh4GDP)
qrfh4GDP$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh4GDP, file = "qrfh4GDP.csv")

lqrh4GDPpca <- LQRFredPCA(pcadatagdp,"GDPC1")
lqrh4GDPpca <- as.data.frame(lqrh4GDPpca)
lqrh4GDPpca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh4GDPpca, file = "lqrh4GDPpca.csv")

lqrh4GDPlasso <- LQRFredLasso(GDPData,"GDPC1")
lqrh4GDPlasso <- as.data.frame(lqrh4GDPlasso)
lqrh4GDPlasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh4GDPlasso, file = "lqrh4GDPlasso.csv")

# headline inlfation
df_toUseHead <- df_PredictCPIAUCSL
#lag data by one quarter for one step ahead forecasts
df_laggedCPIAUCSL <- df_toUseHead
df_laggedCPIAUCSL[, paste0(names(df_laggedCPIAUCSL), "_lag", 4)] <- lapply(df_laggedCPIAUCSL, function(x) lag(x, 4))
df_laggedCPIAUCSL <- cbind(date = dfqua$date, df_laggedCPIAUCSL)
#use data from 1970
df_laggedCPIAUCSL <- df_laggedCPIAUCSL[17:nrow(df_laggedCPIAUCSL),]
X_setCPIAUCSL <- df_laggedCPIAUCSL[, c(1, 251:ncol(df_laggedCPIAUCSL))]
y_setCPIAUCSL <- df_laggedCPIAUCSL[, c("CPIAUCSL")]

#for linear quantile regression
CPIAUCSLData <- cbind(CPIAUCSL =y_setCPIAUCSL,X_setCPIAUCSL)
factorsPCACPIAUCSL <- data.frame(prcomp( X_setCPIAUCSL[,-1], rank =10, scale. = TRUE)$x)
CPIAUCSLPCA <- cbind(CPIAUCSL =y_setCPIAUCSL, factorsPCACPIAUCSL)

qrfh4head <- qrfFred(X_setCPIAUCSL, y_setCPIAUCSL, "CPIAUCSL")
qrfh4head <- as.data.frame(qrfh4head)
qrfh4head$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh4head, file = "qrfh4head.csv")

lqrh4headpca <- LQRFredPCA(CPIAUCSLPCA,"CPIAUCSL")
lqrh4headpca <- as.data.frame(lqrh4headpca)
lqrh4headpca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh4headpca, file = "lqrh4headpca.csv")

lqrh4headlasso <- LQRFredLasso(CPIAUCSLData,"CPIAUCSL")
lqrh4headlasso <- as.data.frame(lqrh4headlasso)
lqrh4headlasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh4headlasso, file = "lqrh4headlasso.csv")


#core inflation 
df_toUsecore<- df_PredictCPILFESL
#lag data by one quarter for one step ahead forecasts
df_laggedCPILFESL <- df_toUsecore
df_laggedCPILFESL[, paste0(names(df_laggedCPILFESL), "_lag", 4)] <- lapply(df_laggedCPILFESL, function(x) lag(x, 4))
df_laggedCPILFESL <- cbind(date = dfqua$date, df_laggedCPILFESL)
#use data from 1970
df_laggedCPILFESL <- df_laggedCPILFESL[17:nrow(df_laggedCPILFESL),]
X_setCPILFESL <- df_laggedCPILFESL[, c(1, 251:ncol(df_laggedCPILFESL))]
y_setCPILFESL <- df_laggedCPILFESL[, c("CPILFESL")]

#for linear quantile regression
CPILFESLData <- cbind(CPILFESL =y_setCPILFESL,X_setCPILFESL)
factorsPCACPILFESl <- data.frame(prcomp( X_setCPILFESL[,-1], rank =10, scale. = TRUE)$x)
CPILFESlPCA <- cbind(CPILFESL =y_setCPILFESL, factorsPCACPILFESl)

qrfh4core <- qrfFred(X_setCPILFESL, y_setCPILFESL, "CPILFESL")
qrfh4core <- as.data.frame(qrfh4core)
qrfh4core$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(qrfh4core, file = "qrfh4core.csv")

lqrh4corepca <- LQRFredPCA(CPILFESlPCA,"CPILFESL")
lqrh4corepca <- as.data.frame(lqrh4corepca)
lqrh4corepca$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh4corepca, file = "lqrh4corepca.csv")

lqrh4corelasso <- LQRFredLasso(CPILFESLData,"CPILFESL")
lqrh4corelasso <- as.data.frame(lqrh4corelasso)
lqrh4corelasso$date <- dates[137:(nrow(dfqua)-1) ]
#write.csv(lqrh4corelasso, file = "lqrh4corelasso.csv")



# evaluate prediction intervals ------------------------------------------------------
columnnames <- c("actual", "low","halflow","median", "halfhigh","high","date" )
df <- qrfH1G
colnames(df) <- columnnames
# Checking if y_test falls within the 95% prediction interval
belowInterval95 <- df$actual < df$low
aboveInterval95 <- df$actual > df$high
belowInterval50 <- df$actual < df$halflow
aboveInterval50 <- df$actual > df$halfhigh

# Calculate the percentage
percentage_below_interval95 <- mean(belowInterval95) * 100
percentage_above_interval95 <- mean(aboveInterval95) * 100
percentage_below_interval50 <- mean(belowInterval50) * 100
percentage_above_interval50 <- mean(aboveInterval50) * 100
cat("Percentage of actual values outside the 50% prediction interval:", percentage_below_interval50+percentage_above_interval50, "%\n")
cat("Percentage of actual values outside the 95% prediction interval:", percentage_below_interval95+percentage_above_interval95, "%\n")

#calculate average interval length
df$intervalLengthLarge <- df$high-df$low
df$intervalLengthSmall <- df$halfhigh-df$halflow
# Print the result
cat("Length of the 95% prediction interval:", mean(df$intervalLengthLarge))
cat("Length of the 50% prediction interval:", mean(df$intervalLengthSmall))

# Compare median point predictions ----------------------------------------
# SETAR point predicitions -------------------------------------------------------------------
SETAR4Fred <-function(horizon,y_variable){
  df<- cbind(date = dates, df_imputed)
  if (horizon !=1) {
    df <- df[(17-horizon+1):nrow(df_imputed),]
  } else {
    df <- df[17:nrow(df_imputed),]
  }
  # Initialize parameters
  n_train <- 120
  n_test <- 96
  total_obs <- nrow(df)
  
  # Extract the target variable
  y <- df[,y_variable]
  
  # Initialize a list to store predictions
  predictions <- list()
  
  
  # Starting date for predictions
  start_date <- ymd("2000-03-01")
  
  for (i in 1:n_test) {
    
    train_start <- i
    train_end <- i + n_train - 1
    # Train the AR(4) model
    train_data <- y[train_start:train_end]
    
    setarModel <- setar(train_data, m= 4)
    prediction <- predict(setarModel, n.ahead = horizon)
    forecast <- prediction[horizon]
    predictions[i] <- forecast
  }
  pred_df <- data.frame(
    date = seq.Date(start_date, by = "quarter", length.out = n_test),
    actual = y[(n_train + horizon):(n_train + n_test+horizon-1)]
  )
  pred_df$predict <- c(predictions)
  return(pred_df)
}
SetarH1GDP <- SETAR4Fred(horizon = 1,"GDPC1")
SetarH2GDP <- SETAR4Fred(horizon =2,"GDPC1")
SetarH4GDP<- SETAR4Fred(horizon = 4,"GDPC1")

SetarH1head <- SETAR4Fred(horizon = 1,"CPIAUCSL")
SetarH2head <- SETAR4Fred(horizon =2,"CPIAUCSL")
SetarH4head<- SETAR4Fred(horizon = 4,"CPIAUCSL")

SetarH1core <- SETAR4Fred(horizon = 1,"CPILFESL")
SetarH2core <- SETAR4Fred(horizon =2,"CPILFESL")
SetarH4core<- SETAR4Fred(horizon = 4,"CPILFESL")

# median point prediction from intervals ---------------------------------------------

#rmspe function
RMSPE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

#SETAR
RMSPE(SetarH1GDP$actual, as.numeric(SetarH1GDP$predict))
RMSPE(SetarH2GDP$actual, as.numeric(SetarH2GDP$predict))
RMSPE(SetarH4GDP$actual, as.numeric(SetarH4GDP$predict))

RMSPE(SetarH1head$actual, as.numeric(SetarH1head$predict))
RMSPE(SetarH2head$actual, as.numeric(SetarH2head$predict))
RMSPE(SetarH4head$actual, as.numeric(SetarH4head$predict))

RMSPE(SetarH1head$actual, as.numeric(SetarH1core$predict))
RMSPE(SetarH2core$actual, as.numeric(SetarH2core$predict))
RMSPE(SetarH4core$actual, as.numeric(SetarH4core$predict))

#AR4
RMSPE(AR4H1GDP$actual, AR4H1GDP$median)
RMSPE(AR4H2GDP$actual, AR4H2GDP$median)
RMSPE(AR4H4GDP$actual, AR4H4GDP$median)
RMSPE(AR4H1headlineInflation$actual, AR4H1headlineInflation$median)
RMSPE(AR4H2headlineInflation$actual, AR4H2headlineInflation$median)
RMSPE(AR4H4headlineInflation$actual, AR4H4headlineInflation$median)
RMSPE(AR4H1coreInflation$actual, AR4H1coreInflation$median)
RMSPE(AR4H2coreInflation$actual, AR4H2coreInflation$median)
RMSPE(AR4H4coreInflation$actual, AR4H4coreInflation$median)

#QRF
RMSPE(qrfH1GDP$y_test, qrfH1GDP$`quantile= 0.5`)
RMSPE(qrfh2GDP$y_test, qrfh2GDP$`quantile= 0.5`)
RMSPE(qrfh4GDP$y_test, qrfh4GDP$`quantile= 0.5`)
RMSPE(qrfH1head$y_test, qrfH1head$`quantile= 0.5`)
RMSPE(qrfH2head$y_test, qrfH2head$`quantile= 0.5`)
RMSPE(qrfH4head$y_test, qrfH4head$`quantile= 0.5`)
RMSPE(qrfH1core$y_test, qrfH1core$`quantile= 0.5`)
RMSPE(qrfH2core$y_test, qrfH2core$`quantile= 0.5`)
RMSPE(qrfH4core$y_test, qrfH4core$`quantile= 0.5`)

#LQRpca
RMSPE(lqrH1GDPpca$y_test, lqrH1GDPpca$`tau= 0.500`)
RMSPE(lqrh2GDPpca$y_test, lqrh2GDPpca$`tau= 0.500`)
RMSPE(lqrh4GDPpca$y_test, lqrh4GDPpca$`tau= 0.500`)
RMSPE(lqrH1headpca$y_test, lqrH1headpca$`tau= 0.500`)
RMSPE(lqrh2headpca$y_test, lqrh2headpca$`tau= 0.500`)
RMSPE(lqrh4headpca$y_test, lqrh4headpca$`tau= 0.500`)
RMSPE(lqrH1corepca$y_test, lqrH1corepca$`tau= 0.500`)
RMSPE(lqrh2corepca$y_test, lqrh2corepca$`tau= 0.500`)
RMSPE(lqrh4corepca$y_test, lqrh4corepca$`tau= 0.500`)

#LQRlasso
RMSPE(lqrH1GDPlasso$y_test, lqrH1GDPlasso$`tau= 0.500`)
RMSPE(lqrh2GDPlasso$y_test, lqrh2GDPlasso$`tau= 0.500`)
RMSPE(lqrh4GDPlasso$y_test, lqrh4GDPlasso$`tau= 0.500`)

RMSPE(lqrH1headlasso$y_test, lqrH1headlasso$`tau= 0.500`)
RMSPE(lqrh2headlasso$y_test, lqrh2headlasso$`tau= 0.500`)
RMSPE(lqrh4headlasso$y_test, lqrh4headlasso$`tau= 0.500`)

RMSPE(lqrH1corelasso$y_test, lqrH1corelasso$`tau= 0.500`)
RMSPE(lqrh2corelasso$y_test, lqrh2corelasso$`tau= 0.500`)
RMSPE(lqrh4corelasso$y_test, lqrh4corelasso$`tau= 0.500`)





# plot data ---------------------------------------------------------------

qrf <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/qrf/qrfH1gdp.csv")
qrf$date <- dfqua[137:(nrow(dfqua)-1),"date"]

df<- qrf
subset_dfGR <- subset(df[,-1], date >= as.Date('2007-03-01') & date <= as.Date('2009-12-01'))
subset_Covid <- subset(df[,-1], date >= as.Date('2020-03-01') & date <= as.Date('2021-12-01'))


ggplot(subset_dfGR, aes(x= date, y= y_test))+
  geom_errorbar(aes(ymin = quantile..0.025, ymax = quantile..0.975), color= "gray55")+
  geom_ribbon(aes(ymin = quantile..0.025, ymax = quantile..0.975), fill = "gray55", alpha = 0.8)+
  geom_point(color = "firebrick2")+ labs(y = "gdp", x="date" )+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))
 
ggplot(ar4h4, aes(x= date, y= actual))+
  geom_errorbar(aes(ymin = low, ymax = high), color= "gray55")+
  geom_ribbon(aes(ymin = low, ymax = high), fill = "gray55", alpha = 0.8)+
  geom_point(color = "firebrick2")+ labs(y = "Core Inflation", x="date" )+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))


