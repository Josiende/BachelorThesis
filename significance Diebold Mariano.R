library(forecast)
library(data.table)

qrf <-read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/QRF/qrfH2gdp.csv")
ar4 <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/ar4/ar4h2gdp.csv")

#all observations 
original <- qrf$y_test
forecast_errorAR4 <- ar4$median - original
forecast_errorqrf <- qrf$quantile..0.5 - original
allobs <-dm.test(forecast_errorAR4, forecast_errorqrf, alternative = "two.sided")
print(allobs)

#great recession 
subset_qrfGR <- subset(qrf[,-1], date >= as.Date('2007-03-01') & date <= as.Date('2009-12-01'))
subset_ar4GR <- subset(ar4[,-1], date >= as.Date('2007-03-01') & date <= as.Date('2009-12-01'))
original <- subset_qrfGR$y_test
forecast_errorAR4 <- subset_ar4GR$median - original
forecast_errorqrf <- subset_qrfGR$quantile..0.5 - original
GR <-dm.test(forecast_errorAR4, forecast_errorqrf, alternative = "two.sided")
print(GR)


#covid 
subset_ar4Covid <- subset(ar4[,-1], date >= as.Date('2020-03-01') & date <= as.Date('2021-12-01'))
subset_qrfCovid <- subset(qrf[,-1], date >= as.Date('2020-03-01') & date <= as.Date('2021-12-01'))
original <- subset_qrfCovid$y_test
forecast_errorAR4 <- subset_ar4Covid$median - original
forecast_errorqrf <- subset_qrfCovid$quantile..0.5 - original
covid <-dm.test(forecast_errorAR4, forecast_errorqrf, alternative = "two.sided")
print(covid)


