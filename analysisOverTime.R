
# qrf ---------------------------------------------------------------------
qrfH1 <-read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/QRF/qrfH1core.csv")
qrfH2 <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/QRF/qrfH2core.csv")
qrfH4 <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/QRF/qrfH4core.csv")

colname <- c("actual", "low","halflow","halfhigh","high","median", "date" )

#rmspe function
RMSPE <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}



df<- qrfH4
subset_dfGR <- subset(df[,-1], date >= as.Date('2007-03-01') & date <= as.Date('2009-12-01'))
subset_Covid <- subset(df[,-1], date >= as.Date('2020-03-01') & date <= as.Date('2021-12-01'))

#RMSPE
RMSPE(subset_dfGR$y_test, subset_dfGR$`quantile..0.5`)
RMSPE(subset_Covid$y_test, subset_Covid$`quantile..0.5`)


#interval Length
subset_dfGR$intervalLengthLarge <- subset_dfGR$quantile..0.975 - subset_dfGR$quantile..0.025
subset_dfGR$intervalLengthSmall <- subset_dfGR$quantile..0.75 - subset_dfGR$quantile..0.25
subset_Covid$intervalLengthLarge <- subset_Covid$quantile..0.975 - subset_Covid$quantile..0.025
subset_Covid$intervalLengthSmall <- subset_Covid$quantile..0.75 - subset_Covid$quantile..0.25

cat("Length of the 95% prediction interval:", mean(subset_dfGR$intervalLengthLarge))
cat("Length of the 95% prediction interval:", mean(subset_Covid$intervalLengthLarge))
cat("Length of the 50% prediction interval:", mean(subset_dfGR$intervalLengthSmall))
cat("Length of the 50% prediction interval:", mean(subset_Covid$intervalLengthSmall))


# Checking if y_test falls within the 95% prediction interval
colnames(subset_dfGR)<- colname
colnames(subset_Covid)<- colname
interval95GR <- subset_dfGR$actual < subset_dfGR$low | subset_dfGR$actual > subset_dfGR$high
interval95C <- subset_Covid$actual < subset_Covid$low | subset_Covid$actual > subset_Covid$high
Interval50GR <- subset_dfGR$actual < subset_dfGR$halflow | subset_dfGR$actual > subset_dfGR$halfhigh
Interval50c <- subset_Covid$actual < subset_Covid$halflow |subset_Covid$actual > subset_Covid$halfhigh


# Calculate the percentage 
mean(interval95GR) * 100
mean(interval95C) * 100
mean(Interval50GR) * 100
mean(Interval50c) * 100




# AR4 ---------------------------------------------------------------------
ar4h1 <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/AR4/AR4h1coreinflation.csv")
ar4h2 <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/AR4/AR4h2coreinflation.csv")
ar4h4 <- read.csv("C:/Users/josie/OneDrive - Erasmus University Rotterdam/Thesis code/CSV files extension/AR4/AR4h4coreinflation.csv")




df<- ar4h4
subset_dfGR <- subset(df[,-1], date >= as.Date('2007-03-01') & date <= as.Date('2009-12-01'))
subset_Covid <- subset(df[,-1], date >= as.Date('2020-03-01') & date <= as.Date('2021-12-01'))

#RMSPE
RMSPE(subset_dfGR$actual, subset_dfGR$median)
RMSPE(subset_Covid$actual, subset_Covid$median)

#interval Length

subset_dfGR$intervalLengthLarge <- subset_dfGR$high - subset_dfGR$low
subset_Covid$intervalLengthLarge <- subset_Covid$high - subset_Covid$low



# Checking if y_test falls within the 95% prediction interval

interval95GR <- subset_dfGR$actual < subset_dfGR$low | subset_dfGR$actual > subset_dfGR$high
interval95C <- subset_Covid$actual < subset_Covid$low | subset_Covid$actual > subset_Covid$high


# Calculate the percentage 
mean(interval95GR) * 100
mean(interval95C) * 100


