#load csv files from SFP
core <- read.csv("C:/Users/josie/Downloads/spfcore.csv")
gdp <- read.csv("C:/Users/josie/Downloads/spfhead.csv")


# Headline inflation ------------------------------------------------------
headline <- read.csv("C:/Users/josie/Downloads/sfpheadline.csv")
headline$X <- dfqua[137:(nrow(dfqua)-1), "date"]
h1_outsideInterval <- headline$actual < headline$low_h1 |headline$actual >headline$high_h1
h2_outsideInterval <- headline$actual < headline$low_h2 |headline$actual >headline$high_h2
h4_outsideInterval <- headline$actual < headline$low_h4 |headline$actual >headline$high_h4

mean(h1_outsideInterval)*100
mean(h2_outsideInterval)*100
mean(h4_outsideInterval)*100

h1_distance <- abs(headline$actual - headline$median_h1)
h2_distance <- abs(headline$actual - headline$median_h2)
h4_distance <- abs(headline$actual - headline$median_h4)
mean(h1_distance)
mean(h2_distance)
mean(h4_distance)

ggplot(headline, aes(x= date, y= actual))+
  geom_errorbar(aes(ymin = low_h1, ymax = high_h1), color= "gray55")+
  geom_ribbon(aes(ymin = low_h1, ymax = high_h1), fill = "gray55", alpha = 0.8)+
  geom_point(color = "firebrick2")+ labs(y = "gdp", x="date" )+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))


# Core inflation ----------------------------------------
core <- read.csv("C:/Users/josie/Downloads/spfcore.csv")
core$date <- dfqua[165:(nrow(dfqua)-1), "date"]
core <- core[5:nrow(core),]
core$low_h1 <- as.numeric(core$low_h1)
core$high_h1 <- as.numeric(core$high_h1)
core$median_h1 <- as.numeric(core$median_h1)
core$low_h2 <- as.numeric(core$low_h2)
core$high_h2 <- as.numeric(core$high_h2)
core$median_h2 <- as.numeric(core$median_h2)
core$low_h4 <- as.numeric(core$low_h4)
core$high_h4 <- as.numeric(core$high_h4)
core$median_h4 <- as.numeric(core$median_h4)


h1_outsideInterval <- core$actual < core$low_h1 |core$actual >core$high_h1
h2_outsideInterval <- core$actual < core$low_h2 |core$actual >core$high_h2
h4_outsideInterval <- core$actual < core$low_h4 |core$actual >core$high_h4
mean(h1_outsideInterval)*100
mean(h2_outsideInterval)*100
mean(h4_outsideInterval)*100

h1_distance <- abs(core$actual - core$median_h1)
h2_distance <- abs(core$actual - core$median_h2)
h4_distance <- abs(core$actual - core$median_h4)
mean(h1_distance)
mean(h2_distance)
mean(h4_distance)

ggplot(core, aes(x= date, y= actual))+
  geom_errorbar(aes(ymin = low_h1, ymax = high_h1), color= "gray55")+
  geom_ribbon(aes(ymin = low_h1, ymax = high_h1), fill = "gray55", alpha = 0.8)+
  geom_point(color = "firebrick2")+ labs(y = "gdp", x="date" )+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))


# GDP ---------------------------------------------------------------------
gdp <- read.csv("C:/Users/josie/Downloads/spfgdp.csv")
gdp$date <- dfqua[137:(nrow(dfqua)-1), "date"]
gdp$logActual <- log(gdp$actual)*100

h1_outsideInterval <- gdp$logActual < gdp$low_h1 |gdp$logActual >gdp$high_h1
h2_outsideInterval <- gdp$logActual < gdp$low_h2 |gdp$logActual >gdp$high_h2
h4_outsideInterval <- gdp$logActual < gdp$low_h4 |gdp$logActual >gdp$high_h4

mean(h1_outsideInterval)*100
mean(h2_outsideInterval)*100
mean(h4_outsideInterval)*100

h1_distance <- abs(gdp$logActual - core$median_h1)
h2_distance <- abs(gdp$logActual - core$median_h2)
h4_distance <- abs(gdp$logActual - core$median_h4)
mean(h1_distance)
mean(h2_distance)
mean(h4_distance)


ggplot(gdp, aes(x= date, y= logActual))+
  geom_errorbar(aes(ymin = low_h1, ymax = high_h1), color= "gray55")+
  geom_ribbon(aes(ymin = low_h1, ymax = high_h1), fill = "gray55", alpha = 0.8)+
  geom_point(color = "firebrick2")+ labs(y = "gdp", x="date" )+
  theme_classic()+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.8))


                
