library(data.table)
library(fpp2)
library(lubridate)

data=fread("bulk_imbalance.csv")

data[, sys_numeric:=ifelse(system_direction=="Negative", -1, ifelse(system_direction=="Neutral", 0, 1))]

data

plot(data$net,type="l", ylab="Net Value", main="Net Values of Given Dataset")

acf(data$net)

pacf(data$net)

library("stats")
library("tsoutliers")

decomp=decompose(ts(data$net,freq=24),type='add')
plot(decomp)

outliers_decompose=tsoutliers(decomp$random)
outliers_data=tsoutliers(data$net)

outliers_decompose

outliers_data

net_val=data$net

str(net_val)

autoplot(ts(tsclean(net_val)), series="clean", color='red', lwd=0.7) +
  autolayer(ts(net_val), series="original", color='gray', lwd=1) +
  geom_point(data = tsoutliers(net_val) %>% as.data.frame(), 
             aes(x=index, y=replacements), col='blue') +
  labs(x = "Date Index", y = "Net Values")

data[outliers_data$index]


