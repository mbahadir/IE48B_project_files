library(forecast)
library(caret)
library(glmnet)
library(data.table)
library(fpp2)
library(skimr)
library(ggcorrplot)
library(zoo)
library(lubridate)
library(urca)
library(rpart)
library(rattle)
require(treeClust)

features = fread(sprintf("%s2022-01-22_weather.csv", path))

wide_feat = dcast(features, date + hour ~ variable+ lat + lon , value.var="value")
#tail(wide_feat)
#colnames(wide_feat)

train_feat <- wide_feat[date <= "2021-11-30",]
test_feat <- wide_feat[date >= "2021-12-01" & date <= "2021-12-14",]

#train <- merge(train, train_feat, by.x = c("date","hour"),by.y = c("date","hour"))
#test <- merge(test, test_feat, by.x = c("date","hour"), by.y = c("date","hour"))

mod_cols <- colnames(wide_feat)[c(-1,-2)]

wide_feat[ , (mod_cols) := lapply(.SD,na.aggregate, by=.(hour,wday(date))), .SDcols = mod_cols]
wide_feat[ , hour := as.factor(hour)]
#wide_feat[,net := na.aggregate(net,by=.(hour,wday(date)))]
head(wide_feat)

corr <- cor(wide_feat[,-c('date', 'hour')])
ggcorrplot(corr)

pca_rep_dswrf = princomp(train_feat[,c(3:9)])
summary(pca_rep_dswrf) # one column covers 95%. One component added to the dataset

train_feat[,DSWRF:=pca_rep_dswrf$scores[,1]]
test_feat[, DSWRF := predict(pca_rep_dswrf,test_feat)[,1]]

pca_rep_rh = princomp(train_feat[,c(10:16)])
summary(pca_rep_rh) #three components cover 87% of the variance. Three components added to the data set.

train_feat[,RH1 := pca_rep_rh$scores[,1]]
test_feat[, RH1 := predict(pca_rep_rh,test_feat)[,1]]

train_feat[,RH2 := pca_rep_rh$scores[,2]]
test_feat[, RH2 := predict(pca_rep_rh,test_feat)[,2]]

train_feat[,RH3 := pca_rep_rh$scores[,3]]
test_feat[, RH3 := predict(pca_rep_rh,test_feat)[,3]]

pca_rep_tcdc = princomp(train_feat[,c(17:23)])
summary(pca_rep_tcdc) #three components cover 74% of the variance. Three components added to the data set.

train_feat[,tcdc1 := pca_rep_tcdc$scores[,1]]
test_feat[, tcdc1 := predict(pca_rep_tcdc,test_feat)[,1]]

train_feat[,tcdc2 := pca_rep_tcdc$scores[,2]]
test_feat[, tcdc2 := predict(pca_rep_tcdc,test_feat)[,2]]

train_feat[,tcdc3 := pca_rep_tcdc$scores[,3]]
test_feat[, tcdc3 := predict(pca_rep_tcdc,test_feat)[,3]]

pca_rep_tmp = princomp(train_feat[,c(24:30)])
summary(pca_rep_tmp) #one component cover 95% of the variance. One component added to the data set.

train_feat[,tmp := pca_rep_tmp$scores[,1]]
test_feat[, tmp := predict(pca_rep_tmp,test_feat)[,1]]

pca_rep_wdir = princomp(train_feat[,c(31:37)])
summary(pca_rep_wdir) # results are not great. no components are added.

#train[,wdir := pca_rep_wdir$scores[,1]]
#test_feat[, wdir := predict(pca_rep_wdir,test_feat)[,1]]

pca_rep_wda = princomp(train_feat[,c(38:44)])
summary(pca_rep_wda) #three components cover 74% of the variance. Three components added to the data set.

train_feat[,wda1 := pca_rep_wda$scores[,1]]
test_feat[, wda1 := predict(pca_rep_wda,test_feat)[,1]]

train_feat[,wda2 := pca_rep_wda$scores[,2]]
test_feat[, wda2 := predict(pca_rep_wda,test_feat)[,2]]

train_feat[,wda3 := pca_rep_wda$scores[,3]]
test_feat[, wda3 := predict(pca_rep_wda,test_feat)[,3]]
