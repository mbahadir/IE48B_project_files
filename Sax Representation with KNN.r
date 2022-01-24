require(caret)
require(ggplot2)
require(data.table)
require(caTools)
library(parallel)
library(doParallel)
library(randomForest)
library(lubridate)

options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)

path="C:/Users/bahad/R Notebooks/"
bulk_imbalance = fread(sprintf("%sbulk_imbalance.csv", path))
data = bulk_imbalance
data = data[order(date,hour)]
data[,ID := 1:.N]
head(data)

current_date="2022-01-22"

features=fread(sprintf("%s%s_weather/%s_weather.csv",path, current_date, current_date))

wide_feat = dcast(features, date + hour ~ variable+ lat + lon , value.var="value")

tail(wide_feat)

data[, lag_24:=shift(net, 24)]
data[, lag_168:=shift(net, 168)]
# data[, lag_6:=shift(net, 6)]
# data[, lag_7:=shift(net, 7)]
# data[, lag_8:=shift(net, 8)]
data <- data[seq(-168,-1),]

data$weekday=wday(data$date)
data[, is_monday:=ifelse(weekday==2, 1, 0)]
data[, is_wday:=ifelse(weekday%in%c(3,4,5), 1, 0)]
data[, is_friday:=ifelse(weekday==6, 1, 0)]
data[, is_saturday:=ifelse(weekday==7, 1, 0)]
data[, is_sunday:=ifelse(weekday==1, 1, 0)]

data[, is_over:=ifelse(hour%in% c(17,18,19,20) , 1, 0)]
data[, is_12:=ifelse(hour==12 , 1, 0)]
data[, is_13:=ifelse(hour==13 , 1, 0)]
data[, is_22:=ifelse(hour==22 , 1, 0)]
data[, is_23:=ifelse(hour==23 , 1, 0)]

head(data)

data = data[,!c("V1","upRegulationZeroCoded","upRegulationOneCoded","upRegulationTwoCoded",
                "downRegulationZeroCoded","downRegulationOneCoded","downRegulationTwoCoded",
                 "upRegulationDelivered","downRegulationDelivered")] 
str(data)

test_start="2021-12-01"
test_finish="2021-12-14"

data <- data[date<=test_finish,]

alphabet_size=10

data[,net_new := cut(net, quantile(net, probs = 0:alphabet_size/alphabet_size),
                              labels = FALSE, include.lowest = TRUE)]
summary(data)

data$system_direction <- as.factor(data$system_direction) 

data$net_new <- as.factor(data$net_new) 

data[(net>(-50))& (net<50), ]$net_new=4

data$net_new <- as.factor(data$net_new) 

head(data)

feat=fread("2022-01-22_weather.csv")



wide_feat=dcast(feat, date + hour ~ variable+ lat + lon , value.var="value")
wide_feat=wide_feat[date<Sys.Date(),]

str(wide_feat)

str(wide_feat)

pca_DSWRF=princomp(wide_feat[,seq(1:7)+2,with=F])
summary(pca_DSWRF,loadings=T)

pca_RH=princomp(wide_feat[,seq(1:7)+9,with=F])
summary(pca_RH,loadings=T)

pca_TCDC=princomp(wide_feat[,seq(1:7)+16,with=F])
summary(pca_TCDC,loadings=T)

pca_TMP=princomp(wide_feat[,seq(1:7)+23,with=F])
summary(pca_TMP,loadings=T)

pca_w=princomp(wide_feat[,seq(1:14)+30,with=F])
summary(pca_w,loadings=T)

new_feat=data.table(date=wide_feat$date, hour=wide_feat$hour, DSWRF=pca_DSWRF$scores[,1], RH=pca_RH$scores[,1],
                    TCDC1 = pca_TCDC$scores[,1], TCDC2 = pca_TCDC$scores[, 2], TMP = pca_TMP$scores[, 1], w1 = pca_w$scores[,1],
                    w2 = pca_w$scores[,2], w3 = pca_w$scores[,3])

head(new_feat)

all_dt=merge(x = data, y=new_feat, by=c("date", "hour"), all.x=TRUE)

head(all_dt)





test_date_start="2021-12-01"
test_date_end="2021-12-14"

train=all_dt[date<test_date_start,]
test=all_dt[(date>=test_date_start) & (date<=test_date_end),]

train=train[,-c("date", "net", "system_direction", "ID", "is_over")]
test=test[,-c("date", "net", "system_direction", "ID", "is_over")]



library(caret)

trainclass=train$net_new
traindata=as.matrix(train[, -c(14),with=F])

traindata=scale(traindata)
train=data.table(cl=trainclass,traindata)

ctrl = trainControl(method="repeatedcv",repeats = 3,number=10) 
knnFit=train(cl ~ ., data = train, method = "knn", 
                trControl = ctrl, 
                tuneLength = 10)

knnFit

plot(knnFit)

testclass=test$net_new
testdata=as.matrix(test[, -c(14),with=F])

testdata=scale(testdata)
test=data.table(cl=testclass,testdata)

pred=predict(knnFit, test)

res_dt=data.table(actual=all_dt[(date>=test_date_start) & (date<=test_date_end),]$net_new, 
                  actual_req=all_dt[(date>=test_date_start) & (date<=test_date_end),]$system_direction, predicted=pred)

table(actual=res_dt$actual,  predicted=res_dt$predicted)

res_dt[, converted_pred:=ifelse(predicted %in% c("1","2", "3"), "Negative", 
                         ifelse(predicted %in% c("5","6","7","8","9","10"), "Positive", "Neutral"))]

table(actual=res_dt$actual_req,  predicted=res_dt$converted_pred)

confusionMatrix(data = as.factor(res_dt$converted_pred), reference = as.factor(res_dt$actual_req), mode = "prec_recall")


