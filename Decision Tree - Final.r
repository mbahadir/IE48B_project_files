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

path="C:/Users/alpsr/Desktop/IE 48B Project/Codes/"
#options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)
options(warn=-1)
bulk_imbalance = fread(sprintf("%sbulk_imbalance.csv", path))
data = bulk_imbalance
data = data[order(date,hour)]
data[,ID := 1:.N]
str(data)

head(data, 128)

skim(data)

# completing the missing values in the net columns
data[,net := na.aggregate(net,by=.(hour,wday(date)))]

# checking the autocorrelations
acf(data$net)

acf <- acf(data$net,lag.max = 169)

# creating the lagged variables, we will not have access to lag6, lag7, and lag8 values while submitting our predictions
data[, lag_24:=shift(net, 24)]
data[, lag_168:=shift(net, 168)]
data[, lag_6:=shift(net, 6)]
data[, lag_7:=shift(net, 7)]
data[, lag_8:=shift(net, 8)]
data <- data[seq(-168,-1),]

# creating binary variables for the day of the week
data$weekday=wday(data$date)
data[, is_monday:=ifelse(weekday==2, 1, 0)]
data[, is_wday:=ifelse(weekday%in%c(3,4,5), 1, 0)]
data[, is_friday:=ifelse(weekday==6, 1, 0)]
data[, is_saturday:=ifelse(weekday==7, 1, 0)]
data[, is_sunday:=ifelse(weekday==1, 1, 0)]

# creating binary variables for the hour of the day
data[, is_over:=ifelse(hour%in% c(17,18,19,20) , 1, 0)]
data[, is_12:=ifelse(hour==12 , 1, 0)]
data[, is_13:=ifelse(hour==13 , 1, 0)]
data[, is_22:=ifelse(hour==22 , 1, 0)]
data[, is_23:=ifelse(hour==23 , 1, 0)]

# removing outliers from the dataset
# outliers := 0.025 quantile on the both sides
upper_limit=quantile(data$net, 0.975)
lower_limit=quantile(data$net, 0.025)
data[, is_outlier:=ifelse((net<=upper_limit) &  (net>=lower_limit), 0, 1)]
data[, net_shift:=shift(net, 168)]
data[is_outlier==1, net:=net_shift]
data$net_shift=NULL
data$is_outlier=NULL

data[,net := na.aggregate(net,by=.(hour,wday(date)))]

corr <- cor(data[,-c('date','ID', 'system_direction')])
ggcorrplot(corr)

features = fread(sprintf("%s2022-01-22_weather.csv", path))

wide_feat = dcast(features, date + hour ~ variable+ lat + lon , value.var="value")
#tail(wide_feat)
#colnames(wide_feat)

mod_cols <- colnames(wide_feat)[c(-1,-2)]

wide_feat[ , (mod_cols) := lapply(.SD,na.aggregate, by=.(hour,wday(date))), .SDcols = mod_cols]
wide_feat[ , hour := as.factor(hour)]
#wide_feat[,net := na.aggregate(net,by=.(hour,wday(date)))]

head(wide_feat)

corr <- cor(wide_feat[,-c('date', 'hour')])
ggcorrplot(corr)

data[,hour_num := hour]
data[,hour := as.factor(hour)]
train = data[date <= "2021-11-30",]
test = data[date >= "2021-12-01" & date <= "2021-12-14",]

tail(train,24)

train_feat <- wide_feat[date <= "2021-11-30",]
test_feat <- wide_feat[date >= "2021-12-01" & date <= "2021-12-14",]

train <- merge(train, train_feat, by.x = c("date","hour"),by.y = c("date","hour"))
test <- merge(test, test_feat, by.x = c("date","hour"), by.y = c("date","hour"))

pca_rep_dswrf = princomp(train[,c(31:37)])
summary(pca_rep_dswrf) # one column covers 95%. One component added to the dataset

train[,DSWRF:=pca_rep_dswrf$scores[,1]]
test[, DSWRF := predict(pca_rep_dswrf,test)[,1]]

pca_rep_rh = princomp(train[,c(38:44)])
summary(pca_rep_rh) #three components cover 87% of the variance. Three components added to the data set.

train[,RH1 := pca_rep_rh$scores[,1]]
test[, RH1 := predict(pca_rep_rh,test)[,1]]

train[,RH2 := pca_rep_rh$scores[,2]]
test[, RH2 := predict(pca_rep_rh,test)[,2]]

train[,RH3 := pca_rep_rh$scores[,3]]
test[, RH3 := predict(pca_rep_rh,test)[,3]]

pca_rep_tcdc = princomp(train[,c(45:51)])
summary(pca_rep_tcdc) #three components cover 74% of the variance. Three components added to the data set.

train[,tcdc1 := pca_rep_tcdc$scores[,1]]
test[, tcdc1 := predict(pca_rep_tcdc,test)[,1]]

train[,tcdc2 := pca_rep_tcdc$scores[,2]]
test[, tcdc2 := predict(pca_rep_tcdc,test)[,2]]

train[,tcdc3 := pca_rep_tcdc$scores[,3]]
test[, tcdc3 := predict(pca_rep_tcdc,test)[,3]]

pca_rep_tmp = princomp(train[,c(52:58)])
summary(pca_rep_tmp) #one component cover 95% of the variance. One component added to the data set.

train[,tmp := pca_rep_tmp$scores[,1]]
test[, tmp := predict(pca_rep_tmp,test)[,1]]

pca_rep_wdir = princomp(train[,c(59:65)])
summary(pca_rep_wdir) # results are not great. no components are added.

#train[,wdir := pca_rep_wdir$scores[,1]]
#test[, wdir := predict(pca_rep_wdir,test)[,1]]

pca_rep_wda = princomp(train[,c(66:72)])
summary(pca_rep_wda) #three components cover 74% of the variance. Three components added to the data set.

train[,wda1 := pca_rep_wda$scores[,1]]
test[, wda1 := predict(pca_rep_wda,test)[,1]]

train[,wda2 := pca_rep_wda$scores[,2]]
test[, wda2 := predict(pca_rep_wda,test)[,2]]

train[,wda3 := pca_rep_wda$scores[,3]]
test[, wda3 := predict(pca_rep_wda,test)[,3]]

getModelInfo("rpart2")

ctrl = trainControl(method="repeatedcv",repeats = 3,number=10) 
#colnames(train)

training = train[hour_num >= 12,c("system_direction", "lag_24", "lag_168", "is_monday", "is_wday", "is_friday", "is_saturday",
                    "is_sunday","is_over","is_12","is_13","is_22","is_23","wdir_10m_36.5_32.5", "wdir_10m_37_35.5", "wdir_10m_38_32.5",
                    "wdir_10m_38.5_27", "wdir_10m_39.75_30.5", "wdir_10m_40_33", "wdir_10m_41_28.75",
                    "DSWRF","RH1","RH2","tcdc1","tcdc2","RH3","tcdc3","tmp","wda1","wda2","wda3")]
trainclass = training$system_direction

testing = test[hour_num >= 12,c("system_direction", "lag_24", "lag_168", "is_monday", "is_wday", "is_friday", "is_saturday",
                    "is_sunday","is_over","is_12","is_13","is_22","is_23","wdir_10m_36.5_32.5", "wdir_10m_37_35.5", "wdir_10m_38_32.5",
                    "wdir_10m_38.5_27", "wdir_10m_39.75_30.5", "wdir_10m_40_33", "wdir_10m_41_28.75",
                    "DSWRF","RH1","RH2","tcdc1","tcdc2","RH3","tcdc3","tmp","wda1","wda2","wda3")]
testclass=testing$system_direction

head(training)

model1 <- train(
            system_direction ~ .,
            data = training,
            method = "rpart2",
            trControl = ctrl,
            tuneLength = 5)

model1

dec_tree1 = rpart(system_direction ~ .,training,method='class',control=rpart.control(cp=0,maxdepth=2))
fancyRpartPlot(dec_tree1)

train_fit = predict(dec_tree1,training)
train_pred = data.table(train_fit)
train_pred_label = colnames(train_pred)[apply(train_pred,1,which.max)]

table(train_pred_label,trainclass)
print(sum(train_pred_label == trainclass)/length(trainclass))

confusionMatrix(data = as.factor(train_pred_label), reference = as.factor(trainclass), mode = "prec_recall")

test_fit = predict(dec_tree1,testing)
test_pred = data.table(test_fit)
test_pred_label = colnames(test_pred)[apply(test_pred,1,which.max)]

table(test_pred_label,testclass)
print(sum(test_pred_label == testclass)/length(testclass))

confusionMatrix(data = as.factor(test_pred_label), reference = as.factor(testclass), mode = "prec_recall")

path="C:/Users/alpsr/Desktop/IE 48B Project/Codes/"

bulk_imbalance = fread(sprintf("%sbulk_imbalance.csv", path))
data_window = bulk_imbalance[order(date,hour)]
data_window[,ID := 1:.N]

data_window[,net := na.aggregate(net,by=.(hour,wday(date)))]

upper_limit=quantile(data_window$net, 0.975)
lower_limit=quantile(data_window$net, 0.025)
data_window[, is_outlier:=ifelse((net<=upper_limit) &  (net>=lower_limit), 0, 1)]
data_window[, net_shift:=shift(net, 168)]
data_window[is_outlier==1, net:=net_shift]
data_window$net_shift=NULL
data_window$is_outlier=NULL
data_window[,net := na.aggregate(net,by=.(hour,wday(date)))]

LongtoWide <- function(datatable, targetvalue){
  new <- data.table()
  for(i in seq(1:length(targetvalue))){
    temp <- dcast(datatable, date ~ hour, value.var = targetvalue[i])
    temp[,Type := targetvalue[i]]
    #print(head(new))
    new <- rbind(new,temp)
  }
  return(new)
}

Result_list <- list()

for(i in seq(12,23)){
  p = 8
  t = i
  remove(Result)
  remove(data_window_wide)
  remove(prev_dat)
  remove(time_indices)
  remove(pos_time_indices)
  remove(neg_time_indices)
  
  t = t-18
  time_indices = seq(t - p + 1,t)
  neg_time_indices = which(time_indices<0)
  pos_time_indices = which(time_indices>=0)
  time_indices = time_indices + (time_indices<0)*(24)
  
  if(length(pos_time_indices) > 0){
    data_window_wide <- LongtoWide(data_window[hour %in%  time_indices[pos_time_indices]], "net")
  }
  
  if(length(neg_time_indices) > 0){
    prev_dat <- LongtoWide(data_window[hour %in% time_indices[neg_time_indices],], "net")
    prev_dat[,date := ymd(date) + ddays(1)]
    #Result <- prev_dat
  }
  
  if(length(neg_time_indices) > 0 & length(pos_time_indices) > 0){
    #prev_dat <- LongtoWide(data_window[hour %in% time_indices[neg_time_indices],], "net")
    #dcast(data_windowset[hour %in% time_indices[neg_time_indices],], date ~ (hour-24), value.var = col_names)
    #prev_dat[,date := ymd(date) + ddays(1)]
    Result <- merge(prev_dat, data_window_wide, by.x = c("date","Type"), by.y =  c("date","Type"))
  } else if(length(neg_time_indices) > 0 & length(pos_time_indices) <= 0){
      Result <- prev_dat
  } else if(length(neg_time_indices) <= 0 & length(pos_time_indices) > 0){
      Result <- data_window_wide
  }
  else{
    continue
  }
  
  Result[,hour := i]
  Result[,Type := NULL]
  colnames(Result) <- c("date","t1","t2","t3","t4","t5","t6","t7","t8","hour")
  
  Result_list <- append(Result_list,list(Result)) 
  
}

windows <- rbindlist(Result_list)
windows <- windows[order(date,hour)]
head(windows)
nrow(windows)

dec_tree2 = rpart(system_direction ~ .,training,method='class',control=rpart.control(cp=0,maxdepth=8))
fancyRpartPlot(dec_tree2)

nodes = data.table(train[,c("date","hour","system_direction")],
                   node=rpart.predict.leaves(dec_tree2,training))
nodes_test = data.table(test[,c("date","hour","system_direction")],
                       node = rpart.predict.leaves(dec_tree2,testing))
nodes[,hour := as.numeric(hour)]
nodes_test[,hour := as.numeric(hour)]

head(nodes_test)

training_window = merge(windows,nodes, by = c("date","hour"))
testing_window = merge(windows,nodes_test, by = c("date","hour"))

head(training_window)
head(testing_window)

model2 <- train(
    system_direction ~ t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + node,
    data = training_window,
    method = "knn",
    trControl = ctrl,
    tuneLength = 5)

model2

predictions <- predict(model2, testing_window)

table(testing_window$system_direction,predictions)

confusionMatrix(data = as.factor(testing_window$system_direction), reference = as.factor(predictions), mode = "prec_recall")
