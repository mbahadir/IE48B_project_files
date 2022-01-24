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

path="C:/Users/alpsr/Desktop/IE 48B Project/Codes/"

options(repr.matrix.max.cols=50, repr.matrix.max.rows=100)
options(warn=-1)
sprintf("%sbulk_imbalance.csv", path)

#"C:\Users\alpsr\Desktop\IE 48B Project\Codes\bulk_imbalance.csv"

bulk_imbalance = fread(sprintf('%sbulk_imbalance.csv', path))
str(bulk_imbalance)

data = bulk_imbalance
data = data[order(date,hour)]
data[,ID := 1:.N]

head(data, 128)

skim(data)

data[,net := na.aggregate(net,by=.(hour,wday(date)))]
acf(data$net)

acf <- acf(data$net,lag.max = 169)

acf

data[hour < 12,last_obs := shift(net,7)]
data$last_obs <- c(rep(NA,7),na.locf(data$last_obs))
data[, lag_24:=shift(net, 24)]
data[, lag_168:=shift(net, 168)]
data[, lag_6:=shift(net, 6)]
data[, lag_7:=shift(net, 7)]
data[, lag_8:=shift(net, 8)]
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

upper_limit=quantile(data$net, 0.975)
lower_limit=quantile(data$net, 0.025)
data[, is_outlier:=ifelse((net<=upper_limit) &  (net>=lower_limit), 0, 1)]
data[, net_shift:=shift(net, 168)]
data[is_outlier==1, net:=net_shift]
data$net_shift=NULL
data$is_outlier=NULL

skim(data)

data[,net := na.aggregate(net,by=.(hour,wday(date)))]

corr <- cor(data[,-c('date','ID', 'system_direction')])
ggcorrplot(corr)
#corr

perf_dt=function(type,actual,forecast){
    name=type
    n=length(actual)
    error=actual-forecast
    mean=mean(actual)
    sd=sd(actual)
    FBias=sum(error)/sum(actual)
    MPE=sum(error/actual)/n
    MAPE=sum(abs(error/actual))/n
    RMSE=sqrt(sum(error^2))/n
    MAD=sum(abs(error))/n
    WMAPE=MAD/mean
    l=data.frame(name,n,mean,sd,FBias,MAPE,RMSE,MAD,WMAPE)
    return(l)
}

head(data)

data[,hour_num := hour]
data[,hour := as.factor(hour)]
train=data[date <= "2021-11-30",]
test=data[date >= "2021-12-01" & date<="2021-12-14",]

head(train)

head(test)

trend_lm <- lm(net~ID,train)
summary(trend_lm)

dayoftheweek_lm <- lm(net ~ is_monday+is_wday+is_friday+is_saturday+is_sunday, train)
summary(dayoftheweek_lm)

dayoftheweek2_lm <- lm(net ~ weekday, train)
summary(dayoftheweek2_lm)

hour_lm <- lm(net ~ is_over+is_12+is_13+is_23+is_22, train)
summary(hour_lm)

hour2_lm <- lm(net ~ hour, train)
summary(hour2_lm)

lag_lm <- lm(net ~ lag_24+lag_168+last_obs,train)
summary(lag_lm)

ggplot(train, aes(x = ID))+
geom_line(aes(y = net, color = 'net'))+
geom_line(aes(y = predict(lag_lm,train), color = 'fitted'))

comb1 <- lm(net ~ hour + is_monday + is_wday + is_friday + is_saturday + is_sunday + lag_24 + lag_168 + last_obs,train)
summary(comb1)

ggplot(train, aes(x=ID))+
geom_line(aes(y=net, color="real"))+
geom_line(aes(y=predict(comb1,train), color = 'predicted'))

ggplot(train, aes(x=ID))+
geom_line(aes(y=net - predict(comb1,train), color = 'predicted'))

residuals = train$net - comb1$fitted
mean(residuals)
summary(ur.kpss(residuals))

train[,Residuals1 := predict(comb1, train) - net]
tree1 = rpart(Residuals1 ~ hour + is_monday + is_wday + is_friday + is_saturday + is_sunday + lag_24 + lag_168 + last_obs,
              train,control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(tree1)

train[, Binary1 := ifelse(lag_168 < 8600 & lag_168 >= 7338 & (hour %in% c(10,11,14,15,16,17)),1,0)]

comb2 <- lm(net ~ ID + hour + is_monday + is_wday + is_friday + is_saturday +
            is_sunday + lag_24 + lag_168 + last_obs + Binary1, train)
summary(comb2)

train[,Residuals2 := predict(comb2, train) - net]
tree2 = rpart(Residuals2 ~ ID + hour + is_monday + is_wday + is_friday +
              is_saturday + is_sunday + lag_24 + lag_168 + last_obs + Binary1,
              train,control=rpart.control(cp=0,maxdepth=4))
fancyRpartPlot(tree2)

train[, Binary2 := ifelse(lag_168 < 7408 & lag_168 < 2909 & lag_24 >= 3489 & lag_168 < -860,1,0)] #2375
train[, Binary3 := ifelse(lag_168 >= 7408 & ID >= 23000 & lag_168 < 8387,1,0)] # 3375
train[, Binary4 := ifelse(lag_168 >= 7408 & ID >= 23000 & lag_168 >= 8387,1,0)] # 2061

comb3 <- lm(net ~ ID + hour + is_monday + is_wday + is_friday + is_saturday +
            is_sunday + lag_24 + lag_168 + last_obs + Binary1 + Binary2 + Binary3 + Binary4, train)
summary(comb3)

train[,Residuals3 := predict(comb3, train) - net]
tree3 = rpart(Residuals3 ~ ID + hour + is_monday + is_wday + is_friday +
              is_saturday + is_sunday + lag_24 + lag_168 + last_obs
              + Binary1 + Binary2 + Binary3 + Binary4,
              train,control=rpart.control(cp=0,maxdepth=4))

fancyRpartPlot(tree3)

train[, Binary5 := ifelse(lag_168 < 8387 & lag_168 > 7408 & ID < 23000,1,0)]
train[, Binary6 := ifelse(lag_168 >= 8387 & lag_168 > 7408 & ID < 23000,1,0)]
comb4 <- lm(net ~ ID + hour + is_monday + is_wday + is_friday + is_saturday +
            is_sunday + lag_24 + lag_168 + last_obs + Binary1 + Binary2 + Binary3 +
            Binary4 + Binary5 + Binary6, train)

summary(comb4)

ggplot(train, aes(x=net))+
geom_point(aes(y = comb4$residuals))+
labs(y = "Residuals")+
labs(x = "Fitted")

checkresiduals(comb4$residuals)

train[, Final_Predict := predict(comb4, train)]
perf_dt("EBLR",train$net,train$Final_Predict)

data[, Binary1 := ifelse(lag_168 < 8600 & lag_168 >= 7338 & (hour %in% c(10,11,14,15,16,17)),1,0)]
data[, Binary2 := ifelse(lag_168 < 7408 & lag_168 < 2909 & lag_24 >= 3489 & lag_168 < -860,1,0)] #2375
data[, Binary3 := ifelse(lag_168 >= 7408 & ID >= 23000 & lag_168 < 8387,1,0)] # 3375
data[, Binary4 := ifelse(lag_168 >= 7408 & ID >= 23000 & lag_168 >= 8387,1,0)] # 2061
data[, Binary5 := ifelse(lag_168 < 8387 & lag_168 > 7408 & ID < 23000,1,0)]
data[, Binary6 := ifelse(lag_168 >= 8387 & lag_168 > 7408 & ID < 23000,1,0)]

start = as.Date('2021-12-01')
end = as.Date('2021-12-15')

estimates <- matrix(0,nrow = 14, ncol = 18)

for(i in seq(0,13)){
    
    t_train= data[(date <= start-1+i) | (date == (start + i) & hour_num <= 5) ,]
    t_test = data[date == (start + i) & hour_num > 5,]
    
    # BUILD THE LINEAER MODEL
    linmodel <- lm(net ~ ID + hour + is_monday + is_wday + is_friday + is_saturday +
                    is_sunday + lag_24 + lag_168 + last_obs + Binary1 + Binary2 + Binary3 + Binary4 + Binary5,t_train)
    
    # ESTIMATE THE LINEAR MODEL
    linest = forecast(linmodel,t_test)
    estimates[i+1,] <- linest$mean
}

estimates

pred_vec <- c(estimates[,seq(7,18)]) # these are predictions that we care about

est_directions <- sign(pred_vec)
est_directions[pred_vec<=50&pred_vec>=-50] <- 0

est_directions

test_final = data[date >= start & (end > date) & (hour_num >= 12),]
test_final

real_directions <- sign(test_final$net)
real_directions

real_directions[test_final$net <= 50 & test_final$net >= -50] <- 0
print(sum(real_directions == est_directions) / length(real_directions))

df <- data.frame (real  = real_directions,
                  predicted = est_directions)
table(df)

confusionMatrix(data = as.factor(est_directions), reference = as.factor(real_directions), mode = "prec_recall")
