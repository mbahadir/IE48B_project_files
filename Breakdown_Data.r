require(data.table)
require(lubridate)
require(ggplot2)
require(tidyr)

directory = setwd("C:/Users/Burak/Documents/kitaplar/IE48B/project_files/ariza_bakim/")

folderpath = sprintf('%s', directory)
datasetname = 'ArizaBakim-01012019-11012022.csv' 
datapath = sprintf('%s/%s', folderpath,datasetname) 
dat = fread(datapath)
head(dat)

dat = dat[,4:7]
setnames(dat, names(dat), c('Start_Time','Finish_Time','Nominal_power','capacity_on_working'))
head(dat)

dat = dat [,Start_Time := dmy_hm(Start_Time)]
dat = dat[,Finish_Time := dmy_hm(Finish_Time)]
head(dat)

dat = dat[, check := date(Finish_Time)-date(Start_Time)] #check all of them starts and finishes at same day
sum(dat$check) #returns 0, means there is not any failure which lasts more than 1 day
dat$check <- NULL

dat = dat[order(Start_Time)]
dat = dat[, duration:= difftime(Finish_Time,Start_Time,units = "min")]
dat$duration <- as.numeric(dat$duration)
dat[,hours_passed := duration/60]
dat[,hours_duration := ceiling(hours_passed)]
head(dat)

dat[,Start_Hour := hour(dat$Start_Time)]
dat[,Finish_Hour := hour(dat$Finish_Time)]
dat[,Day := as.Date(Start_Time)]
dat[,Breakdown_ID := 1:.N ]
head(dat)

seqs = mapply(FUN = function(a, b) {seq(from = a, to = b, by = 1)}, a = dat$Start_Hour, b = dat$Finish_Hour)
dates = mapply(FUN = function(a, b) {rep(a,times = length(b))}, a = dat$Day, b = seqs)
nom_power = mapply(FUN = function(a, b) {rep(a,times = length(b))}, a = dat$Nominal_power, b = seqs)
cap = mapply(FUN = function(a, b) {rep(a,times = length(b))}, a = dat$capacity_on_working, b = seqs)
ID = mapply(FUN = function(a, b) {rep(a,times = length(b))}, a = dat$Breakdown_ID, b = seqs)
breakdown = data.table(ID = unlist(ID), Date = do.call("c", dates), Hour = unlist(seqs), NominalPower = unlist(nom_power),
                       Capacity = unlist(cap))
breakdown[,NominalPower:=as.numeric(gsub(",","",NominalPower))/100]
breakdown[,Capacity:=as.numeric(gsub(",","",Capacity))/100]
head(breakdown,10)

breakdown = as.data.table(aggregate(breakdown[,5], by = list(breakdown$Date,breakdown$Hour),FUN=sum))

setnames(breakdown, names(breakdown), c('Date','Hour','Capacity'))
breakdown[order(Date)]
breakdown[,Trend := 1:.N ]

nrow(breakdown)

ggplot(breakdown[Date < "2022-01-01"], aes(x = Date))  +geom_line(aes(y=Capacity,color='Cap'))

acf(breakdown$Capacity)

require("stats")
require("tsoutliers")



decomp=decompose(ts(breakdown$Capacity,freq=7),type='add')
plot(decomp)

decomp=decompose(ts(breakdown$Capacity,freq=168),type='add')
plot(decomp)

decomp=decompose(ts(breakdown$Capacity,freq=720),type='add')
plot(decomp)

path="C:/Users/Burak/Documents/kitaplar/IE48B/project_files/Random Forest/"
bulk_imbalance = fread(sprintf("%sbulk_imbalance.csv", path))
data = bulk_imbalance
data = data[order(date,hour)]
data[,ID := 1:.N]
head(data)

combined <- merge(breakdown,data,by.x =c("Date","Hour"), by.y =c("date","hour"))

tail(combined)

ggplot(combined, aes(x=Date))+ geom_line(aes(y=Capacity,color='Capacity'))+geom_line(aes(y=net,color='net'))

correlation <- cor(combined[,list(Capacity,net,upRegulationZeroCoded,upRegulationZeroCoded,upRegulationTwoCoded,downRegulationZeroCoded,downRegulationOneCoded,downRegulationTwoCoded,upRegulationDelivered,downRegulationDelivered)])
correlation

correlation2 <- cor(combined[,list(Capacity,net,upRegulationZeroCoded,upRegulationZeroCoded,downRegulationZeroCoded,downRegulationOneCoded,upRegulationDelivered,downRegulationDelivered)])
correlation2

require(corrplot)

corrplot(correlation2, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
