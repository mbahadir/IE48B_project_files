require(data.table)
require(tidyverse)
require(lubridate)
require(ggplot2)

setwd("C:/Users/brkmb/Desktop/kitaplar/IE48B/project files")
dat=fread('YAT-01012019-10122021.csv')

setnames(dat,names(dat),c('date','hour','yat_one','yat_two','yat_three'))
dat[,datex:=strptime(date,'%d/%m/%Y')]

dat[,tst:=ymd_hm(paste(datex,hour))]
dat[,date:=date(tst)]
dat[,hour:=hour(tst)]

dat[,yat_one_t:=gsub('\\.','',yat_one)]
dat[,yat_two_t:=gsub('\\.','',yat_two)]
dat[,yat_three_t:=gsub('\\.','',yat_three)]


dat[,yat_one:=as.numeric(gsub(',','.',yat_one_t))]
dat[,yat_two:=as.numeric(gsub(',','.',yat_two_t))]
dat[,yat_three:=as.numeric(gsub(',','.',yat_three_t))]

yat_dat=dat[,list(date,hour,yat_one,yat_two,yat_three)]

yat_dat=dat[,list(date,hour,yat_one,yat_two,yat_three)]


dat=fread('YAL-01012019-10122021.csv')

# naming is temporary here, used the same set of codes
setnames(dat,names(dat),c('date','hour','yat_one','yat_two','yat_three'))
dat[,datex:=strptime(date,'%d/%m/%Y')]

dat[,tst:=ymd_hm(paste(datex,hour))]
dat[,date:=date(tst)]
dat[,hour:=hour(tst)]

dat[,yat_one_t:=gsub('\\.','',yat_one)]
dat[,yat_two_t:=gsub('\\.','',yat_two)]
dat[,yat_three_t:=gsub('\\.','',yat_three)]


dat[,yal_one:=as.numeric(gsub(',','.',yat_one_t))]
dat[,yal_two:=as.numeric(gsub(',','.',yat_two_t))]
dat[,yal_three:=as.numeric(gsub(',','.',yat_three_t))]


yal_dat=dat[,list(date,hour,yal_one,yal_two,yal_three)]

data = merge(yat_dat, yal_dat, by = c("date","hour"))

data[, SumYat := yat_one + yat_two + yat_three]
data[, SumYal := yal_one + yal_two + yal_three]
data[, Imbalance := SumYat - SumYal]
data[, Label := ifelse(Imbalance <= 50 & Imbalance >= -50, 0, sign(Imbalance))]
data = data[order(date, hour)]
data[, ID := 1:.N]
head(data)
str(data)

head(b1data)

#b1datawide <- dcast(melt(b1datawide, id.vars = "date"), variable ~ date)

b1data =  data[,b1_label := lag(Label,24)]

b1data = data[,b2_label := lag(Label,168)]

day_baseline <- data$b1_label
week_baseline <- data$b2_label



d <- table(data$Label,data$b1_label)
d
sum(diag(d))/sum(d)
sum(d)

d <- table(data$Label,data$b2_label)
d
sum(diag(d))/sum(d)

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

deneme <- LongtoWide(data, c("Imbalance","Label"))
deneme[Type == "Imbalance"]
is.data.table(deneme)

hour <- 0
hour 

datex = '2019-01-02'
datex = ymd('2019-01-02') + days(1)
datex


date_index <- '2019-01-02'

sign <- LongtoWide(data, c("Label"))
    sign <- sign [date <= date_index,]
    sign <- sign [, hour+2, with=FALSE]
sign

PreviousSigns <- function(date_index,hour){
    sign <- LongtoWide(data, c("Label"))
    sign <- sign [date <= date_index,]
    sign <- sign [, hour+2, with=FALSE]
    
    value <- LongtoWide(data, c("Imbalance"))
    value <- value [date <= date_index,]
    value <- value [, hour+2, with=FALSE]
    
    date_inf <- LongtoWide(data, c("Label"))
    date_inf <- date_inf[date<=date_index]
    date_inf <- date_inf[,1]
    date_inf
    
    result <- cbind(date_inf,sign, value)

    setnames(result,c('date','Label','Imbalance'))
    result[,Hour := hour]
    #counts <- table(result[,2])
    #print(paste0("The number of ","Negative Balance(-1): ",counts[1],", Positive Balance(1): ",counts[2]))
    print(result)
    
    return(result)
    }

deneme1 <- PreviousSigns("2019-01-5", 4)





Baselineday<- function(day, hour){ #Shows 1 day before 
    
    day = ymd(day) - days(1) 
    
    sign <- LongtoWide(data, c("Label"))
    sign <- sign[date == day,]
    sign <- sign[,hour+2, with=FALSE]
    
    value <- LongtoWide(data, c("Imbalance"))
    value <- value [date == day,]
    value <- value [, hour+2, with=FALSE]
    
    date_inf <- LongtoWide(data, c("Label"))
    date_inf <- date_inf[date==day]
    date_inf <- date_inf[,1]
    date_inf
    
    result <- cbind(date_inf,sign, value)

    setnames(result,c('date','Label','Imbalance'))
    result[,Hour := hour]
    #print(result)
    return(result)
    
    }
Baselineweek <- function(day, hour){#Shows 1 week before
      day = ymd(day) - days(7) 
    
    sign <- LongtoWide(data, c("Label"))
    sign <- sign[date == day,]
    sign <- sign[,hour+2, with=FALSE]
    
    value <- LongtoWide(data, c("Imbalance"))
    value <- value [date == day,]
    value <- value [, hour+2, with=FALSE]
    
    date_inf <- LongtoWide(data, c("Label"))
    date_inf <- date_inf[date==day]
    date_inf <- date_inf[,1]
    date_inf
    
    result <- cbind(date_inf,sign, value)

    setnames(result,c('date','Label','Imbalance'))
    result[,Hour := hour]
    print(result)
    return(result)
    
}

deneme2 <- Baselineday('2019-01-10', 4)
deneme3 <- Baselineweek(20190110, 4)

h
dates[1]

Baselineday(dates[2],h)


CreateWindows <- function(dataset, col_names, t, p) {
    t = t-3
    time_indices = seq(t - p + 1,t)
    time_indices = time_indices + (time_indices<0)*(24)
    
    t = t-3
    time_indices = seq(t - p + 1,t)
    neg_time_indices = which(time_indices<0)
    pos_time_indices = which(time_indices>=0)
    time_indices = time_indices + (time_indices<0)*(24)

    data_wide <- LongtoWide(dataset[hour %in%  time_indices[pos_time_indices]], col_names)
    
    if(length(neg_time_indices) > 0){
        prev_dat <- LongtoWide(dataset[hour %in% time_indices[neg_time_indices],], col_names)
        #dcast(dataset[hour %in% time_indices[neg_time_indices],], date ~ (hour-24), value.var = col_names)
        prev_dat[,date := date + days(1)]

        Result <- merge(prev_dat, data_wide, by.x = c("date","Type"), by.y =  c("date","Type"))
                        ##by.y = c("date","Type"))
        return(Result)
    } else{
        Result <- data_wide
        return(Result)}
}

deneme <- CreateWindows(data,c("Imbalance", "Label"),12,15)
head(deneme)
