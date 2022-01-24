library(data.table)
library(ggplot2)
library(TSrepr)
library(TSdist)
library(dtw)

current_folder="D:/Datasets/IE48B_project"

dist_path=sprintf('%s/distances/project',current_folder)

data=fread("bulk_imbalance.csv")

data[, lag_24:=shift(net, 24)]
data[, lag_168:=shift(net, 168)]
data <- data[seq(-168,-1),]

data$weekday=wday(data$date)
data[, is_wday:=ifelse(weekday%in%c(2,3,4,5,6), 1, 0)]
data[, is_weekend:=ifelse(weekday%in%c(1,7), 1, 0)]
data$weekday=NULL

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



train_start_date="2021-11-15"

test_date_start="2021-12-01"
test_date_end="2021-12-14"

train=all_dt[(date>=train_start_date) & (date<test_date_start),]
test=all_dt[(date>=test_date_start) & (date<=test_date_end),]

train=train[,-c("date", 'net', 'upRegulationZeroCoded', 'upRegulationOneCoded', 'upRegulationTwoCoded',
                'downRegulationZeroCoded', 'downRegulationOneCoded', 'downRegulationTwoCoded', 'upRegulationDelivered',
                'downRegulationDelivered')]
test=test[,-c("date", 'net', 'upRegulationZeroCoded', 'upRegulationOneCoded', 'upRegulationTwoCoded',
                'downRegulationZeroCoded', 'downRegulationOneCoded', 'downRegulationTwoCoded', 'upRegulationDelivered',
                'downRegulationDelivered')]

str(train)

trainclass=train$system_direction
traindata=as.matrix(train[, -c(2),with=F])

traindata=scale(traindata)

testclass=test$system_direction
testdata=as.matrix(test[, -c(2),with=F])

testdata=scale(testdata)

tlength=ncol(traindata)
n_series_train=nrow(traindata)
n_series_test=nrow(testdata)

## Alternative strategies
# knn k=1 Euclidean distance
# knn k=5 Euclidean distance
# knn k=1 DTW distance + no window
# knn k=5 DTW distance + no window
# knn k=1 DTW distance + window.type='sakoechiba' + window.size=10
# knn k=1 DTW distance + window.type='sakoechiba' + window.size=20
# knn k=1 LCSS + epsilon=0.05 + no window
# knn k=5 LCSS + epsilon=0.1  + no window 
# knn k=1 ERP + gap penalty=1 + no window
# knn k=1 ERP + gap penalty=0.5 + no window
# knn k=5 ERP + gap penalty=0.5 + no window


# calculate distances and store them to save time
large_number=10000
dist_euc=as.matrix(dist(traindata))
diag(dist_euc)=large_number
fwrite(dist_euc,sprintf('%s_euc_raw_dist.csv',dist_path),col.names=F)

dist_dtw=as.matrix(dtwDist(traindata))
diag(dist_dtw)=large_number
fwrite(dist_dtw,sprintf('%s_dtw_raw_dist.csv',dist_path),col.names=F)

dist_dtw1=as.matrix(dtwDist(traindata,window.type='sakoechiba',window.size=10))
diag(dist_dtw1)=large_number
fwrite(dist_dtw1,sprintf('%s_dtw_raw_dist_sakoe_10.csv',dist_path),col.names=F)

dist_dtw2=as.matrix(dtwDist(traindata,window.type='sakoechiba',window.size=20))
diag(dist_dtw2)=large_number
fwrite(dist_dtw2,sprintf('%s_dtw_raw_dist_sakoe_20.csv',dist_path),col.names=F)  

# computation of LCSS is from TSdist package
dist_lcss=TSDatabaseDistances(traindata,distance='lcss',epsilon=0.05)
dist_lcss=as.matrix(dist_lcss)
diag(dist_lcss)=large_number
fwrite(dist_lcss,sprintf('%s_lcss_raw_epsilon_005.csv',dist_path),col.names=F)  
                   
dist_lcss=TSDatabaseDistances(traindata,distance='lcss',epsilon=0.1)
dist_lcss=as.matrix(dist_lcss)
diag(dist_lcss)=large_number
fwrite(dist_lcss,sprintf('%s_lcss_raw_epsilon_01.csv',dist_path),col.names=F) 

# computation of ERP is from TSdist package
dist_erp=TSDatabaseDistances(traindata,distance='erp',g=0.5)
dist_erp=as.matrix(dist_erp)
diag(dist_erp)=large_number
fwrite(dist_erp,sprintf('%s_erp_raw_gap_005.csv',dist_path),col.names=F)  
                   
dist_erp=TSDatabaseDistances(traindata,distance='erp',g=1)
dist_erp=as.matrix(dist_erp)
diag(dist_erp)=large_number
fwrite(dist_erp,sprintf('%s_erp_raw_gap_1.csv',dist_path),col.names=F) 

nn_classify_cv=function(dist_matrix,train_class,test_indices,k=1){
    
    test_distances_to_train=dist_matrix[test_indices,]
    test_distances_to_train=test_distances_to_train[,-test_indices]
    train_class=train_class[-test_indices]
    #print(str(test_distances_to_train))
    ordered_indices=apply(test_distances_to_train,1,order)
    if(k==1){
        nearest_class=as.numeric(trainclass[as.numeric(ordered_indices[1,])])
        nearest_class=data.table(id=test_indices,nearest_class)
    } else {
        nearest_class=apply(ordered_indices[1:k,],2,function(x) {trainclass[x]})
        nearest_class=data.table(id=test_indices,t(nearest_class))
    }
    
    long_nn_class=melt(nearest_class,'id')

    class_counts=long_nn_class[,.N,list(id,value)]
    class_counts[,predicted_prob:=N/k]
    wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')
    wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0
    class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]
    
    
    return(list(prediction=class_predictions,prob_estimates=wide_class_prob_predictions))
    
}

result=nn_classify_cv(dist_lcss,trainclass,1:3,k=5)
str(result)

# cv indices start here
require(TunePareto)

set.seed(13429)
nof_rep=10
n_fold=10
cv_indices=generateCVRuns(trainclass, ntimes =nof_rep, nfold = n_fold, 
                          leaveOneOut = FALSE, stratified = TRUE)

str(cv_indices)

dist_folder=sprintf('%s/distances/',current_folder)
dist_files=list.files(dist_folder, full.names=T)


list.files(dist_folder)

k_levels=c(2,5,10)
approach_file=list.files(dist_folder)
result=vector('list',length(dist_files)*nof_rep*n_fold*length(k_levels))
iter=1
for(m in 1:length(dist_files)){ #
    print(dist_files[m])
    dist_mat=as.matrix(fread(dist_files[m],header=FALSE))
    for(i in 1:nof_rep){
        this_fold=cv_indices[[i]]
        for(j in 1:n_fold){
            test_indices=this_fold[[j]]
            for(k in 1:length(k_levels)){
                current_k=k_levels[k]
                current_fold=nn_classify_cv(dist_mat,trainclass,test_indices,k=current_k)
                accuracy=sum(trainclass[test_indices]==current_fold$prediction$predicted)/length(test_indices)
                tmp=data.table(approach=approach_file[m],repid=i,foldid=j,
                               k=current_k,acc=accuracy)
                result[[iter]]=tmp
                iter=iter+1
                
            }
            
        }
    
    }   
    
}


overall_results=rbindlist(result)
overall_results[,list(avg_acc=mean(acc),sdev_acc=sd(acc),result_count=.N),by=list(approach,k)]

head(res_dt[order(avg_acc,decreasing = TRUE)], 8)

require(ggplot2)
ggplot(overall_results,aes(x=paste0(approach,'_when k:',k), y=acc)) +
geom_boxplot()+ xlab("Models")+ylab("Box Plot of Accuracy")+
coord_flip()

all_dt=all_dt[(date<="2021-12-14")& (date>="2021-01-01"),]

all_dt=all_dt[,-c("date", 'net', 'upRegulationZeroCoded', 'upRegulationOneCoded', 'upRegulationTwoCoded',
                'downRegulationZeroCoded', 'downRegulationOneCoded', 'downRegulationTwoCoded', 'upRegulationDelivered',
                'downRegulationDelivered')]

allclass=all_dt$system_direction
alldata=as.matrix(all_dt[, -c(2),with=F])

alldata=scale(alldata)

large_number=10000
dist_euc_all=as.matrix(dist(alldata))
diag(dist_euc_all)=large_number

test_indices=(nrow(alldata)-335):nrow(alldata)# 2 weeks 
k=10 
dist_matrix_all=dist_euc_all
test_distances_all=dist_matrix_all[test_indices,]

ordered_indices=apply(test_distances_all,1,order)
nearest_class=apply(ordered_indices[1:k,],2,function(x) {allclass[x]})
nearest_class=data.table(id=test_indices,t(nearest_class))

#nearest_class
long_nn_class=melt(nearest_class,'id')

class_counts=long_nn_class[,.N,list(id,value)]
class_counts[,predicted_prob:=N/k]
wide_class_prob_predictions=dcast(class_counts,id~value,value.var='predicted_prob')

wide_class_prob_predictions[is.na(wide_class_prob_predictions)]=0


class_predictions=class_counts[,list(predicted=value[which.max(N)]),by=list(id)]

library(caret)

confusionMatrix(data = as.factor(class_predictions$predicted), reference = as.factor(all_dt[(nrow(all_dt)-335):nrow(all_dt)]$system_direction), mode = "prec_recall")


