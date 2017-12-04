library(data.table)
library(plyr)
library(entropy)
library(stringr)
library(rpart)
library(party)
library(xgboost)
library(caret)

train <- read.table('E://Files/Columbia/17fall Courses/4995 CS DATA SCIENCE/HW3/train.csv', sep = ',', header = T)
test <- read.table('E://Files/Columbia/17fall Courses/4995 CS DATA SCIENCE/HW3/test.csv', sep = ',', header = T)

#---data cleaning---#
#change factors to characters
train$Upc = as.character(train$Upc)
test$Upc = as.character(test$Upc)
train$Weekday = as.character(train$Weekday)
train$DepartmentDescription = as.character(train$DepartmentDescription)
test$Weekday = as.character(test$Weekday)
test$DepartmentDescription = as.character(test$DepartmentDescription)

max(train$FinelineNumber, na.rm=TRUE)
max(test$FinelineNumber, na.rm=TRUE)

#max = 9999, so we replace NA with 10001
train$FinelineNumber[which(is.na(train$FinelineNumber))] <- 10001
test$FinelineNumber[which(is.na(test$FinelineNumber))] <- 10001

#after observe train$Upc, 4011 seems to be default number
#so we set all NA ('') to 4011
train$Upc[which(train$Upc == '')] <- "4011"
test$Upc[which(test$Upc == '')] <- "4011"

#check dept list and visiter list
deptintrain <- sort(unique(train$DepartmentDescription))
deptintest <- sort(unique(test$DepartmentDescription))
Visitertrain<-sort(unique(train$VisitNumber))

#after checking, we find that 'HEALTH AND BEAUTY AIDS' is not listed in test, 
train$DepartmentDescription[which(train$DepartmentDescription == 'HEALTH AND BEAUTY AIDS')] <- 'OTHER DEPARTMENTS'
deptintrain <- sort(unique(train$DepartmentDescription))

#feature generating functions
positive_count <- function(x){length(x$ScanCount[x$ScanCount > 0])}
negative_count <- function(x){length(x$ScanCount[x$ScanCount<0])}
positive_sum <- function(x){sum(x$ScanCount[x$ScanCount>0])}
negative_sum <- function(x){abs(sum(x$ScanCount[x$ScanCount<0]))}
positive_ratio<-function (x){
    a=sum(x$ScanCount[x$ScanCount>0])
    b=abs(sum(x$ScanCount[x$ScanCount<0]))
    cumSum=a+b
    r=a/cumSum
    return (r)
}
negative_ratio<-function (x){
    a=sum(x$ScanCount[x$ScanCount>0])
    b=abs(sum(x$ScanCount[x$ScanCount<0]))
    cumSum=a+b
    r=b/cumSum
    return (r)
}
departments_count <- function(x){length(unique(x$DepartmentDescription))}
insertdeptframe<-function(x){
    k<-ddply(x,c('DepartmentDescription'),function (d) c(counts=sum(abs(d$ScanCount))))
    ##include raw counts also as features
    raw_counts<-c(rep(0,68))
    names(raw_counts)<-c(deptintrain)
    for (i in 1:nrow(k))
    {
         raw_counts[as.character(k$DepartmentDescription[i])]<-k$counts[i]
    }
    names(raw_counts)<-c(paste("raw_",c(1:68)))
    k$counts<-k$counts/sum(k$counts)
    v<-c(rep(0,71))
    names(v)<-c(deptintrain,"dif_peaks","sum_peaks","sd_dist")
    for (i in 1:nrow(k))
    {
         v[as.character(k$DepartmentDescription[i])]<-k$counts[i]
    }
    k<-sort(v,decreasing=TRUE)
    v["dif_peaks"]<-k[1]-k[2]
    v["sum_peaks"]<-k[1]+k[2]
    v["sd_dist"]<-sd(v)
    v<-c(v,raw_counts)
    v
}

fine_line_counts<-function(x){length(unique(x$FinelineNumber))}

#divide fine numbers and Upcs in separate bins
generate_fine_bins<-function(x){
    v<-discretize(x$FinelineNumber,numBins=1000,r=range(0,10001))
    names(v)<-paste("bins",c(1:1000),sep="_")
    #s<-sd(v)
    #v<-c(v,s)
    v<-v/length(x$FinelineNumber)
    v
}

generate_bins_upcs<-function(x)
{
  upcode<-x$Upc[which(str_length(x$Upc)>=4)]
  upc_temp<-strtoi(str_sub(upcode,start=1,end=4), base = 0L)
  #estimation 50 per bin
  v<-discretize(upc_temp,numBins=500,r=range(0,10000)) 
  names(v)<-paste("upc_bins",c(1:500),sep="_")
  v<-v/length(upc_temp)
  v
}

counts_small_upcs<-function(x){
    indexes_length<-length(which(str_length(x$Upc)<=4))
    indexes_length
}

#feature generating; for using 95,000 to generate new features would cause memory overflow,
#we separate into ten groups...
uniqueVisitNumbers<-sort(unique(train$VisitNumber))
splits<-split(uniqueVisitNumbers,ceiling(1:length(uniqueVisitNumbers)/10000))
str(splits)

#...and manipulate them dividedly
for (i in 1:length(splits))
{
  
checkdata<-ddply(train[which(train$VisitNumber %in% splits[[i]]),],c('TripType','VisitNumber'),
                 function(x) c(dayType=x$Weekday[1],
                               scanLength=length(x$ScanCount),
                               maxScan=max(x$ScanCount),
                               minScan=min(x$ScanCount),
                               scanSum=sum(abs(x$ScanCount)),
                               scanMean=mean(abs(x$ScanCount)),
                               #scan features
                               scan_positive_count=positive_count(x),
                               scan_negative_count=negative_count(x),
                               scan_positive_sum=positive_sum(x),
                               scan_negative_sum=negative_sum(x),
                               scan_positive_ratio=positive_ratio(x),
                               scan_negative_ratio=negative_ratio(x),
                               #department features
                               unique_departments_count=departments_count(x),
                               #department distribution
                               dept_dis=insertdeptframe(x), 
                               #FineLine features
                               unique_fine_line_counts=fine_line_counts(x),
                               #1000 bins for FineLineNumber 
                               bins=generate_fine_bins(x),
                               upc_bins=generate_bins_upcs(x),
                               small_upcs=counts_small_upcs(x)
                 )
)  
if (i == 1)
  checkdata1 <- checkdata
else if (i == 2)
  checkdata2 <- checkdata
else if (i == 3)
  checkdata3 <- checkdata
else if (i == 4)
  checkdata4 <- checkdata
else if (i == 5)
  checkdata5 <- checkdata
else if (i == 6)
  checkdata6 <- checkdata
else if (i == 7)
  checkdata7 <- checkdata
else if (i == 8)
  checkdata8 <- checkdata
else if (i == 9)
  checkdata9 <- checkdata
else if (i == 10)
  checkdata10 <- checkdata

print(paste(i*100/length(splits),"% done"))
}

checkdataf <- rbind(checkdata1, checkdata2, checkdata3, checkdata4, checkdata5, checkdata6, checkdata7, checkdata8, checkdata9, checkdata10)

#---classifying---#
uniqueVisitNumbers2<-sort(unique(test$VisitNumber))
splits2<-split(uniqueVisitNumbers2,ceiling(1:length(uniqueVisitNumbers2)/10000))
str(splits2)

for (i in 1:length(splits2))
{
  
  checkdatatest<-ddply(test[which(test$VisitNumber %in% splits2[[i]]),],c('VisitNumber'),
                       function(x) c(dayType=x$Weekday[1],
                                     scanLength=length(x$ScanCount),
                                     maxScan=max(x$ScanCount),
                                     minScan=min(x$ScanCount),
                                     scanSum=sum(abs(x$ScanCount)),
                                     scanMean=mean(abs(x$ScanCount)),
                                     #scan features
                                     scan_positive_count=positive_count(x),
                                     scan_negative_count=negative_count(x),
                                     scan_positive_sum=positive_sum(x),
                                     scan_negative_sum=negative_sum(x),
                                     scan_positive_ratio=positive_ratio(x),
                                     scan_negative_ratio=negative_ratio(x),
                                     #department features
                                     unique_departments_count=departments_count(x),
                                     #department distribution
                                     dept_dis=insertdeptframe(x), 
                                     #FineLine features
                                     unique_fine_line_counts=fine_line_counts(x),
                                     #1000 bins for FineLineNumber 
                                     bins=generate_fine_bins(x),
                                     upc_bins=generate_bins_upcs(x),
                                     small_upcs=counts_small_upcs(x)
                       )
  )  
  if (i == 1)
    checkdatatest1 <- checkdatatest
  else if (i == 2)
    checkdatatest2 <- checkdatatest
  else if (i == 3)
    checkdatatest3 <- checkdatatest
  else if (i == 4)
    checkdatatest4 <- checkdatatest
  else if (i == 5)
    checkdatatest5 <- checkdatatest
  else if (i == 6)
    checkdatatest6 <- checkdatatest
  else if (i == 7)
    checkdatatest7 <- checkdatatest
  else if (i == 8)
    checkdatatest8 <- checkdatatest
  else if (i == 9)
    checkdatatest9 <- checkdatatest
  else if (i == 10)
    checkdatatest10 <- checkdatatest
  
  print(paste(i*100/length(splits2),"% done"))
}
checkdatatestf <- rbind(checkdatatest1, checkdatatest2, checkdatatest3, checkdatatest4, checkdatatest5, checkdatatest6, checkdatatest7, checkdatatest8, checkdatatest9, checkdatatest10)

check4model <- checkdataf[sample(95674, 5000), ]
check4model1 <- check4model
check4model1$dayType <- factor(check4model$dayType)
check4model1$scanSum <- factor(check4model$scanSum)
check4model1$scan_positive_count <- factor(check4model$scan_positive_count)
check4model1$scan_negative_count <- factor(check4model$scan_negative_count)
check4model1$unique_departments_count <- factor(check4model$unique_departments_count)

model1 <- rpart(TripType ~ dayType + scanSum + scan_positive_count + scan_negative_count + unique_departments_count, data = check4model1, method = "class")
r1 <- predict(model1, c(check4model1$dayType, check4model1$scanSum, check4model1$scan_positive_count, check4model1$scan_negative_count, check4model1$unique_department_count))
confusionMatrix(check4model1$TripType, r1)
             
param <- list("objective" = "multi:softmax",
              "eval_metric" = "mlogloss",
              "num_class" = 38,
              "eta"=0.05,
              "max.depth"=8,
              "nthread" = -1,
              "min_child_weight"=1,
              "colsample_bytree"=0.9,
              "subsample"=0.9
)

y=check4model$TripType
sortedTripType<-sort(unique(y))
result<-c(1:length(y))
for (i in 1:length(y))
{
  result[i]<-which(sortedTripType==y[i]) 
}
result<-result-1            
             
checkmat <- checkdataf[ ,-c(1,2)]
for(i in 1:5000)
{
  if(checkmat$dayType[i] == "Monday")
    checkmat$dayType[i] <- '1'
  else if (checkmat$dayType[i] == "Tuesday")
    checkmat$dayType[i] <- '2'
  else if (checkmat$dayType[i] == "Wednesday")
    checkmat$dayType[i] <- '3'
  else if (checkmat$dayType[i] == "Thursday")
    checkmat$dayType[i] <- '4'
  else if (checkmat$dayType[i] == "Friday")
    checkmat$dayType[i] <- '5'
  else if (checkmat$dayType[i] == "Saturday")
    checkmat$dayType[i] <- '6'
  else if (checkmat$dayType[i] == "Sunday")
    checkmat$dayType[i] <- '7'
}

for (i in 1:1654)
{
  checkmat[ ,i] <- as.numeric(checkmat[ ,i])
}
checkmatrix <- as.matrix(checkmat)
Boostmodel <- xgboost (params = parameterlist, data = checkmatrix, label = result, nrounds = 500)
r2 <- predict(Boostmodel, checkmatrix)
confusionMatrix(r2, result)
