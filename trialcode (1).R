data<-read.csv(file.choose(), header = F,fileEncoding= "UTF-8-BOM")
data
summary(data)
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss)
apply(data,1,pMiss)
library(mice)
md.pattern(data)
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(data,m=5,maxit=5,meth='mean')
summary(tempData)
tempData$imp$V6
tempData$meth
completedData1 <- complete(tempData,1)
completedData2 <- complete(tempData,2)
completedData3 <- complete(tempData,3)
completedData4 <- complete(tempData,4)
completedData5 <- complete(tempData,5)
write.table(completedData1, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/BCW/PID_AE_20/PID_AE_20a.csv",sep=",",row.names=FALSE,col.names=FALSE)
write.table(completedData2, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/BCW/PID_AE_20/PID_AE_20b.csv",sep="," ,row.names=FALSE,col.names=FALSE)
write.table(completedData3, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/BCW/PID_AE_20/PID_AE_20c.csv",sep="," ,row.names=FALSE,col.names=FALSE)
write.table(completedData4, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/PID/PID_AE_20/PID_AE_20d.csv",sep="," , row.names=FALSE,col.names=FALSE)
write.table(completedData5, "C:/Users/vasavi/Desktop/Datasets for Course Projects/predicteddatasets/PID/PID_AE_20/PID_AE_20e.csv",sep="," , row.names=FALSE,col.names=FALSE)
original<-read.csv("C:/Users/vasavi/Desktop/Datasets for Course Projects/original datasets.csv/BCW.csv",header=F)
a1 <- sqrt(sum((completedData1-original)^2))
a2 <- sqrt(sum((completedData2-original)^2))
a3 <- sqrt(sum((completedData3-original)^2))
a4 <- sqrt(sum((completedData4-original)^2))
a5 <- sqrt(sum((completedData5-original)^2))
b <- sqrt(sum(original^2))
NRMS1 <- a1/b
NRMS2 <- a2/b
NRMS3 <- a3/b
NRMS4 <- a4/b
NRMS5 <- a5/b
NRMS1
NRMS2
NRMS3
NRMS4
NRMS5

 

