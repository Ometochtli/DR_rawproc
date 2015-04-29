# # # # # # Place datalogger .csv and data_inputs file within a file called CO2_front_data within your working directory # # # # # # #

# # # load packages # # #
library(data.table)
library(stringr)

# # # Preamble: CHANGE SETTINGS HERE # # #
data_inputs<-scan(file=paste(data_file,"data_inputs.txt",sep='/'),what="list",sep="\n")
InputList<-as.list(unlist(strsplit(data_inputs[1],'[|]')))
# # # load .csv from the datalogger # # #
DataloggerData<-fread(input=paste(data_file,data_inputs[7],sep="/"),na.strings="NAN",skip=4)
# # # create sensible column names # # # 
ColumnNames<-c(unlist(strsplit(data_inputs[8],'|',fixed=TRUE)))
setnames(DataloggerData,ColumnNames)
class(DataloggerData$flow_chamber)<-"numeric"
class(DataloggerData$flow_bypass)<-"numeric"

# # # create a list of pre-processed files from the deltaray to load # # # 
CalibratedDRInputList<-as.list(paste('CalibratedDR',unlist(strsplit(data_inputs[1],'[|]')),sep="_"))
# # # read in a batch of processed DR .csv files # # # 
CalibratedDRDataList<-lapply(CalibratedDRInputList,FUN=function(x) fread(input=paste(data_file,x,sep='/')))

# # # create data logger subsets # # # 
DataloggerDataList<-lapply(CalibratedDRDataList,FUN=function(x) DataloggerData[strptime(DataloggerData$timestamp, "%Y-%m-%d %T")>min(strptime(x$sample_start,"%m/%d/%Y %r")) & strptime(DataloggerData$timestamp,"%Y-%m-%d %T")<max(strptime(x$sample_end,"%m/%d/%Y %r")),])
names(DataloggerDataList)<-as.list(paste('DataloggerData',unlist(strsplit(data_inputs[1],'[|]')),sep="_"))

# # # create and fill the flow integration group field in dataloggerdatalist
lapply(DataloggerDataList,FUN=function(x) x[,c('flow_integration_group'):=list('NA')])
for(j in 1:length(DataloggerDataList)) {for(i in 1:nrow(CalibratedDRDataList[[j]])) {DataloggerDataList[[j]]$flow_integration_group[strptime(DataloggerDataList[[j]]$timestamp, "%Y-%m-%d %T")>(strptime(CalibratedDRDataList[[j]]$min_cycle_timestamp[i],"%m/%d/%Y %r")-10) & strptime(DataloggerDataList[[j]]$timestamp, "%Y-%m-%d %T")<(strptime(CalibratedDRDataList[[j]]$max_cycle_timestamp[i],"%m/%d/%Y %r"))]<-CalibratedDRDataList[[j]]$info_integration_group[i]}}

# # # 

ColumnsToOperateOn<-c('flow_chamber','flow_bypass')
MeanDataList<-lapply(DataloggerDataList,FUN=function(y) y[,lapply(.SD,mean,na.rm=T),.SDcols=ColumnsToOperateOn,by=flow_integration_group])
lapply(MeanDataList,FUN=function(x) setnames(x,c('flow_integration_group','chamber_flow','bypass_flow')))  

SdDataList<-lapply(DataloggerDataList,FUN=function(y) y[,lapply(.SD,sd,na.rm=T),.SDcols=ColumnsToOperateOn,by=flow_integration_group])
lapply(SdDataList,FUN=function(x) setnames(x,c('sd_flow_integration_group','sd_chamber_flow','sd_bypass_flow')))  

NDataList<-lapply(DataloggerDataList,FUN=function(y) y[,lapply(.SD,FUN=function(x) sum(!is.na(x))),.SDcols=ColumnsToOperateOn,by=flow_integration_group])
lapply(NDataList,FUN=function(x) setnames(x,c('n_flow_integration_group','n_chamber_flow','n_bypass_flow')))  

# # # combined datalogger dats
CombinedDataList<-lapply(1:length(MeanDataList), FUN=function(i) cbind(MeanDataList[[i]],SdDataList[[i]],NDataList[[i]]))
OutputList<-lapply(InputList,FUN=function(x) paste('IntegratedDL',x,sep="_"))
names(CombinedDataList)<-OutputList
CombinedDataListSubset<-lapply(CombinedDataList, FUN=function(x) subset(x,flow_integration_group!='NA',select=c('flow_integration_group','chamber_flow','bypass_flow','sd_chamber_flow','sd_bypass_flow','n_chamber_flow','n_bypass_flow')))

# # # combined with DR # # #

CompleteDataList<-lapply(1:length(CombinedDataListSubset), FUN=function(i) cbind(CalibratedDRDataList[[i]],CombinedDataListSubset[[i]]))
OutputList<-lapply(InputList,FUN=function(x) paste('Complete',x,sep="_"))
names(CompleteDataList)<-OutputList

lapply(1:length(CompleteDataList),FUN=function(i) write.csv(CompleteDataList[[i]],file=paste(data_file,names(CompleteDataList[i]),sep="/"),row.names=FALSE))

                    
# # # Clear workspace # # # 
rm(list = setdiff(ls(), lsf.str()))

