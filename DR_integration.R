# # # # # # Place data and data_inputs file within your working directory # # # # # # #



# # # load packages # # #
library(data.table)
library(stringr)

# # # Preamble: CHANGE SETTINGS HERE # # #
data_inputs<-scan(file=paste(data_file,"data_inputs.txt",sep='/'),what="list",sep="\n")

# # # create a list of .csv files from the DeltaRay to load # # #
InputList<-as.list(unlist(strsplit(data_inputs[1],'[|]')))
# # # create sensible column names # # # 
ColumnNames<-c(unlist(strsplit(data_inputs[2],'|',fixed=TRUE)))
# # # set flush and integration periods
flush_period<-as.numeric(data_inputs[3])
integration_period<-as.numeric(data_inputs[4])
# # # create treatment names # # # 
treatment_experiment<-c(unlist(strsplit(data_inputs[9],'|',fixed=TRUE)))
treatment<-str_split_fixed(treatment_experiment," & ",2)[,1]
experiment<-str_split_fixed(treatment_experiment,"& ",2)[,2]

# # # Load Data # # #
# # # read in the batch of .csv files specified as 'inputlist' # # # 
DataList<-lapply(InputList,FUN=function(x) fread(input=paste(data_file,x,sep='/')))

# # # Rename data.tables with InputList # # # 
names(DataList)<-InputList

# # # Rename the data tables in DataList with ColumnNames # # #
lapply(DataList,FUN=function(x) setnames(x,ColumnNames))

# # # Process data # # # 
# # # Drop pointless columns # # # 
DataListSubset<-lapply(DataList,FUN=function(x) subset(x,select=c("labbook","sample_number","sample_start","sample_end","cycle_number","cycle_timestamp","sample_label","flush_time","measurement_time","port","multiport","set_conc","sample_type","C13_ppm_raw","C12_ppm_raw",'O18_ppm_raw')))

# # #  Remove the flush period from each sample # # # 
NoFlushDataList<-lapply(DataListSubset,FUN=function(x) x[cycle_number>flush_period])

# # # Create variable to group measurements by integration_period # # # THIS BIT MAY CAUSE BLINDNESS - sorry # # # https://www.youtube.com/watch?v=ypdgAyzHx5Y

IntGroups<-lapply(NoFlushDataList, FUN=function(x) x[,lapply(.SD,FUN=function(x) paste(levels(factor(x)),rep(c(1:(length(x)/integration_period)),each=integration_period),sep="_")),.SDcols=c('sample_number'),by=sample_number])
lapply(IntGroups,FUN=function(x) setnames(x,c('sample_number','integration_group')))
IntGroupDataList<-lapply(1:length(NoFlushDataList),FUN=function(i) cbind(IntGroups[[i]],NoFlushDataList[[i]]))

# # # Calculate mean, sd and n for each sample # # #
ColumnsToStripFrom<-c('sample_number','labbook','sample_start','sample_end','sample_label','port','multiport','set_conc')
ColumnsToOperateOn<-c('C13_ppm_raw','C12_ppm_raw','O18_ppm_raw')
ColumnsTime<-c('cycle_timestamp')

InfoDataList<-lapply(IntGroupDataList,FUN=function(y) y[,lapply(.SD,FUN=function(x) levels(factor(x))),.SDcols=ColumnsToStripFrom,by=integration_group])
lapply(InfoDataList,FUN=function(x) setnames(x,c('info_integration_group',ColumnsToStripFrom)))

MeanDataList<-lapply(IntGroupDataList,FUN=function(y) y[,lapply(.SD,mean,na.rm=T),.SDcols=ColumnsToOperateOn,by=integration_group])
lapply(MeanDataList,FUN=function(x) setnames(x,c('mean_integration_group',ColumnsToOperateOn)))

SdDataList<-lapply(IntGroupDataList,FUN=function(y) y[,lapply(.SD,sd,na.rm=T),.SDcols=ColumnsToOperateOn,by=integration_group])
lapply(SdDataList,FUN=function(x) setnames(x,c('sd_integration_group',paste('sd',ColumnsToOperateOn,sep="_"))))

NDataList<-lapply(IntGroupDataList,FUN=function(y) y[,lapply(.SD,FUN=function(x) sum(!is.na(x))),.SDcols=ColumnsToOperateOn,by=integration_group])
lapply(NDataList,FUN=function(x) setnames(x,c('n_integration_group',paste('n',ColumnsToOperateOn,sep="_"))))


MinTimeDataList<-lapply(IntGroupDataList,FUN=function(y) y[,lapply(.SD,FUN=function(x) min(x)),.SDcols=ColumnsTime,by=integration_group])
lapply(MinTimeDataList,FUN=function(x) setnames(x,c('min_integration_group','min_cycle_timestamp'))) 

MaxTimeDataList<-lapply(IntGroupDataList,FUN=function(y) y[,lapply(.SD,FUN=function(x) max(x)),.SDcols=ColumnsTime,by=integration_group])
lapply(MaxTimeDataList,FUN=function(x) setnames(x,c('max_integration_group','max_cycle_timestamp')))  

 
                         
CombinedDataList<-lapply(1:length(InfoDataList), FUN=function(i) cbind(treatment=rep(treatment[i],dim(InfoDataList[[i]])[1]),experiment=rep(experiment[i],dim(InfoDataList[[i]])[1]),InfoDataList[[i]],MinTimeDataList[[i]],MaxTimeDataList[[i]],MeanDataList[[i]],SdDataList[[i]],NDataList[[i]]))
OutputList<-lapply(InputList,FUN=function(x) paste('IntegratedDR',x,sep="_"))
names(CombinedDataList)<-OutputList

lapply(1:length(CombinedDataList),FUN=function(i) write.csv(CombinedDataList[[i]],file=paste(data_file,names(CombinedDataList[i]),sep="/"),row.names=FALSE))

# # # Clear workspace # # # 
rm(list = setdiff(ls(), lsf.str()))


       
