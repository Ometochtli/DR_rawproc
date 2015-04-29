# # # load packages # # #
library(data.table)
library(stringr)
library(reshape2)

# # # Preamble: CHANGE SETTINGS HERE # # #
data_inputs<-scan(file=paste(data_file,"data_inputs.txt",sep='/'),what="list",sep="\n")
InputList<-as.list(unlist(strsplit(data_inputs[1],'[|]')))

# # # v_mol # # # 25 C
v_mol<-((8.31441*(25+273.15))/101325)
mlm2m3s<-60*1e6
A<-as.numeric(c(unlist(strsplit(data_inputs[10],'|',fixed=TRUE))))

# # # create a list of pre-processed files from the deltaray to load # # # 
CompleteDataInputList<-as.list(paste('Complete',unlist(strsplit(data_inputs[1],'[|]')),sep="_"))
# # # read in a batch of processed DR .csv files # # # 
CompleteDataList<-lapply(CompleteDataInputList,FUN=function(x) fread(input=paste(data_file,x,sep='/')))

# # # subset to chambers and add pairs and weights # # # 
SubsetCompleteDataList<-lapply(CompleteDataList,FUN=function(x) subset(x,sample_label=='bypass'| sample_label=='chamber',select=c("treatment","experiment","info_integration_group","sample_number","labbook","sample_start","sample_end","sample_label","port","multiport","min_cycle_timestamp","max_cycle_timestamp","delta_C13_VPDB_corSM","delta_O18_VPDBCO2_corSM","CO2_ppm_corSM","sd_delta_C13_VPDB_corSM","sd_delta_O18_VPDBCO2_corSM","sd_CO2_ppm_corSM","chamber_flow","bypass_flow","sd_chamber_flow","sd_bypass_flow")))
lapply(SubsetCompleteDataList,FUN=function(x) x[,c('flux_pair'):=list(rep(c(1:(nrow(x)/2)),each=2))])

# # # melt me and add weight # # #
WideSubsetCompleteDataList<-lapply(SubsetCompleteDataList,FUN=function(x) reshape(x,idvar=c('flux_pair'),timevar='sample_label',direction='wide'))
AddWideSubsetCompleteDataList<-lapply(1:length(WideSubsetCompleteDataList), FUN=function(i) cbind(WideSubsetCompleteDataList[[i]],A=rep(A[i],nrow(WideSubsetCompleteDataList[[i]]))))


# # # calcualte open fluxes # # # 
lapply(AddWideSubsetCompleteDataList,FUN=function(x) x[,c('F_CO2','F_C13','F_O18'):=list((((x$chamber_flow.chamber/mlm2m3s/v_mol)*(x$CO2_ppm_corSM.chamber-x$CO2_ppm_corSM.bypass))/x$A),((x$delta_C13_VPDB_corSM.chamber*x$CO2_ppm_corSM.chamber-delta_C13_VPDB_corSM.bypass*x$CO2_ppm_corSM.bypass)/(x$CO2_ppm_corSM.chamber-x$CO2_ppm_corSM.bypass)),((x$delta_O18_VPDBCO2_corSM.chamber*x$CO2_ppm_corSM.chamber-delta_O18_VPDBCO2_corSM.bypass*x$CO2_ppm_corSM.bypass)/(x$CO2_ppm_corSM.chamber-x$CO2_ppm_corSM.bypass)))])


# # # names and write
OutputList<-lapply(InputList,FUN=function(x) paste('Fluxes',x,sep="_"))
names(AddWideSubsetCompleteDataList)<-OutputList

lapply(1:length(AddWideSubsetCompleteDataList),FUN=function(i) write.csv(AddWideSubsetCompleteDataList[[i]],file=paste(data_file,names(AddWideSubsetCompleteDataList[i]),sep="/"),row.names=FALSE))

# # # Clear workspace # # # 
rm(list = setdiff(ls(), lsf.str()))
