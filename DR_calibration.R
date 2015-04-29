# # # # # # Place processed deltaray and data_inputs file within a file called CO2_front_data within your working directory # # # # # # #


# # # load packages # # #
library(data.table)
library(stringr)
library(nlme)

# # # Preamble: CHANGE SETTINGS HERE # # #
data_inputs<-scan(file=paste(data_file,"data_inputs.txt",sep='/'),what="list",sep="\n")

# # # create a list of pre-processed files from the deltaray to load # # # 
IntegratedDRInputList<-as.list(paste('IntegratedDR',unlist(strsplit(data_inputs[1],'[|]')),sep="_"))
# # # read in a batch of processed DR .csv files # # # 
DRDataList<-lapply(IntegratedDRInputList,FUN=function(x) fread(input=paste(data_file,x,sep='/')))

# # # Interpolating low and high calibration cylinder values # # #
LowConCyl<-as.numeric(c(unlist(strsplit(data_inputs[5],'|',fixed=TRUE))))
HighConCyl<-as.numeric(c(unlist(strsplit(data_inputs[6],'|',fixed=TRUE))))

SplineLowC12List<-lapply(DRDataList,FUN=function(x) spline(strptime(x$min_cycle_timestamp[x$port=='PortConRef1'],"%m/%d/%Y %r"),x$C12_ppm_raw[x$port=='PortConRef1'],xout=strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")))
SplineLowC13List<-lapply(DRDataList,FUN=function(x) spline(strptime(x$min_cycle_timestamp[x$port=='PortConRef1'],"%m/%d/%Y %r"),x$C13_ppm_raw[x$port=='PortConRef1'],xout=strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")))
SplineLowO18List<-lapply(DRDataList,FUN=function(x) spline(strptime(x$min_cycle_timestamp[x$port=='PortConRef1'],"%m/%d/%Y %r"),x$O18_ppm_raw[x$port=='PortConRef1'],xout=strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")))

SplinesLowList<-lapply(1:length(SplineLowC12List),FUN=function(i) cbind(rbindlist(SplineLowC12List[i]),rbindlist(SplineLowC13List[i]),rbindlist(SplineLowO18List[i])))
lapply(SplinesLowList, FUN=function(x) setnames(x,c('InterpolatedTime','C12_ppm_int','InterpolatedTime1','C13_ppm_int','InterpolatedTime2','O18_ppm_int')))
lapply(SplinesLowList,FUN=function(x) x[,c('C12_ppm_cyl','sd_C12_ppm_cyl','C13_ppm_cyl','sd_C13_ppm_cyl','O18_ppm_cyl','sd_O18_ppm_cyl'):=list(LowConCyl[1],LowConCyl[2],LowConCyl[3],LowConCyl[4],LowConCyl[5],LowConCyl[6])])

SplineHighC12List<-lapply(DRDataList,FUN=function(x) spline(strptime(x$min_cycle_timestamp[x$port=='PortConRef2'],"%m/%d/%Y %r"),x$C12_ppm_raw[x$port=='PortConRef2'],xout=strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")))
SplineHighC13List<-lapply(DRDataList,FUN=function(x) spline(strptime(x$min_cycle_timestamp[x$port=='PortConRef2'],"%m/%d/%Y %r"),x$C13_ppm_raw[x$port=='PortConRef2'],xout=strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")))
SplineHighO18List<-lapply(DRDataList,FUN=function(x) spline(strptime(x$min_cycle_timestamp[x$port=='PortConRef2'],"%m/%d/%Y %r"),x$O18_ppm_raw[x$port=='PortConRef2'],xout=strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")))

SplinesHighList<-lapply(1:length(SplineHighC12List),FUN=function(i) cbind(rbindlist(SplineHighC12List[i]),rbindlist(SplineHighC13List[i]),rbindlist(SplineHighO18List[i])))
lapply(SplinesHighList,FUN=function(x) setnames(x,c('InterpolatedTime','C12_ppm_int','InterpolatedTime1','C13_ppm_int','InterpolatedTime2','O18_ppm_int')))
lapply(SplinesHighList,FUN=function(x) x[,c('C12_ppm_cyl','sd_C12_ppm_cyl','C13_ppm_cyl','sd_C13_ppm_cyl','O18_ppm_cyl','sd_O18_ppm_cyl'):=list(HighConCyl[1],HighConCyl[2],HighConCyl[3],HighConCyl[4],HighConCyl[5],HighConCyl[6])])

SplinesList_SM<-lapply(1:length(SplinesLowList), FUN=function(i) rbind(subset(SplinesLowList[[i]],select=c('InterpolatedTime','C12_ppm_int','C13_ppm_int','O18_ppm_int','C12_ppm_cyl','sd_C12_ppm_cyl','C13_ppm_cyl','sd_C13_ppm_cyl','O18_ppm_cyl','sd_O18_ppm_cyl')),subset(SplinesHighList[[i]],select=c('InterpolatedTime','C12_ppm_int','C13_ppm_int','O18_ppm_int','C12_ppm_cyl','sd_C12_ppm_cyl','C13_ppm_cyl','sd_C13_ppm_cyl','O18_ppm_cyl','sd_O18_ppm_cyl'))))

SplinesList_WM<-lapply(1:length(SplinesLowList), FUN=function(i) cbind(subset(SplinesLowList[[i]],select=c('InterpolatedTime','C12_ppm_int','C13_ppm_int','O18_ppm_int','C12_ppm_cyl','sd_C12_ppm_cyl','C13_ppm_cyl','sd_C13_ppm_cyl','O18_ppm_cyl','sd_O18_ppm_cyl')),subset(SplinesHighList[[i]],select=c('InterpolatedTime','C12_ppm_int','C13_ppm_int','O18_ppm_int','C12_ppm_cyl','sd_C12_ppm_cyl','C13_ppm_cyl','sd_C13_ppm_cyl','O18_ppm_cyl','sd_O18_ppm_cyl'))))
lapply(SplinesList_WM, FUN=function(x) setnames(x,c('InterpolatedTimeLow','C12_ppm_intLow','C13_ppm_intLow','O18_ppm_intLow','C12_ppm_cylLow','sd_C12_ppm_cylLow','C13_ppm_cylLow','sd_C13_ppm_cylLow','O18_ppm_cylLow','sd_O18_ppm_cylLow','InterpolatedTimeHigh','C12_ppm_intHigh','C13_ppm_intHigh','O18_ppm_intHigh','C12_ppm_cylHigh','sd_C12_ppm_cylHigh','C13_ppm_cylHigh','sd_C13_ppm_cylHigh','O18_ppm_cylHigh','sd_O18_ppm_cylHigh')))

# # # Regressions for interpolated cal cylidners (SM)# # # 

RegressionC12List<-lapply(SplinesList_SM,FUN=function(x) lmList(C12_ppm_cyl~C12_ppm_int|InterpolatedTime,data=x,pool=FALSE))
RegressionC13List<-lapply(SplinesList_SM,FUN=function(x) lmList(C13_ppm_cyl~C13_ppm_int|InterpolatedTime,data=x,pool=FALSE))
RegressionO18List<-lapply(SplinesList_SM,FUN=function(x) lmList(O18_ppm_cyl~O18_ppm_int|InterpolatedTime,data=x,pool=FALSE))

RegressionCoefsList<-lapply(1:length(RegressionC12List), FUN=function(i) cbind(names(RegressionC12List[[i]]),coef(RegressionC12List[[i]]),coef(RegressionC13List[[i]]),coef(RegressionO18List[[i]])))
lapply(RegressionCoefsList, FUN=function(x) setnames(x,c('InterpolatedTime','c_C12_ppm','m_C12','c_C13_ppm','m_C13','c_O18_ppm','m_O18')))

# # # Combining info and values # # #
CoefDRDataList<-lapply(1:length(DRDataList), FUN=function(i) cbind(DRDataList[[i]],SplinesList_WM[[i]],RegressionCoefsList[[i]]))

# # # SM - corrected # # #  
lapply(CoefDRDataList,FUN=function(x) x[,c('C12_ppm_corSM','C13_ppm_corSM','O18_ppm_corSM'):=list((x$C12_ppm_raw*x$m_C12+x$c_C12_ppm),(x$C13_ppm_raw*x$m_C13+x$c_C13_ppm),(x$O18_ppm_raw*x$m_O18+x$c_O18_ppm))])
lapply(CoefDRDataList,FUN=function(x) x[,c('delta_C13_VPDB_corSM','delta_O18_VPDBCO2_corSM','CO2_ppm_corSM'):=list(1000*(((x$C13_ppm_corSM/(x$C12_ppm_corSM))/0.0112372)-1),1000*((((x$O18_ppm_corSM*0.5)/(x$C12_ppm_corSM))/0.00208835)-1),(x$C13_ppm_corSM+x$C12_ppm_corSM+x$O18_ppm_corSM))])
lapply(CoefDRDataList,FUN=function(x) x[,c('sd_delta_C13_VPDB_corSM','sd_delta_O18_VPDBCO2_corSM','sd_CO2_ppm_corSM'):=list(((x$sd_C13_ppm_raw+x$sd_C12_ppm_raw*x$C13_ppm_corSM/x$C12_ppm_corSM)/x$C12_ppm_corSM/0.0112372*1000),(x$sd_O18_ppm_raw+x$sd_C12_ppm_raw*x$O18_ppm_corSM/x$C12_ppm_corSM/2/x$C12_ppm_corSM/0.00208835*1000),((x$sd_C13_ppm_raw^2+x$sd_C12_ppm_raw^2+x$sd_O18_ppm_raw^2)^0.5))])


# WM - corrected 


# # file writing # # 
InputList<-as.list(unlist(strsplit(data_inputs[1],'[|]')))
OutputList<-lapply(InputList,FUN=function(x) paste('CalibratedDR',x,sep="_"))
names(CoefDRDataList)<-OutputList

lapply(1:length(CoefDRDataList),FUN=function(i) write.csv(CoefDRDataList[[i]],file=paste(data_file,names(CoefDRDataList[i]),sep="/"),row.names=FALSE))

#RbindCoefDRDataList<-rbindlist(CoefDRDataList)
#write.csv(RbindCoefDRDataList,file=paste(data_file,'CalibratedDR_RbindAll.csv',sep="/"),row.names=FALSE)

# # # Calibration plots # # # 

library(lattice)
# calibration plots
lapply(CoefDRDataList,FUN=function(x) xyplot((C12_ppm_corSM+C12_ppm_intHigh)+C13_ppm_corSM+O18_ppm_corSM~InterpolatedTimeHigh|sample_label,data=x,subset=port=='PortConRef1'|port=='PortConRef2',outer=TRUE,scales='free'))


# run plots
lapply(CoefDRDataList,FUN=function(x) xyplot(C12_ppm_corSM+C13_ppm_corSM+O18_ppm_corSM~InterpolatedTimeHigh,data=x,outer=TRUE,group=sample_label,scales='free',auto.key=list(x=.6,y=.7,corner=c(0,0))))
lapply(CoefDRDataList,FUN=function(x) xyplot(CO2_ppm_corSM+delta_C13_VPDB_corSM+delta_O18_VPDBCO2_corSM~InterpolatedTimeHigh,data=x,outer=TRUE,group=sample_label,scales='free',auto.key=list(x=.6,y=.7,corner=c(0,0))))



lapply(CoefDRDataList,xyplot(CO2_ppm_corSM~InterpolatedTimeHigh,data=x,group=sample_label)
plot(strptime(CoefDRDataList[[1]]$min_cycle_timestamp,"%m/%d/%Y %r"),CoefDRDataList[[1]]$CO2_ppm_corSM)
strptime(x$min_cycle_timestamp,"%m/%d/%Y %r")
# # # clear work space # # #
rm(list = setdiff(ls(), lsf.str()))


