
DeltaRay_Integration<-function(x) {data_file<<-x
                             source("DR_integration.R")}

DeltaRay_Calibration<-function(x) {data_file<<-x
                              source("DR_calibration.R")}

Datalogger_Integration<-function(x) {data_file<<-x
                                     source("DL_integration.R")}

DeltaRay_Fluxes<-function(x) {data_file<<-x
                                     source("DR_fluxes.R")}