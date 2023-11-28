library(HyetosMinute)

BLpar=list(lambda=0.431535846,phi=0.0673,kappa=0.01
,alpha=66.034,v=1.6221,mx=99,sxmx=99)


dir_path <- "C:\\Users\\aashish\\Desktop\\BLRP_READY"

files <- list.files(path = dir_path, pattern = "\\.txt")


files


i = 2063


for (filey in files) {

ex21 <- DisagSimul(TimeScale=1,BLpar=BLpar,CellIntensityProp=list(Weibull=TRUE, iota=NA),
                   RepetOpt=list(DistAllowed=0.1,FacLevel1Rep=20,MinLevel1Rep=50,
                   TotalRepAllowed=5000),NumOfSequences=25,Statistics=list(print=TRUE,plot=FALSE),
                   ExportSynthData=list(exp=TRUE,FileContent=c("AllDays"),
                   file= "C:\\Users\\aamir\\Desktop\\BLRP_OUT\\" + i + ".txt"),
                   ImportHistData=list(file=filey,
                   na.values="NA",FileContent=c("AllDays"), 
                   DaysPerSeason=30),PlotHyetographs=FALSE,RandSeed=5)

i = i + 1


}
