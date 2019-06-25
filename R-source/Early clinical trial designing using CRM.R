#!/SYSTEM/R/3.3.3/bin/Rscript
.libPaths("./libs")

dir.create("result");


library(descr)
library(ordcrm)
library(MASS)
library(optparse)


option_list <- list (
    make_option(c("-i","--inp"), type='character', help="Input file path", default=NULL ,metavar="File_Path")
);

opt_parser <- OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);

inputfile = opt$inp;

## input param ##
input <- data.frame(t(read.table(inputfile, row.names = 1, sep = "=",
                                 comment.char = ";",
                                 strip.white = TRUE,
                                 stringsAsFactors = FALSE)), stringsAsFactors = FALSE);

input;
################################
###    	packages	     ###    
################################

#install.packages("checkmate")
#install.packages("MASS")
#install.packages("ordcrm")
#install.packages("descr")
#install.packages("dplyr")

#library(ggplot2)
#library(descr)
#library(ordcrm)
#library(MASS)
#library(dplyr)

################################
###    factor definition     ###    
################################

#dose10=2
#dose90=40
#targetDLT=0.9
#numbersims=100
#samplesize=200
stopearly=TRUE
#stopearlynumber=20
#startdose=40

dose10=input$dose10
dose90=input$dose90
targetDLT=input$targetDLT
numbersims=input$numbersims
samplesize=input$samplesize
stopearlynumber=input$stopearlynumber
startdose=input$startdose

d=pseudodata(design='CRM',dose10 = dose10, dose90 = dose90, targetDLT = targetDLT, stabilize = TRUE, discrete = FALSE)
attributes(d)

pseudotox=d$"Pseudodata Toxicities" 
pseudodose=d$"Pseudodata Doses"

discretedoses=seq(from=dose10, to=dose90, by=2)


dose10_2=4
dose90_2=20
targetDLT_2=0.9
numbersims_2=100
samplesize_2=200
stopearly_2=TRUE
stopearlynumber_2=50
startdose_2=20


e=pseudodata(design='CRM',dose10 = dose10_2, dose90 = dose90_2, targetDLT = targetDLT_2, stabilize = TRUE, discrete = FALSE)
attributes(e)

pseudotox_2=e$"Pseudodata Toxicities" 
pseudodose_2=e$"Pseudodata Doses"
discretedoses_2=seq(from=dose10_2, to=dose90_2, by=2)


################################
###  		 model	     ###    
################################

c=pseudodata(design='CR',dose10 = dose10, dose90 = dose90, targetDLT = targetDLT, stabilize = TRUE, discrete = FALSE)

attributes(c)
c$"Regression Model" 
crmodel1<-c(-0.049,-0.0883,0.6335,-0.0883,0.4588,-0.0883,3.2795,-0.0883) #CR Model 1


f=pseudodata(design='CR',dose10 = dose10_2, dose90 = dose90_2, targetDLT = targetDLT_2, stabilize = TRUE, discrete = FALSE)

attributes(f)
f$"Regression Model" 
crmodel2<-c(0.6151,-0.2106,0.6405,-0.2106,0.3892,-0.2106,3.3117,-0.2106) #CR Model 2


################################
###    crmsimulations        ###    
################################

##prior
a=crmsimulations(startdose = startdose, numbersims=numbersims, cohortsize = 3,
samplesize = samplesize, pseudoweights = 1, pseudotox = pseudotox ,
pseudodose = pseudodose , dosetox = crmodel1, truedosetoxmodeltype = 'CR',
design = 'CRM', targetDLT =targetDLT, discrete=TRUE,
discretedoses=discretedoses,
numberdltrule = NA, lowerlimitrule = NA, upperlimitrule = NA, dltrule = NA,
increaserule = NA, minimum = NA, maximum = NA, combine01 = FALSE, stopearly=stopearly, stopearlynumber=stopearlynumber)
a

attributes(a)

a$Dose
a$"Median Dose"
a$"Acutal MTD"
a$"Median Sample Size For All Trials"
a$"Patients per Cohort"
a$"25% Quantile Sample Size"
a$"Median Total Sample Size of Trials Not Stopped Early Due to Safety Concerns"



##true
b=crmsimulations(startdose = startdose_2, numbersims=numbersims_2, cohortsize = 1,
samplesize = samplesize_2, pseudoweights = 1, pseudotox = pseudotox_2,
pseudodose = pseudodose_2, dosetox = crmodel2, truedosetoxmodeltype = 'CR',
design = 'CRM', targetDLT =targetDLT_2, discrete=TRUE,
discretedoses=discretedoses_2,
numberdltrule = NA, lowerlimitrule = NA, upperlimitrule = NA, dltrule = NA,
increaserule = NA, minimum = NA, maximum = NA, combine01 = FALSE, stopearly=stopearly_2, stopearlynumber=stopearlynumber_2)
b

attributes(b)

b$Dose
b$"Median Dose"
b$"Acutal MTD"
b$"Median Sample Size For All Trials"
b$"Patients per Cohort"

png(filename="result/CRM.png",type="cairo")

################################
###    	plot	           ###
################################

#prior, true line graph
data2=data.frame(targetDLT=c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1), 
		M.D.PRI=c(36,30,24,20,18,16,12,8,2),
		MTD.PRI=c(42,33,27,23,19,16,12,8,2),
		M.D.TRUE=c(18,14,12,11,10,10,8,6,4),
		MTD.TRUE=c(21,17,15,13,11,10,8,6,4))
data2
class(data2)

plot(data2[,1],data2[,3], main="Dose By TargetDLT",type="b", pch=19,lty=5, xlab="targetDLT",ylab="Dose", ylim=c(0,45),xlim=c(0.1,1), col="lightblue",lwd=2 )
points(data2[,1],data2[,5], type="b", pch=18,lty=1, xlab="targetDLT",ylab="Dose", ylim=c(0,45),xlim=c(0.1,1), col="tomato", lwd=2)
legend(0.12,44,legend=c("prior curve MTD","true curve MTD"),col=c("lightblue","tomato"),lty=c(5,1),lwd=c(2,2), cex=0.8)


dev.off()
