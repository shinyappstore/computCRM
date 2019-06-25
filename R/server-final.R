library(descr)
library(ordcrm)
library(MASS)
library(optparse)



# server.R ----


# setup ----

# deployApp()

library(deSolve)
library(plyr)
library(grid)
library(compiler)
library(shinyTime)
library(lubridate)
library(TeachingDemos)
library(rmarkdown)
library(knitr)
library(DT)
library(rsconnect)
library(tidyverse)

default_dose_example_csv <- '"Date","Inf_st_Time","Inf_ed_Time","Dose"
"17.05.03","10:30","11:30","500"
"17.05.03","22:30","23:30","750"
"17.05.04","10:30","11:30","1000"
"17.05.04","22:30","23:30","1000"'

calculate_crcl <- function(age, weight, sex, scr){
  crcl <- ((140-age) * weight * ifelse(sex == 'Female', 0.85, 1)) / (72*scr)
  return(crcl)
}

# main ----

shiny::shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    coln=c("Date","Inf_st_Time","Inf_ed_Time","Dose" )
    dat1=c("17.05.03", "10:30", "11:30","500")
    dat2=c("17.05.03", "22:30", "23:30","500")
    dat3=c("17.05.04", "10:30", "11:30","1000")
    dat4=c("17.05.04", "22:30", "23:30","1000")
    
    suppl=data.frame (rbind(dat1,dat2,dat3,dat4))
    colnames(suppl)<-coln
    rownames(suppl)<-NULL
    suppl
  })
  
  dose.data <- reactive({
    inFile <- input$file1 
    if (is.null(inFile)) return(NULL) 
    a=read.csv(inFile$datapath, header=T, stringsAsFactors = T) 
    b=a[complete.cases(a), ]  
    b$paste=paste(b$Date,b$Time) 
    b
    #Time calculation code is copyrighted
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("dose_example", '.csv', sep='') },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = F)
    }
  )
  
  output$dosing_history_contents <- renderTable({
    if (is.null(input$file1)) return(read_csv(default_dose_example_csv))
    return(read_csv(input$file1$datapath))
  })
  
  # crcl: https://www.mdcalc.com/creatinine-clearance-cockcroft-gault-equation
  output$creatinine_clearance <- renderText({
    return(calculate_crcl(input$age, input$weight, input$sex, input$scr) %>% round(digits = 2))
  })
  
  output$output_table1_time_predicted_concentration <- renderTable({
    prt1 <- sim.data()
    prt2 <- prt1[complete.cases(prt1),]
    if (input$Observations=='1') {
      prtx_predicted_concentration <- prt2 %>% 
        slice(2) %>% 
        select(pointtime, 
               `observed conc.`=observedConc, 
               `predicted conc.`=predictedConc)
    } 
    if (input$Observations=='2') {
      prtx_predicted_concentration <- prt2 %>% 
        slice(2) %>% 
        select(pointtime1, 
               observedConc1, 
               predictedConc1)
    }
    return(prtx_predicted_concentration)
  })
  
  output$outputtable2 <- renderTable({
    prt1=sim.data()
    prt2=prt1[complete.cases(prt1),]
    if (input$Observations=='1')
    {
      prtx=prt2[2,c("CL","V1","V2")]
    }
    
    if (input$Observations=='2')
    {
      prtx=prt2[2,c("pointtime2","observedConc2","predictedConc2")]
    }
    prtx
  })
  
  output$outputtable3 <- renderTable({
    prt1=sim.data()
    prt2=prt1[complete.cases(prt1),]
    if(input$Observations=='1'){
    }
    if (input$Observations=='2')
    {
      prtx=prt2[2,c("CL","V1","V2")]
      prtx
    }
  })
})



# setup ----


## input param ##
# input <- data.frame(t(read.table(inputfile, row.names = 1, sep = "=",
#                                  comment.char = ";",
#                                  strip.white = TRUE,
#                                  stringsAsFactors = FALSE)), stringsAsFactors = FALSE);
# 
# input;

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
a=crmsimulations(startdose = startdose, 
                 numbersims=numbersims, 
                 cohortsize = 3,
                 samplesize = samplesize, 
                 pseudoweights = 1, 
                 pseudotox = pseudotox ,
                 pseudodose = pseudodose , 
                 dosetox = crmodel1, 
                 truedosetoxmodeltype = 'CR',
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
















