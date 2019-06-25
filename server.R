library(descr)
library(ordcrm)
library(MASS)
library(optparse)

pipet_pseudodata_cr <- function(dose10, dose90, targetDLT) {
  pseudodata(design='CR',dose10 = dose10, dose90 = dose90, targetDLT = targetDLT, stabilize = TRUE, discrete = FALSE)
}

pipet_pseudodata_crm <- function(dose10, dose90, targetDLT) {
  pseudodata(design='CRM',
               dose10 = dose10, 
               dose90 = dose90, 
               targetDLT = targetDLT, 
               stabilize = TRUE, discrete = FALSE)
}

shiny::shinyServer(function(input, output) {
  
  output$continuationratiomodel <- renderPlot({
    pipet_pseudodata_cr(dose10 = input$dose10, 
                        dose90 = input$dose90, 
                        targetDLT = input$targetDLT)
  })
  output$binarycrm <- renderPlot({
    pipet_pseudodata_crm(dose10 = input$dose10, 
                        dose90 = input$dose90, 
                        targetDLT = input$targetDLT)
  })
  
  output$crmresults <- renderText({
    
    c <- pipet_pseudodata_cr(dose10 = input$dose10, 
                             dose90 = input$dose90, 
                             targetDLT = input$targetDLT)
    
    d <- pipet_pseudodata_crm(dose10 = input$dose10, 
                              dose90 = input$dose90, 
                              targetDLT = input$targetDLT)
    
    pseudotox <- d$"Pseudodata Toxicities" 
    pseudodose <- d$"Pseudodata Doses"
    discretedoses <- seq(from=input$dose10, to=input$dose90, by=2)
    
    ccoeff <- c$`Regression Model`$coefficients
    crmodel1<-c(ccoeff[1],ccoeff[5],
                ccoeff[2],ccoeff[5],
                ccoeff[3],ccoeff[5],
                ccoeff[4],ccoeff[5]) #CR Model 1
    
    # a <- crmsimulations(startdose = input$startdose, 
    #                numbersims=input$numbersims, 
    #                cohortsize = 3,
    #                samplesize = input$samplesize, 
    #                pseudoweights = 1, 
    #                pseudotox = pseudotox, 
    #                pseudodose = pseudodose, 
    #                dosetox = crmodel1, #####
    #                truedosetoxmodeltype = 'CR',
    #                design = 'CRM', 
    #                targetDLT = input$targetDLT, 
    #                discrete=TRUE,
    #                discretedoses=discretedoses,
    #                numberdltrule = NA, 
    #                lowerlimitrule = NA, 
    #                upperlimitrule = NA, 
    #                dltrule = NA,
    #                increaserule = NA, minimum = NA, maximum = NA, combine01 = FALSE, 
    #                stopearly=TRUE, 
    #                stopearlynumber=input$stopearlynumber)#
    # a$Dose
  })
})

