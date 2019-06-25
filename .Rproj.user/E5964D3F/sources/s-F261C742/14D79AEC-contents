# setup ----

library(shinydashboard)
library(shinyTime)
library(lubridate)
library(shinythemes)
library(shiny)

tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
)

# main ----

shiny::navbarPage(
  
  # Title page ----
  
  title = "Continuous Reassessment Method (CRM)",
  theme = shinytheme("paper"),
  
  # Chapter 1. CRM ----
  tabPanel(
    icon = icon("address-card"), 
    title = "Input data",
    tabName="main",
    
    column(width=2,
           box(title = "Information", status = "primary", width = NULL, solidHeader = TRUE,
               sliderInput('dose10', "Dose 10", value = 2, min = 0, max = 100, step = 1,width = NULL),
               sliderInput('dose90', "Dose 90", value = 40, min = 0, max = 100, step = 1,width = NULL),
               sliderInput('targetDLT', "Target DLT", value = 0.9, min = 0, max = 1, step = 0.05,width = NULL),
               numericInput('numbersims', "Number of simulations", 10, min = NA, max = NA, step = 1,width = NULL),
               numericInput('samplesize', "Sample size", 200, min = NA, max = NA, step = 5,width = NULL),
               numericInput('stopearlynumber', "Stop-early number", 20, min = NA, max = NA, step = 5,width = NULL),
               numericInput('startdose', "Starting dose", 40, min = NA, max = NA, step = 5,width = NULL))
    ),
    column(width=4,
           box(title = "ordinal continuation ratio model CRM", status = "primary", width = NULL, solidHeader = TRUE,
               plotOutput("continuationratiomodel",  height="500px"))),
    column(width=4,
           box(title = "Binary 2-parameter logistic model CRM", status = "primary", width = NULL, solidHeader = TRUE,
               plotOutput("binarycrm",  height="500px"))
    )
  ),
  tabPanel(
    icon = icon("line-chart"), 
    title = "Simulation",
    tabName="Simulation",
    img(src='CRM.png', align = "center")
  )
)

