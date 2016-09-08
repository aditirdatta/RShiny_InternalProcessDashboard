# library(shiny)
# library(plyr)
# library(dplyr)
# library(rJava)
# library(shiny)
# library(shinydashboard)
# library(ggplot2)
# library(XLConnect)
# library(readxl)
# library(xlsx)
# library(png)
# library(leaflet)
# library(htmlwidgets)
# library(htmltools)
# library(DT)
# library(rpivotTable)
# library(knitr)
# library(plotrix)
# library(plotly)
# library(dygraphs)
# library(formattable)
# library(data.table)
# library(ggplot2)

# Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
# Datafeed <- read.xlsx2("Datasets/GT22016_DataFeedProfile_10July2016_V2.xlsx",
#                        sheetIndex = 2, startRow = 2, stringsAsFactors = F, header = T)
# Implementation <- read.xlsx2("Datasets/The Network Participant Onboarding Status - July.xlsm",
#                              sheetIndex = 1, startRow = 7, stringsAsFactors = F, header = T)
# Workflow <- read.xlsx2("Datasets/Assessment Tracker.xlsx",
#                        sheetIndex = 1, startRow = 1, stringsAsFactors = F, header = T, colClasses = c(rep("character", 5), rep("Date",7)))
# GeoRecruitment <- read.xlsx2("Datasets/Updated Facility Crosswalk 2016-7-28.xlsx",
#                           sheetIndex = 1, startRow = 1, stringsAsFactors = F, header = T, colClasses = c(rep("character",27), rep("numeric",2)))
# city <- as.character(GeoRecruitment$City)
# cityU <- unique(city)
# impl.colnames <- names(Implementation)
# 
# df <- data.frame(lon = GeoRecruitment$Longitude[!is.na(GeoRecruitment$Longitude)], lat = GeoRecruitment$Latitude[!is.na(GeoRecruitment$Latitude)], label = GeoRecruitment$Participant[!is.na(GeoRecruitment$Latitude)])

# setwd("U:/ads/Rscripts")

# runApp(host = "0.0.0.0", port = 3797)

dashboardPage(
  dashboardHeader(title = " AzHeC Dashboard"),
#--------------------- Dashboard Sidebar removed --------------------
   dashboardSidebar(width = "0px"
  # #   sidebarMenu(h4("Enter User Information"), h5(textInput("Name","Enter Name","")),
  # #                h5(textInput("Department","Enter Department","")),
  # #                radioButtons("User", "Enter User Type",list("Administative","General"),""),
  # #                column(8, submitButton("Submit"), p("Submit Information"), align = "center"),
  # 
  #   ,fluidRow(
  #     column(12, selectInput("City", "Select Region", c("All", cityU), selected = "All", multiple = TRUE)
  #            #,selectInput("dataset","Select Process", choices = c("Recruitment","Workflow", "Implementation", "Datafeed", "GeoRecruitment"),"")
  #            )
  # 
  #     # column(6, selectInput("channel", "Select Channel", choices = c("ADT"= 12,"LAB"= 48,"TRAN"= 72,"Rad"= 51),  selected = "ADT", multiple = T),
  #     #        selectInput("group","Select Participant", choices = c("Health System","Behavioral Health",  "Community Providers"),""))
  #    # column(12, sliderInput("slider", "Select Number of Channels", min = 0, max = 8, value = 3, animate = T, step = 1)),
  #     #column(8, submitButton("Submit"), p("Submit Selection"), align = "center" ),
  #     #column(8, radioButtons("color", "Select Color", choices = c("Cyan", "Beige", "Light Green")))
  # 
  #   )
  ),

    dashboardBody(width = "100%", height = "100%",
      #h3("Geographic Map of Particpants", align = "center"), 
             
              tabBox(width = 12,
                          
                          tabPanel("Data Process Map", 
                                   infoBox("Recruitment Hand-off to Workflow", paste(handoffCnt, '/',handoffCntP), icon = icon("users")),
                                   infoBox("Workflow Complete", paste(draftcomCnt, '/',draftcomCntP), icon = icon("info"), col = "yellow"),
                                   infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("thumbs-up", lib = "glyphicon"), col = "green"),
                                   fluidRow(
                                     
                                       column(12,tags$iframe(style = 'height: 650px; width:100%; scrolling = no', src = "ProcessMap1.jpg"), align = "center")
                                       ,HTML('<style>.rChart {width: 100%; height: 500px}</style>')
                                     
                                   )
                                   #tags$iframe(style = 'height: 600px; width:1000px; scrolling = yes',src = "ProcessMap.png")
                                   
                                   ),
                          tabPanel("DataSet Selection", 
                                   selectInput("dataset","Select Process", choices = c("Recruitment","Workflow", "Implementation", "Datafeed", "GeoRecruitment"),""),
                                   dataTableOutput("myData")),
                          
                     tabPanel("Summary Highlights", 
                              infoBox("Recruited Participants", paste(participantCnt), icon = icon("globe", lib = "glyphicon"), color = "green"),
                              infoBox("Workflow Completed", paste(OrgdraftcomCnt,'===>',draftcomCnt), icon = icon("industry"), color = "yellow"),
                              infoBox("Implemented Participants", paste(OrgimpOpertionalCnt,'===>',impOperationalCnt), icon = icon("cloud", lib = "glyphicon"), color = "blue"),
                              box(
                                amChartsOutput("mySummary1"), title = "Process Implementation Segmentation", background = "teal",collapsible = T,  
                                status = "primary", solidHeader = T
                                
                              ),
                              
                              box( amChartsOutput("mySummary"), title = "Inbound Datafeed Channel Segmentation", background = "teal",collapsible = T,  status = "primary", solidHeader = T
                              )
                              
                     ),
                     
                     tabPanel("Recruitment Summary Dashboard", 
                              infoBox("Recruitment Hand-off to Workflow", paste(handoffCnt, '/',handoffCntP), icon = icon("globe"), col = "green"),
                              infoBox("Workflow Complete", paste(draftcomCnt, '/',draftcomCntP), icon = icon("industry"), col = "yellow"),
                              infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("thumbs-up", lib = "glyphicon"), col = "blue"),
                              
                     box(bold = T,  title = "EMR Classification",
                                  background = "yellow", solidHeader = T, status = "primary", collapsible = T,amChartsOutput("myWorkflow3")),
                    
                      box(plotOutput("myWorkflow4"),bold = T, title = "Participant Type Classification" ,
                         background = "yellow", solidHeader = T, status = "primary", collapsible = T)
                              
                     
                     ),
                     
                     
                          tabPanel("Workflow Summary Dashboard", 
                                   infoBox("Recruitment Hand-off to Workflow", paste(handoffCnt, '/',handoffCntP), icon = icon("globe"), col = "yellow"),
                                   infoBox("Workflow Complete", paste(draftcomCnt, '/',draftcomCntP), icon = icon("industry"), col = "blue"),
                                   infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("thumbs-up", lib = "glyphicon"), col = "green"),
                                  
                                   box(title = "Average Days Spent between Workflow Stages", bold = T,  
                                       background = "olive", solidHeader = T, status = "primary", collapsible = T, 
                                       amChartsOutput("myWorkflow1")),
                                   
                                   box(title = "Time Series Chart for the 3 Process", bold = T,  
                                       background = "olive", solidHeader = T, status = "primary", 
                                       collapsible = T,plotOutput("myWorkflow2"))
                                   
                                   # box(title = "Time Series Chart for the 3 Process", bold = T,  
                                   #     background = "olive", solidHeader = T, status = "primary", 
                                   #     collapsible = T,plotOutput("myWorkflow3")),
                                   # 
                                   # box(title = "Time Series Chart for the 3 Process", bold = T,
                                   #     background = "olive", solidHeader = T, status = "primary",
                                   #     collapsible = T,plotOutput("myWorkflow4"))
                                   
                                     
                                   
                                   
                                   # plotOutput("myWorkflow2"),
                                   
                                   
                                   ),
                                   
                                     
                                     #column(12, plotOutput("myWorkflow2"))
                                  
                          tabPanel("Implementation Summary Dashboard", 
                                   infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("industry")),
                                   infoBox("Implementation Recorded", paste(OrgimpCnt, '/',OrgimpCntP), icon = icon("cloud", lib = "glyphicon"), col = "yellow"),
                                   #to work on below calculation!
                                   infoBox("Total Operational", paste(impOperationalCnt, '/', impOperationalP), icon = icon("thumbs-up", lib = "glyphicon"), col = "green"),
                                  fluidRow(
                                    
                                    box(title = "Inbound/Outbound", bold = T,  
                                        solidHeader = T, status = "primary",collapsible = T,
                                      checkboxInput("select_inbound_c14",  impl.colnames[15], value = TRUE, width = NULL),
                                      checkboxInput("select_query_c16", impl.colnames[17], value = FALSE, width = NULL),
                                      checkboxInput("select_portal_c17", impl.colnames[18], value = FALSE, width = NULL),
                                      checkboxInput("select_alerts_c18", impl.colnames[19], value = FALSE, width = NULL),
                                      checkboxInput("select_email_c19", impl.colnames[20], value = FALSE, width = NULL),
                                      checkboxInput("select_crisis_c20", impl.colnames[21], value = FALSE, width = NULL),
                                      selectInput("impl_stage", "Status of Implementation", c("Operational", "Planning", "Underway"), 
                                                  multiple = T, selected = "Operational"), height = 400
                                      
                                    ),
                                    
                                    
                                   
                                   box(collapsible = T,  status = "primary", solidHeader = T,
                                   DT::dataTableOutput("myImplementation"), width = 6, height = 400
                                  
                                   )
                                   # box(title = "Project Division", bold = T,  
                                   #     solidHeader = T, status = "primary",collapsible = T,
                                   #     plotOutput("myImplementation2",
                                   #                
                                   #                height = 300), width = 6            
                                   # )
                                   
                                   
                                  )
                                   
                                   ),
                          
                      tabPanel("Datafeed Summary Table",
                               infoBox("Recruitment Hand-off to Workflow", paste(handoffCnt, '/',handoffCntP), icon = icon("globe"), col = "green"),
                               infoBox("Workflow Complete", paste(draftcomCnt, '/',draftcomCntP), icon = icon("industry"), col = "yellow"),
                               infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("thumbs-up", lib = "glyphicon"), col = "blue"),
                     
                              dataTableOutput("myDatafeed")),
                     # tabPanel("Datafeed Summary Dashboard",
                     #          infoBox("Recruitment Hand-off to Workflow", paste(handoffCnt, '/',handoffCntP), icon = icon("users")),
                     #          infoBox("Workflow Complete", paste(draftcomCnt, '/',draftcomCntP), icon = icon("info"), col = "yellow"),
                     #          infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("thumbs-up", lib = "glyphicon"), col = "green")
                     #          
                     #          
                     #          
                     #          ),
                     
                     tabPanel("Geographic Map of Particpants",
                              infoBox("Recruitment Hand-off to Workflow", paste(handoffCnt, '/',handoffCntP), icon = icon("globe"), col = "green"),
                              infoBox("Workflow Complete", paste(draftcomCnt, '/',draftcomCntP), icon = icon("industry"), col = "yellow"),
                              infoBox("Workflow Hand-off to Implementation", paste(finalCnt, '/', finalCntP), icon = icon("thumbs-up", lib = "glyphicon"), col = "blue"),
                              
                              leafletOutput("myMap", width = "100%", height = 800)),
                     
                     tabPanel("Tracking Process Movement",
                              
                              dygraphOutput("mydygraph")
                       
                       
                     )
                     
                     # tabPanel("Sample Workflow Time Series Plots",
                     #          
                     #          box(plotOutput("myWorkflow3"))
                     #          # box(plotOutput("myWorkflow4"))
                     #          # 
                     # )
                     
                    
                           #tabPanel("Structure", verbatimTextOutput("myStr"))
                          
                         
                          # tabPanel("Audit Log", textOutput("myName")
                          #   # sidebarLayout(
                          #   #   sidebarPanel(h4("User Logs"), position = "center",
                          #   #      textOutput("myName"), textOutput("myDepartment"), textOutput("myUser")),
                          #   #   mainPanel())
                          # )
              
              
              
    
                )
  

  
  )
  )


# mymap <- leaflet(),
# mymap <- addProviderTiles(mymap, provider = "CartoDB.Positron"),      
  
  
