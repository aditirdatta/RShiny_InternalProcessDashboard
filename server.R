

shinyServer(function(input,output){
    
  #Reactive Dataset Selection
  datasetInput <- reactive({
    return(switch(input$dataset, "Recruitment" = Recruitment,
                  "Workflow" = Workflow,
                  "Implementation" = Implementation,
                  "Datafeed" = Datafeed,
                  "GeoRecruitment" = GeoRecruitment))
  })
  
  cityInput <- reactive({
    return((input$City))
  }) 
  
  #Audit Log information
  # output$myName <- renderText(
  #     paste("User:", input$Name, "    Department:", input$Department, "      User Type:", input$User)        #paste("Department:", input$Department), paste("UserType:", input$User)
  #                                 )
  #  
  #   
  #   output$mySlider <- renderText(
  #      paste("You selected value:", input$Slider)
  #   )
    
    # output$myCity <- renderText(input$City)
    
    #Map
    output$myMap <- renderLeaflet({
      mymap <- leaflet()%>%
        addTiles() %>%
        
        setView(lng = mean(df$lon), lat = mean(df$lat), zoom = 6) %>%
        addMarkers(data = df, lng = ~lon, lat = ~lat, popup = ~label, group = "All Sites") %>%
        addMarkers(data = inboundSiteDF, lng = ~lon, lat = ~lat,  group = "Inbound", popup = ~label) %>%
        addMarkers(data = outboundSiteDF, lng = ~lon, lat =~lat, group = "Outbound", popup = ~label) %>%
        # , options = markerOptions(clickable = F, riseOnHover = T)
        
        addCircleMarkers(data = inboundSiteDF , lng = ~lon, lat = ~lat, fillColor = "red", stroke = F, fillOpacity = 0.8, group = "Inbound") %>%
        addCircleMarkers(data = outboundSiteDF, lng = ~lon, lat = ~lat, fillColor = "yellow", fillOpacity = 0.4, group = "Outbound") %>%
        addCircleMarkers(data = Operational.Portal.InstallationsDF, lng = ~lon, lat = ~lat, fillColor = "yellow", fillOpacity = 0.8, group = "Portal.Installations") %>%
        addCircleMarkers(data = Operational.Alerts..InstallationsDF, lng = ~lon, lat = ~lat, fillColor = "yellow", fillOpacity = 0.8, group = "Alerts..Installations") %>%
        addCircleMarkers(data = Operational.Query.Response.InstallationsDF, lng = ~lon, lat = ~lat, fillColor = "yellow", fillOpacity = 0.8, group = "Query.Response.Installations") %>%
        addCircleMarkers(data = Operational.Direct.Email.InstallationsDF, lng = ~lon, lat = ~lat, fillColor = "yellow", fillOpacity = 0.4, group = "Direct.Email.Installations") %>%
        # addCircleMarkers(data = Operational.Crisis.Portal.InstallationsDF, lng = ~lon, lat = ~lat, fillColor = "orange", fillOpacity = 0.4, group = "Crisis.Portal.Installations") %>%
        
        addLayersControl(
          overlayGroups = c("All Sites","Inbound","Outbound","Portal.Installations","Alerts..Installations","Query.Response.Installations",
                            "Direct.Email.Installations","Crisis.Portal.Installations"),
          options = layersControlOptions(collapsed = FALSE)
        )
     
      
       
    })  
    
          
    #Plot
    output$myPlot <- renderPlot({
    piePlotdata <- aggregate(.~Gender, )
    }
      
    )
    
      
      # colm <- as.numeric(input$channel)
      # hist(datafeed[colm], breaks = seq(0, max(datafeed[,colm])), l = input$bin+1) 
                                
    #Tabset
    output$myImage <- renderImage({
      outfile <- tempfile(fileext = "Datasets/ProcessMap.png")
      png(outfile)
      dev.off()
      list(src = outfile, contentType = 'image/png', alt = "alt text")
      
    })
    

    
    # output$myWorkflow1 <- renderPlot({
    #   
    #   typel <- table(Workflow$Type)
    #   lbl <- paste(names(typel), "/", typel, sep = "")
    #   legend("center", names(typel))
    #   pie(typel,  radius = 1, shade = 15, main = "Segregation by Participant Type")
      # pie3D.labels(radialpos = type, labelrad =, labels = lbl)
                                     # })
 # Workflow  
   
    output$myWorkflow1 <- renderAmCharts({
      
      amBarplot(data = tot_duration, y = "Total Days", x = "Transition Stage",  horiz = T,export = T,
                show_values = T, depth = 15, stack_type = "regular", creditsPosition = "bottom-right")
                                
      # hist(as.numeric(difftime(inviteDate,handoffDate, units = "days")), breaks = 100, xlim = c(0,100), border = "blue", col = "lightgreen")
      # lines(density(as.numeric(difftime(inviteDate,handoffDate, units = "days"))))
     
    })
    
    output$myWorkflow2 <- renderPlot({p1})
    
    # 5,6 are extra graph examples for time series representation
    output$myWorkflow6 <- renderPlot({p})
    
     output$myWorkflow5 <- renderPlot({
     
       barplot(c(timelength_handoff$Count_ho, timelength_draftcomp$Count_dc, timelength_final$Count_f), main = "Main",
       xlab = "Month", beside = T, axisnames = T, col = c("blue","green", "red"), fill = c(timelength_handoff,timelength_draftcomp,timelength_final),
       legend = c(timelength_final$Month_Final, timelength_draftcomp$Month_Draftcomp,timelength_handoff$Month_Handoff))
      
 
     })
     
     
   # Below graph is represented in Recruitment tab
     
     output$myWorkflow3 <- renderAmCharts({
       amBarplot(x = "EMR", y = "Count_f", data = EMRrecordCnt, ylab = "Frequency", depth = 25, 
                 labelRotation = 0,creditsPosition = "bottom-left", horiz = T, zoom = T)
       
     })
     output$myWorkflow4 <- renderPlot({
       
       treemap::treemap(GeoRecruitment, index="Type", vSize="Latitude", type="index", title = "")
       
       # Awesome interactive treemap : itreemap(particpantType, index = "Type", vSize = "Count_t", vColor = "Type")
       
       # map.market(id = particpantType$ID[-1], area = particpantType$Count_t[-1], group = particpantType$Type[-1], 
       #           color = particpantType$ID[-1], lab   = c("group"=T, "id"=F), main = "")
       
      
     })
#Implementation
     
      #Implementation2 <- Implementation[sample(nrow(Implementation),100),]
  selected_impl_stage   <- reactive(return(input$impl_stage)) 

      #Implementation Dataset - Checkbox input
  
implCheckbox <- reactive({
  TEMP = Implementation
  if (input$select_inbound_c14 == TRUE)
  {TEMP <- TEMP %>%
    dplyr::filter(TEMP[,15] == selected_impl_stage())
  }
  if (input$select_query_c16 == TRUE )
  {TEMP <- TEMP %>%
    dplyr::filter(TEMP[,17] == selected_impl_stage())
  }

  if (input$select_portal_c17 == TRUE)
  {TEMP <- TEMP %>%
    dplyr::filter(TEMP[,18] == selected_impl_stage())

  }

  if (input$select_alerts_c18 == TRUE)
  {TEMP <- TEMP %>%
    dplyr::filter(TEMP[,19] == selected_impl_stage())
  }

  if (input$select_email_c19 == TRUE)
  {TEMP <- TEMP %>%
    dplyr::filter(TEMP[,20] == selected_impl_stage())
  }

  if (input$select_crisis_c20 == TRUE)
  {TEMP <- TEMP %>%
    dplyr::filter(TEMP[,21] == selected_impl_stage())
  }
  
  TEMP <- TEMP %>%
    dplyr::select(Participant.Name, Project.Manager )
  
  return(TEMP)
})    
output$myImplementation <- DT::renderDataTable({
  DT::datatable(data = implCheckbox(),
              options = list(scrollX = TRUE, pageLength = 7) )
  #implCheckbox()
  # names(Implementation[,c(14,16,17,18,19,20,21)
  # DT::datatable(Implementation[, input$in_out, drop = FALSE])
})

output$myImplementation2 <- renderPlot({
  barplot(div$count, horiz = TRUE, names.arg = paste((div$PM)), col = "blue", las = 1)
  
})
    
    output$myGeoRecruitment <- renderPlot({
      hist(GeoRecruitment$Population)
      })
    
    output$myStr <- renderPrint({
      str(datafeed)
    })
    
# Dataset selection
     output$myData <- renderDataTable({
       DT::datatable(data = datasetInput(), extensions = 'Buttons', options = list(dom = 'Bfrtip', 
                                                                                   buttons = list('copy', 'print'),
                                                                                   list(extend = 'collection', buttons = c('pdf', 'excel'),text = 'Download'),server = FALSE,
                                                                                   orderClasses = T, scrollX = T, 
                                                                                   initComplete = JS(
                                                                                     "function(settings, json){",
                                                                                     "$(this.api().table().header()).css({'background-color': 'darkblue', 'color': '#fff'});",
                                                                                     "}"),
                                                                                   options = list(scrollX = TRUE)
       ) )
                     
     })
    
# Datafeed Audit
     
     output$myDatafeed <- DT::renderDataTable({
       DT::datatable(dataAudit, extensions = 'Buttons', options = list(dom = 'Bfrtip', 
                                                             buttons = list('copy', 'print'),
                                                                      #list(extend = 'collection', buttons = c('pdf', 'excel'),text = 'Download')),server = FALSE,
                                                             orderClasses = T, scrollX = T, 
                                               order = list(list(4,'desc')), initComplete = JS(
         "function(settings, json){",
         "$(this.api().table().header()).css({'background-color': 'darkblue', 'color': '#fff'});",
         "}")
       ) ) %>%
                       formatStyle('ADT', backgroundColor = styleEqual("Y", "green")) %>%
                       formatStyle('LAB', backgroundColor = styleEqual("Y", "green")) %>% 
                       formatStyle('TRANS', backgroundColor = styleEqual("Y", "green")) %>%
                       formatStyle('RAD', backgroundColor = styleEqual("Y", "green")) %>%
                       formatStyle('PATH', backgroundColor = styleEqual("Y", "green")) %>%
                       formatStyle('Micro Bio', backgroundColor = styleEqual("Y", "green")) %>%
                       formatStyle('CVX', backgroundColor = styleEqual("Y", "green")) %>%
                       formatStyle('SIU', backgroundColor = styleEqual("Y", "green"))
                                               
                                               
                                               
     }) 
     

# Datafeed Audit Dashboard
     
     

#Summary Highlights
output$mySummary <- renderAmCharts({
  amFunnel(data = ChannelsP, depth = 50 , inverse = T, export = T,
        creditsPosition = "bottom-right") %>% plot(width = "100%", height = "100%")
})

output$mySummary1 <- renderAmCharts({
  amBarplot(x = "processName", y = "processCnt", export = T,
            data = processDF, labelRotation = -45, horiz = T,
            creditsPosition = "bottom-right", depth = 15, show_values = T, stack_type = "regular")
})
output$mydygraph <- renderDygraph({
    predicted <- function(){
  #hw <- HoltWinters(lungDeaths)
  #predict(lungDeaths, n.ahead = 12, prediction.interval = T, 
         # level = 0.95)
  }
  dygraph(lungDeaths, main = "Process Time Series") %>%
    # dySeries(c("lwr", "fit", "upr"), label = "Progress") %>%
    dyOptions(colors = RColorBrewer::brewer.pal(3,"Set2"), 
              fillGraph = T, fillAlpha = 0.4) %>%
    dyLegend(show = "never")
    # dyOptions(drawGrid)
})

})



    
# lat.long <- function(place)
# {
#   theURL <- sprintf('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=%s', place)
#   doc <- xmlToList(theURL)
#   data.frame(Place=place, Latitude=as.numeric(doc$result$geometry$location$lat), Longitude=as.numeric(doc$result$geometry$location$lng), stringsAsFactors=FALSE)
# }
# 
# places <- adply(facilityCW$Participant, 1, lat.long)
