#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) {
        output$ny_map <- renderLeaflet({
                leaflet() %>% 
                        addProviderTiles("CartoDB.Positron", 
                                         options = providerTileOptions(noWrap = TRUE)) %>% 
                        setView(lng = -73.99928, lat = 40.80542, zoom = 10)
        })
        
             observe({
                zipcodes <- if(is.null(input$borough)) character(0) else {
                        data_app %>%
                                filter(borough %in% input$borough) %>%
                                '$'('zip.code') %>%
                                unique() %>%
                                sort() 
                } 
                
                selected <- isolate(input$zipcode[input$zipcode %in% zipcodes])
                updateSelectInput(session, 'zipcode', choices = zipcodes, selected = selected)
        })
        
        dm <- reactive({
               data_app %>% filter(
                       category %in% input$category,
                       impact %in% input$impact,
                       is.null(input$borough)|borough %in% input$borough,
                       is.null(input$zipcode)|zip.code %in% input$zipcode
               ) 
        })
        
        
      
        vals_month <- reactiveValues(keepm = rep(TRUE, nrow(isolate(dm()))))
        vals_time <- reactiveValues(keept = rep(TRUE, nrow(isolate(dm()))))
        
        observeEvent(input$brush_month, {
                vals_month$keepm <- brushedPoints(dm(), input$brush_month, allRows = TRUE)$selected_
        })
        
        observeEvent(input$brush_time, {
                vals_time$keept <- brushedPoints(dm(), input$brush_time, allRows = TRUE)$selected_
        })
        
        
       
 

              output$plot_month <- renderPlot({

                     dmo <- if(!is.null(input$brush_time)){
                              dm()[vals_time$keept, , drop = FALSE]
                      } else {
                              dm()
                      }
                      
                      
                     dmo <- dmo %>% group_by(month) %>% 
                              mutate(total_month = sum(count)) %>% 
                             arrange(month)
                     
                     if (!sum(dmo$total_month) %in% 0)
                      
                      ggplot(dmo, aes(x = month, y = total_month)) + 
                              geom_line(aes(color = "red")) +
                              theme_classic() +
                              theme(legend.position = "none",
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank()) +
                              scale_x_continuous(breaks = seq(1, 12, by = 1))+
                              scale_y_continuous(breaks = c(max(dmo$total_month), median(dmo$total_month)))
              
                      
              })
              
              output$month_text <- renderText({
                      dmo <- if(!is.null(input$brush_time)){
                              dm()[vals_time$keept, , drop = FALSE]
                      } else {
                              dm()
                      }
                      
                      
                      dmo <- dmo %>% group_by(month) %>% 
                              mutate(total_month = sum(count)) %>% 
                              arrange(month)
                      
                      if (sum(dmo$total_month) %in% 0)
                              paste('No records for', input$impact, paste0(input$category, "s"),'in the selected region per the time frame.')
              })
        
        output$plot_time <- renderPlot({
                
                dt <- if(!is.null(input$brush_month)){
                        dm()[vals_month$keepm, , drop = FALSE]
                } else {
                        dm()
                }
                
                dt <- dt %>% group_by(time) %>% 
                        mutate(total_time = sum(count)) %>% 
                        arrange(time)
                
                if (sum(dt$total_time) >= 1)
                
                ggplot(dt, aes(x = time, y = total_time, color = "red")) + 
                        geom_line() +
                        theme_classic() +
                        theme(legend.position = "none",
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank())+
                        scale_x_continuous(breaks = seq(0, 23.59, by = 1)) +
                        scale_y_continuous(breaks = c(max(dt$total_time), median(dt$total_time)))
                
        })
        
        output$time_text <- renderText({
                dt <- if(!is.null(input$brush_month)){
                        dm()[vals_month$keepm, , drop = FALSE]
                } else {
                        dm()
                }
                
                dt <- dt %>% group_by(time) %>% 
                        mutate(total_time = sum(count)) %>% 
                        arrange(time)
                
                if (sum(dt$total_time) %in% 0)
                        paste('No records for', input$impact, paste0(input$category, "s"), 'in the selected region per the selected time frame.')
                        
                
        })
    
       
        
        data_map <- reactive({ 
                if(is.null(input$brush_month) & is.null(input$brush_time)){
                        dm()
                } else if (is.null(input$brush_month) & !is.null(input$brush_time)){
                        dm()[vals_time$keept, , drop = FALSE]
                } else if (!is.null(input$brush_month) & is.null(input$brush_time)) {
                        dm()[vals_month$keepm, , drop = FALSE]
                } else {
                        dm()[(vals_month$keepm & vals_time$keept), , drop = FALSE] 
                }
                
        })
     
        
        observeEvent(input$update, {
                dat <- isolate(data_map()) %>% 
                        group_by(borough, zip.code, latitude, longitude, category, impact) %>%
                        summarize(total_count_map = sum(count)) %>%
                        filter(total_count_map >= 1) %>%
                        arrange(-total_count_map)
                
                
                output$table1 <- renderDataTable(dat)
                
                output$note <- renderText({
                        if (sum(dat$total_count_map) %in% 0){
                               paste('No records for', input$impact, paste0(input$category, "s"),'in the selected region per the selected time frame.')
                        }  else if (sum(dat$total_count_map) %in% 1){
                               paste('Only 1', input$category, 'was', input$impact, 'in the selected region per the selected time frame.')  
                                
                        } else {
                                paste('The total of', sum(dat$total_count_map), paste0(input$category, "s"),'were', input$impact, 'in the selected region per the selected time frame.') 
                        }
                })
                
                pop_cont <- paste(sep = "<br/>", 
                                  paste0("<b>","Borough:","</b>"," ", dat$borough), 
                                  paste0("<b>","Zip Code:","</b>"," ", dat$zip.code), 
                                  paste0("<b>", "Total number of"," ", paste0(dat$category, "s")," ", dat$impact, ":","</b>"," ", dat$total_count_map))
                
                fill <- ifelse(dat$category %in% "pedestrians", "#feb24c", "#addd8e")
                circ <- ifelse(dat$impact %in% "injured", "#377eb8", "#e41a1c")
                
                leafletProxy("ny_map", data = dat) %>%
                        clearMarkerClusters() %>%
                        clearControls() %>%
                        addCircleMarkers(~longitude, ~latitude, popup = pop_cont, stroke = TRUE, radius = ~total_count_map*5, color = circ, fillColor = fill, fillOpacity = 1, clusterOptions = markerClusterOptions()) %>%
                        addLegend(colors = c("#feb24c", "#addd8e"), labels = c("pedestrians", "cyclists"), position = "bottomright", title = "   Category   ", opacity = 1) %>%
                        addLegend(colors = c("#377eb8", "#e41a1c"), labels = c("injured", "killed"), title = "Impact", opacity = 1)
        })
        
        observe({
                bor2 <- data_app %>%
                                filter(! borough %in% input$borComp1) %>%
                               '$'('borough') %>%
                                unique()
                
                
                selected <- isolate(input$borComp2[input$borComp2 %in% bor2])
                updateSelectInput(session, 'borComp2', choices = bor2, selected = selected)
        })
        
        
        
        dc <- reactive({
                data_app %>% 
                        filter(borough %in% input$borComp1|borough %in% input$borComp2) %>%
                        mutate(catImp = paste(impact, paste0(category, "s")))
                
        })
        
        observeEvent(input$compare, {
                dcomp <- isolate(dc()) %>% 
                        group_by(borough, category, impact) %>% 
                        summarise(total_borough = sum(count))
                
                output$plot_borough <- renderPlot(
                        ggplot(dcomp, aes(x = borough, y = total_borough, fill = borough)) +
                                geom_bar(stat = "identity") + 
                                scale_fill_brewer(palette = "Reds") + 
                                facet_wrap(category ~ impact, scales = "free") + 
                                geom_text(aes(label = total_borough), size = 3, vjust = 1) +
                                theme_minimal() +
                                theme(legend.position = "none",
                                      axis.title.x = element_blank(),
                                      axis.title.y = element_blank(),
                                      strip.text.x = element_text(size = 14))
                )
                
        })            
                    observeEvent(input$renderplot, {
                                dcomp_m <- isolate(dc()) %>% 
                                group_by(borough, month, category, impact) %>% 
                                summarise(total_month_comp = sum(count))
                        
                        output$plot_month2 <- renderPlot(
                                ggplot(dcomp_m, aes(x = month, y = total_month_comp, color = borough)) +
                                        geom_line() +
                                        geom_point() +
                                        scale_fill_brewer(palette = "Reds") + 
                                        facet_wrap(category ~ impact, scales = "free") +
                                        scale_x_continuous(breaks = seq(1, 12, by = 1)) +
                                        theme_minimal() +
                                        theme(axis.title.x = element_blank(),
                                              axis.title.y = element_blank(),
                                              strip.text.x = element_text(size = 14))
                        )
                    })
        
})
