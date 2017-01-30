#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(shiny)
library(shinythemes)
library(leaflet)
library(DT)
library(shinyBS)
library(RColorBrewer)
library(dplyr)
library(markdown)


shinyUI(
        navbarPage(title = "Explorer",
                   theme = shinytheme("united"),
                   tabPanel("Home", icon = icon('home'),
                          div(class = "outer",
                              tags$head(
                                      includeCSS("style.css")
                              )

                              ),
                          absolutePanel(
                                  id = "home",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = FALSE,
                                  top = 150,
                                  left = 150,
                                  right = 150,
                                  bottom = 50,
                                  
                        h2("Road Safety Explorer"),
                        h2("for Non-Motorists in"),
                        h2("New York City"),
                        br(),
                        br(),
                        h4('This tool is designed to explore the sites of motor vehicle collisions with an impact on pedestrians and cyclists.'),
                        h4('The data used for the creation of this application is downloaded from'),
                        tags$div(
                           tags$a(href="https://data.cityofnewyork.us/Public-Safety/NYPD-Motor-Vehicle-Collisions/h9gi-nx95", "NYC OpenData")
                       )  
                                  
                          )
                            
                            ), 
                   tabPanel("Interactive Map", icon = icon('map-marker'),
                            sidebarLayout(
                                    sidebarPanel(
                                            selectInput('category', 'Category', c("cyclist", "pedestrian")),
                                            
                                            selectInput('impact', 'Impact', c("killed", "injured")),
                                            
                                            selectInput('borough', 'Borough', c("All" = "", unique(as.character(data_app$borough))), selectize = TRUE, multiple = TRUE),
                                            
                                            conditionalPanel("input.borough",
                                            selectInput('zipcode', 'Zip Code', c("All" = ""), selectize  = TRUE, multiple = TRUE)
                                            ),
                                            
                                            h6("Brush to Select Month(s) of Accident Occurence"),
                                            plotOutput('plot_month', height = "50px",
                                                        brush = brushOpts(id = "brush_month",
                                                                          resetOnNew = FALSE,
                                                                          fill = "red", 
                                                                          stroke = "#036", 
                                                                          opacity = 0.3,
                                                                          delay = 500,
                                                                          delayType = "debounce",
                                                                          direction = "x")),
                                            verbatimTextOutput('month_text'),
                                            
                                            h6("Brush to Select Time of Accident Occurence"),
                                            plotOutput('plot_time', height = "50px",
                                                                 brush = brushOpts(id = "brush_time",
                                                                                   resetOnNew = FALSE,
                                                                                   fill = "red", 
                                                                                   stroke = "#036", 
                                                                                   opacity = 0.3,
                                                                                   delay = 500,
                                                                                   delayType = "debounce",
                                                                                   direction = "x")),
                                            verbatimTextOutput('time_text'),
                                            helpText('Note: To clear the brush selections, click in the plotting area.'),
                                           
                                            
                                            br(),
                                           fluidRow(
                                                   column(4
                                                          ),
                                                   column(3,
                                                         
                                            bsButton("update", label = "Update Map", icon = icon("map-marker"), class = "btn-primary")
                                                   )
                                           ) 
                                    ),
                                    mainPanel(
                                            leafletOutput('ny_map', width = "100%", height = "500px"),
                                    
                                            hr(),
                                            
                                            verbatimTextOutput('note'),
                                            helpText('Note: This text reflects the updated version of the map.')
                                )
                            )),
                   tabPanel("Data", icon = icon("filter", lib = "glyphicon"),
                            DT::dataTableOutput('table1')),
                   tabPanel("Borough by Borough Comparison", icon = icon('map-signs'),
                           sidebarPanel(selectInput('borComp1', 'Borough #1', c("All" = "", unique(as.character(data_app$borough)))),
                                        selectInput('borComp2', 'Borough #2', c("All" = ""))
                                   ),
                                   
                           mainPanel(
                                   tabsetPanel(
                                           tabPanel("Overall",icon = icon('map-signs'),
                                           bsButton("compare",icon = icon('map-signs'), label = "Render Plot", class = "btn-primary"),
                                           helpText('Select the boroughs to compare before pressing the button.'),
                                           plotOutput('plot_borough')
                                                   ),
                                   
                                           tabPanel("Monthly Trends", icon = icon('calendar'),
                                        fluidRow( 
                                                column(2
                                                       ),
                                                column(4,
                                                      
                                           bsButton("renderplot", label = "Render Plot", icon = icon("calendar"), class = "btn-primary"),
                                           helpText('Select the boroughs to compare before pressing the button.')
                                                )
                                           ),
                                           plotOutput('plot_month2') 
                                           
                                                   )
                                          
                                   )
                           )
                       ),
                   tabPanel("Documentation", icon = icon("info-circle"),
                            div(class = "outer"#,
                               # tags$head(
                                        #includeCSS("style.css")
                               # )
                                
                            ),
                           
                            absolutePanel(
                                    id = "documentation",
                                    class = "panel panel-default",
                                    fixed = TRUE,
                                    draggable = FALSE,
                                    top = 150,
                                    left = 250,
                                    right = 250,
                                    bottom = 50,
                                    
                            includeMarkdown("documentation.md")     
                                    
                            )
                            )
                   )
                   
                   
                   )
        


 