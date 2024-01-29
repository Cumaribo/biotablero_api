library(leaflet)
library(leaflet.extras)
library(shiny)
library(rgeos)
library(rgdal)
library(shinydashboard)
library(highcharter)
library(devtools)
library(shinycssloaders)
if(!'reshape2' %in% rownames(installed.packages())){installed.packages('reshape2')}
#if(!'write.xlsx' %in% rownames(installed.packages())){install.packages('write.xlsx')}

# install.packages('rgeos')
# install.packages('leaflet.extras'
# install.packages('highcharter')

devtools::source_url("https://raw.githubusercontent.com/gonzalezivan90/biotablero_api/master/biotablero_fun.R")
aws <- 'http://ec2-3-22-249-11.us-east-2.compute.amazonaws.com' # Prod
aws_port <- ':8080'

# setwd('C:/GoogleDrive/IAvH/V2/Others/polyg_draw/app8_upload/')

outDir <- ifelse( Sys.info()["sysname"] == "Linux",  '/home/shiny/tmpR/', 'C:/temp/Rtmp/'); dir.create(outDir)
sapply(grep(paste0(outDir, '//*.+'), list.dirs(outDir), value = TRUE), unlink, recursive = TRUE, force = TRUE)
print(paste('WD: ', getwd()))
isShpLoad <<- FALSE
shp <<- NULL
xls_recs <<- xls_biod <<- xls_uicn <<- data.frame(sp = c('Empty', 'Empty'))

hcNULL <- highchart() %>% hc_chart(type = "pie") %>% hc_title(text = 'Result is 0')
hcBIG <- highchart() %>% hc_chart(type = "pie") %>% hc_title(text = 'AOI bigger than threshold')

hcErrors <- highchart() %>% hc_chart(type = "") %>% 
  hc_title(text = paste('<b>Error:<\b> . ',  'This is a title with <i>margin</i> and <b>Strong or bold text</b>' ),
           margin = 20, style = list(fontSize = "30px", color = "red", useHTML = TRUE))


# UI  ---------------

ui <- dashboardPage(skin = 'green',
                    dashboardHeader(
                      title = "Decision support sytem for Colombian BON v. Beta", 
                      
                      #title = tags$a(#href='http://rsensus.org/en/', "Decision support sytem for Colombian BON v. Beta"),
                      titleWidth = 600),
                    dashboardSidebar(
                      sidebarMenu(
                        # UI Panel  ---------------
                        
                        menuItem("Intro", tabName = "intro"),
                        menuItem("Define AOI", tabName = "draw") ,
                        
                        menuItem("Ecosystems", tabName = "tab_ecosystem", startExpanded = TRUE,
                                 menuSubItem("Forest loss", tabName = "in_forest"),
                                 menuSubItem("Corine Land Cover", tabName = "in_clc"),
                                 menuSubItem("Ecosystem red list", tabName = "in_red"),
                                 menuSubItem("Biotic region", tabName = "in_biot"),
                                 menuSubItem("Biome", tabName = "in_biom"),
                                 menuSubItem("Tropical dry forest", tabName = "in_dry"),
                                 menuSubItem("Paramos", tabName = "in_param"),
                                 menuSubItem("Wetlands", tabName = "in_wet")
                        ),
                        
                        menuItem("Managment", tabName = "tab_managment", startExpanded = TRUE,
                                 menuSubItem("Protected areas", tabName = "in_ap"),
                                 menuSubItem("Colective areas", tabName = "in_cole"),
                                 menuSubItem("Special managment", tabName = "in_sma"),
                                 menuSubItem("Compensation factor", tabName = "in_comp")
                        ),
                        menuItem("Species", tabName = "tab_species", startExpanded = TRUE,
                                 menuSubItem("UICN", tabName = "in_uicn"),
                                 menuSubItem("Biomodelos", tabName = "in_biod"),
                                 menuSubItem("GBIF", tabName = "in_rec")
                        ),
                        menuItem("Indexes", tabName = "tab_index", startExpanded = TRUE,
                                 menuSubItem("Red list index", tabName = "in_rli"),
                                 menuSubItem("Surface", tabName = "in_sur")
                        )
                      )
                    ),
                    
                    
                    dashboardBody(
                      tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          background-color: #3c8dbc;
        }
        .skin-blue .main-header .logo:hover {
          background-color: #3c8dbc;
        }
      '))),
                      tabItems(
                        tabItem("intro",
                                fluidRow(
                                  tabBox(width = 12,

                                         tabPanel("Home",
                                                  includeMarkdown("md_intro.md")
                                         ),
                                         tabPanel("People",
                                                  includeMarkdown("md_ppl.md")
                                         ),
                                         tabPanel("Objectives",
                                                  includeMarkdown("md_obj1.md")
                                         ),
                                         tabPanel("Milestones",
                                                  includeMarkdown("md_obj2.md")
                                         ),
                                         tabPanel("Metrics",
                                                  includeMarkdown("md_table.md")
                                         )
                                  )
                                )
                        ),
                        # UI draw pol   ---------------
                        tabItem("draw",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,  
                                  column(width = 4,
                                         h5('Load either the following:'),
                                         h5('- ZIP fie containing a ESRI Shapefile polygon'),
                                         h5('- ESRI Shapefile files (at least .shp, .shx, .dbf'),
                                         h5('- GeoJSON file'),
                                         h5('- SQLite file'),
                                         h5('- Geopackage file'),
                                         h5('The polygon must be a single geometry, smaller than 100000 Km and in WGS84 projection'),
                                         h5(''),
                                         
                                         
                                         
                                         shiny::fileInput('shapefile', 'Input Polygon/AOI',
                                                          accept=c('.shp','.dbf','.sbn','.sbx','.shx',".prj", '.zip', '.gpkg', '.SQLite', '.GeoJSON'),
                                                          #accept= '.zip',
                                                          multiple=TRUE),
                                         actionButton("loadShp", "Load")),
                                  column(width = 6, leafletOutput("loadMapLL", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        ######## Ecosystems
                        # UI Ecosystems  ---------------
                        tabItem("in_forest",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 4, 
                                         selectInput(inputId = "aoi_forest", label = "AOI: ", choices =  c('Draw'), selected = 'Draw'),
                                         selectInput(inputId = "forestsour", label = "Source: ", choices =  c('ideam', 'hansen'), selected = 'hansen')),
                                  # selectInput(inputId = "forestvar", label = "Metric: ",
                                  #             choices =  c('area'), selected = 'area'),
                                  column(width = 4, sliderInput(inputId = "forestyearrng", label = 'Data range',
                                                                min = 1990, max = 2018, value=c(2000, 2016)),
                                         sliderInput(inputId = "forestporcrng", label = 'Forest cover percentage:',
                                                     min = 0, max = 100, value=c(80, 100))),
                                  column(width = 1, actionButton("go_forest", "Run"))
                                ),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 6, leafletOutput("leafForest", height = "600px") %>% withSpinner(color="#0dc5c1"))
                                  , column(width = 6, highchartOutput("foresttrend", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_clc",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                  column(width = 6,
                                         fluidRow(
                                           column(width = 6, selectInput(inputId = "aoi_clc", 
                                                                         label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 4, selectInput(inputId = "clc_lev", 
                                                                         label = "Level: ", choices =  c(1:3), selected = 1)),
                                           column(width = 2, actionButton("go_clc", "Run")) 
                                         ),
                                         leafletOutput("clcLeaf", height = "600px")
                                  ),
                                  
                                  column(width = 6, 
                                         highchartOutput("clcPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("clcPlot2", height = "600px")%>% withSpinner(color="#0dc5c1")
                                  )
                                )
                        ),
                        
                        tabItem("in_red",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_red", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_red", "Run"))
                                         ),
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           leafletOutput("redLeaf", height = "600px") %>% withSpinner(color="#0dc5c1"))),
                                  column(width = 6, highchartOutput("redPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("redPlot2", height = "600px")%>% withSpinner(color="#0dc5c1")
                                  )
                                )
                        ),
                        
                        tabItem("in_biot",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_biot", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_biot", "Run"))
                                         ),
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           leafletOutput("biotLeaf", height = "600px")%>% withSpinner(color="#0dc5c1"))),
                                  column(width = 6, highchartOutput("biotPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("biotPlot2", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_biom",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_biom", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_biom", "Run"))
                                         ),
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           leafletOutput("biomLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         ))
                                  , 
                                  column(width = 6, highchartOutput("biomPlot1", height = "600px")%>% withSpinner(color="#0dc5c1"),
                                         highchartOutput("biomPlot2", height = "600px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_dry",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_dry", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_dry", "Run"))),
                                         fluidRow(
                                           leafletOutput("dryLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("dryPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("dryPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_param",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_param", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_param", "Run"))),
                                         fluidRow(
                                           leafletOutput("paramLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("paramPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("paramPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        tabItem("in_wet",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_wet", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_wet", "Run"))),
                                         fluidRow(
                                           leafletOutput("wetLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("wetPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("wetPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        ######## UI Managment -----
                        
                        tabItem("in_ap",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_ap", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_ap", "Run"))),
                                         fluidRow(
                                           leafletOutput("apLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("apPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("apPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_cole",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_cole", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_cole", "Run"))),
                                         fluidRow(
                                           leafletOutput("coleLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("colePlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("colePlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_sma",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_sma", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_sma", "Run"))),
                                         fluidRow(
                                           leafletOutput("smaLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("smaPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("smaPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_comp",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 8, selectInput(inputId = "aoi_comp", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 1, br(), actionButton("go_comp", "Run"))),
                                         fluidRow(
                                           leafletOutput("compLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, 
                                         highchartOutput("compPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"), 
                                         highchartOutput("compPlot2", height = "300px")%>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        ######## UI Species -----
                        tabItem("in_uicn",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "aoi_uicn", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 3, br(), actionButton("go_uicn", "Run")),
                                           column(width = 3, br(), downloadButton("uicn_xls", "Download"))
                                         ),
                                         fluidRow(
                                           leafletOutput("uicnLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, dataTableOutput('tablespuicn')%>% withSpinner(color="#0dc5c1") )
                                )
                        ),
                        tabItem("in_biod",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "aoi_biod", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 3, br(), actionButton("go_biod", "Run")),
                                           column(width = 3, br(), downloadButton("biod_xls", "Download"))),
                                         fluidRow(
                                           leafletOutput("biodLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, dataTableOutput('tablespbiod') %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        tabItem("in_rec",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 6, selectInput(inputId = "aoi_rec", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 3, br(), actionButton("go_rec", "Run")),
                                           column(width = 3, br(), downloadButton("recs_xls", "Download"))),
                                         fluidRow(
                                           leafletOutput("recLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")))
                                  ,
                                  column(width = 6, dataTableOutput('tablesprec') %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        
                        ######## UI Indexes -----
                        tabItem("in_sur",
                                fluidRow(h3(' ')),
                                h5(''),
                                fluidRow(
                                  column(width = 6, 
                                         fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                           column(width = 5 , selectInput(inputId = "aoi_sur", label = "AOI: ", choices =  c('Draw'), selected = 'Draw')),
                                           column(width = 5 , selectInput(inputId = "sur_size", label = "Cell size: ", 
                                                                          choices =  rev(c(1, 2, 5, 10, 20, 50, 100, 200, 500)), selected = 20)),
                                           column(width = 2, br(), actionButton("go_sur", "Run"))
                                         ),
                                         fluidRow(
                                           leafletOutput("surLeaf", height = "600px")%>% withSpinner(color="#0dc5c1")
                                         )
                                         
                                  ),
                                  column(width = 6, 
                                         highchartOutput("surPlot1", height = "800px") %>% withSpinner(color="#0dc5c1"))
                                )
                        ),
                        tabItem("in_rli",
                                fluidRow(h3(' ')),
                                h5(''),
                                column(width = 1, 
                                       fluidRow(#width = 6, status = "info", solidHeader = TRUE, title = "Title", height = 500,
                                         column(width = 1, actionButton("go_rli", "Run"))))
                                ,
                                column(width = 11, 
                                       highchartOutput("rliPlot1", height = "400px") %>% withSpinner(color="#0dc5c1"))
                        )
                      )
                    )
)

# forest
# clc
# red
# biot
# biom
# dry
# param
# wet
# ap
# cole
# sma
# comp
# uicn
# biom
# rec
# sur
# rli


##### SERVER ----------------------
server <- function(input, output, session) {
  # leafForest, clcPlot1, clcPlot2, redPlot1, redPlot2, biotcPlot1, biotcPlot2, biomPlot1, biomPlot2, 
  # dryPlot1. dryPlot2, paramPlot1, paramPlot2, wetPlot1, wetPlot2, apPlot1, apPlot2, colePlot1, colePlot2,
  # smaPlot1, smaPlot2, compPlot1, compPlot2, uicnPlot1, uicnPlot2, biodPlot1, biodPlot2, recPlot1, recPlot2, 
  # rliPlot1, rliPlot2, surPlot1, surPlot2
  
  
  ##### Default widgets ----------------------
  
  output$foresttrend <- renderHighchart({NULL})
  output$loadMapLL <- renderHighchart({NULL})
  output$paramPlot1 <- renderHighchart({NULL})
  output$paramPlot2 <- renderHighchart({NULL})
  output$dryPlot1 <- renderHighchart({NULL})
  output$dryPlot2 <- renderHighchart({NULL})
  
  output$clcPlot1 <- output$clcPlot2 <- output$redPlot1 <- output$redPlot2 <- output$biotPlot1 <- output$biotPlot2 <- output$biomPlot1 <- output$biomPlot2 <- output$dryPlot1 <- output$dryPlot2 <- 
    output$paramPlot1 <- output$paramPlot2 <- output$wetPlot1 <- output$wetPlot2 <- output$apPlot1 <- output$apPlot2 <- output$colePlot1 <- output$colePlot2 <- output$smaPlot1 <- output$smaPlot2 <- 
    output$compPlot1 <- output$compPlot2 <- output$uicnPlot1 <- output$uicnPlot2 <- output$biodPlot1 <- output$biodPlot2 <- output$recPlot1 <- 
    output$recPlot2 <- output$rliPlot1 <- output$rliPlot2 <- output$surPlot1 <- output$surPlot2 <- renderHighchart({NULL})
  
  output$loadMapLL <- renderLeaflet({
    reactShp$leaf0
  })
  
  output$tablesprec <- output$tablespuicn <- output$tablespbiod <- renderDataTable(NULL)
  
  ##### Leaflets ----------------------
  
  output$clcLeaf <- output$redLeaf <- output$biotLeaf <- output$biomLeaf <- output$wetLeaf <- 
    output$apLeaf <- output$coleLeaf <- 
    output$smaLeaf <- output$compLeaf <- output$uicnLeaf <- output$biodLeaf <- output$recLeaf <- output$surLeaf <- 
    output$dryLeaf <- output$paramLeaf <- output$leafForest <- renderLeaflet({
      reactShp$leaf0 %>%
        leaflet.extras::addDrawToolbar(targetGroup='draw', polylineOptions = FALSE,
                                       rectangleOptions = FALSE, circleOptions = FALSE,
                                       markerOptions = FALSE, circleMarkerOptions = FALSE,
                                       editOptions = leaflet.extras::editToolbarOptions())
      #addDrawToolbar(editOptions = editToolbarOptions())
    })
  
  
  
  reactShp <- reactiveValues(shp = FALSE,
                             leaf0 = leaflet()%>% addTiles() %>% 
                               setView(lng = -73.7, lat = 4.28, zoom = 8))
  hc1 <<- hc2 <<- NULL 
  
  ##### Load shp  ----------------------
  
  observeEvent(input$loadShp, {
    #values$total <- values$total + 1
    inFiles <- input$shapefile
    #setwd('../app6_v0_Tabs/')
    #x0 <- inFiles; print(x0); print(class(x0)); print(str(x0))
    
    if ( class(inFiles) != "NULL" ){
      
      if ( nrow(inFiles) == 1 & grepl('*\\.zip', inFiles$name)){ ## zip files
        outZip <- tempfile(tmpdir = outDir); dir.create(outZip)
        unzip(zipfile = inFiles$datapath, exdir = outZip)
        uZ <- list.files(outZip)
        #x0 <- uZ; print(x0); print(class(x0)); print(str(x0))
        #shp <- readOGR('.', 'llano')
        #save(inFiles, outZip, uZ, file = 'inFileShp.RData');
        #load('inFile.RData')
        shp <<- tryCatch(readOGR(outZip, layer = tools::file_path_sans_ext(uZ[1])), error = function (e) NULL)
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
              min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          {
            print("loaded!")
            isShpLoad <<- TRUE
            reactShp$shp <- TRUE
          }
        }
      } else if ( nrow(inFiles) == 1 & grepl('\\.SQLite|\\.gpkg|\\.GeoJSON', inFiles$name)){ ## single
        #save(inFiles, file = 'inFileSingle.RData');
        
        shp <<- tryCatch(readOGR(inFiles$datapath[1]), error = function (e) NULL)
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
              min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          {
            print("loaded!")
            isShpLoad <<- TRUE
          }
        }
      } else if ( nrow(inFiles) >= 3  & all(sapply(c('\\.shp', '\\.shx', '\\.dbf'), grep, inFiles$name))){ ## shp several
        #save(inFiles, file = 'inFileSeveral.RData');
        
        inFiles$datapath2 <- gsub('\\/[0-9]\\.', '/1.', inFiles$datapath)
        sapply(inFiles$datapath,USE.NAMES = F, function(x){
          file.rename(x, 
                      gsub('\\/[0-9]\\.', '/1.', x)
          )
        })
        
        shp <<- tryCatch(readOGR(dirname(inFiles$datapath2[1]),
                                 basename(tools::file_path_sans_ext(inFiles$datapath2[1]))), error = function (e) NULL)
        
        print(shp)
        if(class(shp) == 'SpatialPolygonsDataFrame'){
          if( min(shp@bbox['x', ])>-180 & max(shp@bbox['x', ])<180 &
              min(shp@bbox['y', ])>-90 & max(shp@bbox['y', ])<90 )  #><
          {
            print("loaded!")
            isShpLoad <<- TRUE
          }
        }
      }
      
      if (isShpLoad){
        #print("   shp: "); print(shp)
        reactShp$shp <- TRUE
        reactShp$leaf0 <- leaflet()%>% addTiles() %>% addPolygons(data = shp)
        output$loadMapLL <- renderLeaflet({reactShp$leaf0})
        print("   reactShp: "); print(reactShp); print(str(reactShp))
        #save(reactShp, file = paste0('read.RData'))
        
        updateSelectInput(session, 'aoi_forest', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_clc', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_red', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_biot', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_biom', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_param', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_dry', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_wet', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_ap', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_cole', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_sma', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_comp', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_uicn', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_rec', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_biod', choices = c('Draw', 'Layer'), selected = 'Layer')
        updateSelectInput(session, 'aoi_sur', choices = c('Draw', 'Layer'), selected = 'Layer')
      }
      
    }
  })
  
  ##### Go buttons  ----------------------
  ##### Go buttons Forest  ----------------------  
  # input <- list(forestyearrng = c(2000, 2005), forestporcrng = c(25, 100), forestsour = 'ideam')
  
  isolate(observeEvent(input$go_forest,{
    readyLayer <- FALSE
    polDraw <- input$leafForest_draw_new_feature
    
    if( (!is.null(polDraw) & input$aoi_forest  == 'Draw') |
        (!is.null(shp) & input$aoi_forest  == 'Layer')) {
      
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_forest  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', 
                                        paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
      } else if (!is.null(shp) & input$aoi_forest == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, 
                            port = aws_port, endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      sprintf('Area: %s', biotKm2$polsizekm2)
      
      
      biotForYearString <- paste0(input$forestyearrng[1], ':', input$forestyearrng[2])
      biotForPorcString <- paste0(input$forestporcrng[1], ':', input$forestporcrng[2])
      
      # input <- list(forestsour = 'ideam')
      output$foresttrend <- renderHighchart({
        biotForest <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'forest',
                                 sour = input$forestsour, ebvstat = 'area',
                                 ebvyear=biotForYearString, ebvporcrange = biotForPorcString,
                                 pol = gwkt, printURL = TRUE)
        
        #save(biotForest, gwkt, biotForYearString, biotForPorcString , file = 'forestMet.RData')
        #load('forestMet.RData')
        print('Done querying bt\n')
        
        if(class(biotForest) == 'list'){
          if(class(biotForest$result) == 'data.frame'){
            
            hc1 <<- highchart() %>% hc_add_series(name = 'Area (km2)',
                                                  type = "line",
                                                  mapping = hcaes(x = year, y = area),
                                                  data = biotForest$result) %>%
              hc_title(text = paste( 'Area:', biotKm2, ' km2')) %>%
              hc_xAxis(title = list(text = paste('Time: ',
                                                 (biotForest$params['time',])))) %>%
              hc_exporting(enabled = TRUE)
            
          }
        } else if (class(biotForest) == 'character' | class(biotForest)[1] == 'simpleError'){
          
          hc1 <<- highchart() %>% hc_add_series(
            name = 'Error',
            type = "line",
            mapping = hcaes(x = 1, y = 1)) %>%
            hc_title(text = paste( 'Area:', biotKm2, ' km2')) %>%
            hc_xAxis(title = list(text = unlist(biotForest[1]))) %>% 
            hc_exporting(enabled = TRUE)
        }
        hc1
      })
      # input$leafmap_draw_new_feature <- NULL
    }
  }))
  
  
  ##### Go buttons CLC  ----------------------  
  
  observeEvent(input$go_clc,{
    readyLayer <- FALSE
    polDraw <- input$clcLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_clc  == 'Draw') |
        (!is.null(shp) & input$aoi_clc  == 'Layer')) {
      print('clc0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_clc  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        #print('clc1A')
        #save(gwkt, coordMat, file='Polclc.RData') # load('Polclc.RData') 
      } else if (!is.null(shp) & input$aoi_clc  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        #print('clc1B')
        #save(gwkt, shp, file='PolParamB.RData'); load('gwkt.RData')        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$clcPlot1 <- renderHighchart({
        #input <- list(clc_lev = 1)
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port, rasterLayer = TRUE,
                                 endpoint = 'biotablero', metric = 'clc', clclevel = input$clc_lev,
                                 pol = gwkt, printURL = TRUE)
        
        #print(getwd());save(biotKm2, biotMetric, file='metclc.RData') # load('metclc.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots clc')
            clcdf <- reshape2::melt(biotMetric$result)
            clcdf$name2 <- paste(clcdf$legend, clcdf$cod)
            
            hc1 <<- hchart(clcdf, "column", hcaes(x = legend, y = value, group = variable)) %>%
              hc_exporting(enabled = TRUE)  %>% hc_yAxis(title = list(text = 'Area (km2)')) %>% 
              hc_xAxis(title = list(text = 'CLC Category'))
            
            
            hc2 <<- hchart(clcdf, "line", hcaes(x = variable, y = value, group = legend)) %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE) %>% hc_yAxis(title = list(text = 'Area (km2)')) %>% 
              hc_xAxis(title = list(text = 'Year'))
            
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$clcPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  ##### Go buttons red  ----------------------  
  
  observeEvent(input$go_red,{
    readyLayer <- FALSE
    polDraw <- input$redLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_red  == 'Draw') |
        (!is.null(shp) & input$aoi_red  == 'Layer')) {
      print('red0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_red  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
      } else if (!is.null(shp) & input$aoi_XXX  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port, 
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$redPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'ecouicn', rasterLayer = TRUE,
                                 pol = gwkt, printURL = TRUE)
        #print(getwd());save(biotKm2, biotMetric, file='metred.RData') # load('metred.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Ecosystem red list')  %>% 
              hc_xAxis(title = list(text = 'Ecosystem red list')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$redPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons biom  ----------------------  
  
  observeEvent(input$go_biom,{
    readyLayer <- FALSE
    polDraw <- input$biomLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_biom  == 'Draw') |
        (!is.null(shp) & input$aoi_biom  == 'Layer')) {
      print('biom0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_biom  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_biom  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$biomPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'biome',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        #save(biotMetric, file = 'metBiome.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biom')
            
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Biomes')  %>% 
              hc_xAxis(title = list(text = 'Biomes')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$biomPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  ##### Go buttons botic  ----------------------  
  
  observeEvent(input$go_biot,{
    readyLayer <- FALSE
    polDraw <- input$biotLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_biot  == 'Draw') |
        (!is.null(shp) & input$aoi_biot  == 'Layer')) {
      print('biot0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_biot  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_biot  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$biotPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'bioticreg',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd());save(biotKm2, biotMetric, file='metbiot.RData') # load('metbiot.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biot')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Biotic region')  %>% 
              hc_xAxis(title = list(text = 'Biotic region')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$biotPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons params  ----------------------  
  
  observeEvent(input$go_param,{
    readyLayer <- FALSE
    polDraw <- input$paramLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_param  == 'Draw') |
        (!is.null(shp) & input$aoi_param  == 'Layer')) {
      print('Param0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_param  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        print('Param1A')
      } else if (!is.null(shp) & input$aoi_param  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        print('Param1B')   
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, 
                            port = aws_port, endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      sprintf('Area: %s', biotKm2$polsizekm2)
      
      #print(getwd());save(biotKm2, gwkt, file='metParam.RData')
      # load('metParam.RData')
      
      
      output$paramPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'param',
                                 pol = gwkt, printURL = TRUE)
        
        if(class(biotMetric)[1] == 'list'){
          
          if(class(biotMetric$result) == 'data.frame'){
            print('plots Param')
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = param, y = km2), name = 'Km2') %>%
              hc_yAxis(title = list(text = 'Km2')) %>% 
              hc_xAxis(title = list(text = 'Paramos'))
            
            pieCh <- data.frame(Km2 = c(sum(biotMetric$result$km2), biotKm2$polsizekm2 - sum(biotMetric$result$km2)), 
                                label = c('Paramo', 'No Paramo')) 
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            # hc2 <<- highchart() %>% hc_chart(type = "pie") %>% 
            #   hc_add_series_labels_values(labels = pieCh$label, values = pieCh$perc) %>% 
            #   hc_xAxis(title = list(text = 'Paramo')) %>% hc_yAxis(title = list(text = 'Km2'))
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2'))
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL
          }
        } else if (class(biotForest) == 'character' | class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      output$paramPlot2 <- renderHighchart({hc2})
    }
  })
  
  ##### Go buttons dry forest  ----------------------  
  
  observeEvent(input$go_dry,{
    readyLayer <- FALSE
    polDraw <- input$dryLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_dry  == 'Draw') |
        (!is.null(shp) & input$aoi_dry  == 'Layer')) {
      print('Dry0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_dry  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        #print('Dry1A')
        #save(gwkt, coordMat, file='PolParamADry.RData') # load('PolParamADry.RData')
      } else if (!is.null(shp) & input$aoi_dry  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        #print('Dry1B')
        #save(gwkt, shp, file='PolParamB.RData')
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$dryPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'tropdryforest',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        #print(getwd());save(biotKm2, biotMetric, file='metDry.RData') # load('metDry.RData')
        #load('C:/GoogleDrive/IAvH/V2/Others/polyg_draw/app6_v0_Tabs/metDry.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots Dry')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Bosque seco')  %>% 
              hc_xAxis(title = list(text = 'Bosque seco')) %>% hc_yAxis(title = list(text = 'Km2'))
            
            
            pieCh <- data.frame(Km2 = c(sum(biotMetric$result$km2), 
                                        biotKm2$polsizekm2 - sum(biotMetric$result$km2)), 
                                label = c('Bosque seco', 'No Bosque'))
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            # hc2 <<- highchart() %>% hc_chart(type = "pie") %>% 
            #   hc_add_series_labels_values(labels = pieCh$label, values = pieCh$perc) %>% 
            #   hc_xAxis(title = list(text = 'Paramo')) %>% hc_yAxis(title = list(text = 'Km2'))
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2'))
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL
            
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$dryPlot2 <- renderHighchart({hc2})
    }
  })
  
  
  
  ##### Go buttons wetlands  ----------------------  
  
  observeEvent(input$go_wet,{
    readyLayer <- FALSE
    polDraw <- input$wetLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_wet  == 'Draw') |
        (!is.null(shp) & input$aoi_wet  == 'Layer')) {
      print('wet0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_wet  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        #print('wet1A')
        #save(gwkt, coordMat, file='PolParamAwet.RData') # load('PolParamAwet.RData')
      } else if (!is.null(shp) & input$aoi_wet  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        #print('wet1B')
        #save(gwkt, shp, file='PolParamB.RData') 
        #load('PolParamB.RData')
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$wetPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'hum',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        #print(getwd());save(biotKm2, biotMetric, file='metwet.RData') # load('metwet.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots wet')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Wetlands')  %>% 
              hc_xAxis(title = list(text = 'Wetlands')) %>% hc_yAxis(title = list(text = 'Km2'))  %>%
              hc_exporting(enabled = TRUE)
            
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2'))  %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$wetPlot2 <- renderHighchart({hc2})
    }
  })
  
  
  ##### Go buttons ap  ----------------------  
  
  observeEvent(input$go_ap,{
    readyLayer <- FALSE
    polDraw <- input$apLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_ap  == 'Draw') |
        (!is.null(shp) & input$aoi_ap  == 'Layer')) {
      print('ap0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_ap  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_ap  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$apPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'protectareas',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        # print(getwd()); save(biotKm2, biotMetric, file='metap.RData') # load('metap.RData')
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biot')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = protected, y = km2, 
                                     color = category_p, 
                                     group = category_p, fill = category_p), 'Protected area')  %>% 
              hc_xAxis(title = list(text = 'Protected area')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$protected)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$apPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  ##### Go buttons cole  ----------------------  
  
  observeEvent(input$go_cole,{
    readyLayer <- FALSE
    polDraw <- input$coleLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_cole  == 'Draw') |
        (!is.null(shp) & input$aoi_cole  == 'Layer')) {
      print('cole0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_cole  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_cole  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$colePlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'colectareas',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd()); metName <- 'metcolle.RData'; save(biotKm2, biotMetric, file=metName) # sss <- load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots cole')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2, group = terType), 'Collective areas')  %>% 
              hc_xAxis(title = list(text = 'Collective areas')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$colePlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons sma  ----------------------  
  
  observeEvent(input$go_sma,{
    readyLayer <- FALSE
    polDraw <- input$smaLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_sma  == 'Draw') |
        (!is.null(shp) & input$aoi_sma  == 'Layer')) {
      print('sma0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_sma  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_sma  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$smaPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'sma',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd()); metName <- 'metsma.RData'; save(biotKm2, biotMetric, file=metName) # sss <- load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots sma')
            
            # biotMetric$result$result2$management 
            # iconv(biotMetric$result$result2$management, from = 'utf8', to = 'ascii', sub='')
            # iconv(biotMetric$result$result2$management, from = 'latin1', to = 'utf8', sub='')
            
            biotMetric$result$result2$name2 <- paste0(biotMetric$result$result2$management, '/', biotMetric$result$result2$category)
            
            
            hc1 <<- biotMetric$result$result2 %>%
              hchart('column', hcaes(x = management, y = km2, group = category), 'Special managment areas')  %>% 
              hc_xAxis(title = list(text = 'Special managment areas')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$result2$km2, 
                                label = biotMetric$result$result2$name2)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$smaPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons comp  ----------------------  
  
  observeEvent(input$go_comp,{
    readyLayer <- FALSE
    polDraw <- input$compLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_comp  == 'Draw') |
        (!is.null(shp) & input$aoi_comp  == 'Layer')) {
      print('comp0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_comp  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_XXX  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$compPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'faccomp',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd()); metName <- 'metcompfactor.RData'; save(biotKm2, biotMetric, file=metName) # load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots biot')
            biotMetric$result$name2 <- paste(biotMetric$result$name, biotMetric$result$factor)
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name2, y = km2), 'Compensation factor')  %>% 
              hc_xAxis(title = list(text = 'Compensation  factor')) %>% hc_yAxis(title = list(text = 'Km2')) %>%
              hc_exporting(enabled = TRUE)
            
            pieCh <- data.frame(Km2 = biotMetric$result$km2, 
                                label = biotMetric$result$name2)
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2')) %>%
              hc_exporting(enabled = TRUE)
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$compPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  
  ##### Go buttons uicn  ----------------------  
  
  observeEvent(input$go_uicn,{
    readyLayer <- FALSE
    polDraw <- input$uicnLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_uicn  == 'Draw') |
        (!is.null(shp) & input$aoi_uicn  == 'Layer')) {
      print('uicn0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_uicn  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_uicn  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      # print(getwd()); metName <- 'metcompfactor.RData'; save(biotKm2, biotMetric, file=metName) # load('gwkt.RData')
      output$tablespuicn <- renderDataTable({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'species', sour ='uicn',
                                 spFormat = 'list', pol = gwkt, printURL = TRUE)
        # print(getwd()); metName <- 'metuicn'; save(biotKm2, biotMetric, file=metName) # load(metName)
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            print('plots uicn')
            dfsp <- data.frame(species = biotMetric$result)
          } else if(class(biotMetric)[1] == 'simpleError'){
            dfsp <- data.frame(error = 'error')
          }
        }
        colnames(dfsp) <- gsub('species\\.', '', gsub('species\\.y$', 'species', colnames(dfsp)))
        xls_uicn <<- dfsp <<- dfsp[, 2:ncol(dfsp)]
        dfsp
      })
    }
  }) ## ending go button
  
  ##### Go buttons biod  ----------------------  
  
  observeEvent(input$go_biod,{
    readyLayer <- FALSE
    polDraw <- input$biodLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_biod  == 'Draw') |
        (!is.null(shp) & input$aoi_biod  == 'Layer')) {
      print('biod0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_biod  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_biod  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      dfsp <- data.frame(sp = 0)
      output$tablespbiod <- renderDataTable({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'species',
                                 sour = 'biomod', spFormat = 'list', 
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        # print(getwd()); metName <- 'metbiomod'; save(biotKm2, biotMetric, file=metName) # load(metName)
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            print('plots biod')
            dfsp <- biotMetric$result
          } else {
            dfsp <- data.frame(error = 'error')
          }
        } else if(class(biotMetric)[1] == 'simpleError'){
          dfsp <- data.frame(error = 'error')
        }
        xls_biod <<- dfsp
        dfsp
      })
    }
  }) ## ending go button
  
  
  ##### Go buttons rec  ----------------------  
  
  observeEvent(input$go_rec,{
    readyLayer <- FALSE
    polDraw <- input$recLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_rec  == 'Draw') |
        (!is.null(shp) & input$aoi_rec  == 'Layer')) {
      print('rec0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_rec  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_rec  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
      }
      
      dfsp <- data.frame(sp = 0)
      output$tablesprec <- renderDataTable({
        biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                              endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
        print(sprintf('Area: %s',biotKm2$polsizekm2))
        
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'species',
                                 sour = 'records', spFormat = 'list', 
                                 pol = gwkt, printURL = TRUE)
        #print(getwd()); metName <- 'metrecords.RData'; save(biotKm2, biotMetric, gwkt, file=metName) 
        # load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            print('plots recs')
            dfsp <- biotMetric$result
          } else {
            dfsp <- data.frame(error = 'error')
          }
        } else if(class(biotMetric)[1] == 'simpleError'){
          dfsp <- data.frame(error = 'error')
        }
        xls_recs <<- dfsp[1:(min(5000, nrow(dfsp))), ]
        dfsp
        
      })
    }
  }) ## ending go button
  
  
  
  ##### Go buttons sur  ----------------------  
  
  observeEvent(input$go_sur,{
    readyLayer <- FALSE
    polDraw <- input$surLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_sur  == 'Draw') |
        (!is.null(shp) & input$aoi_sur  == 'Layer')) {
      print('sur0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_sur  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_sur  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE) 
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$surPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'surface',
                                 cellSize = input$sur_size, 
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        #print(getwd()); metName <- 'metsurf.RData'; save(biotKm2, biotMetric, gwkt, file=metName) 
        #load(metName)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'list'){
            
            print('plots sur')
            
            hc1 <<- data.frame(var = 'Surface', per = biotMetric$result$percentage) %>%
              hchart('column', hcaes(x = var, y = per), 'Surface with information')  %>%
              hc_xAxis(title = list(text = 'Surface with information')) %>% 
              hc_yAxis(title = list(text = '% area')) %>%
              hc_exporting(enabled = TRUE) %>% hc_yAxis(max = 100)
            
            
            
          } else if(length(biotMetric$result) == 0){
            hc1 <<-  hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
    }
  }) ## ending go button
  
  
  ##### Go buttons rli  ----------------------  
  
  observeEvent(input$go_rli,{
    readyLayer <- FALSE
    
    output$rliPlot1 <- renderHighchart({
      biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                               endpoint = 'biotablero', metric = 'rli',
                               printURL = TRUE, rasterLayer = TRUE)
      
      if(class(biotMetric)[1] == 'list'){
        if(names(biotMetric$result) %in% 'redListIndex'){
          
          print('plots rli')
          rlidf <- reshape2::melt(biotMetric$result$redListIndex)
          rlidf$variable <- as.numeric(as.character(rlidf$variable))
          rlidf <- rlidf[order(rlidf$variable), ]
          hc1 <<- rlidf %>%
            hchart('line', hcaes(x = variable, y = value, color = 'Grupo', group = 'Grupo', fill = 'Grupo'), 'RLI')  %>% 
            hc_xAxis(title = list(text = 'Red list index')) %>% hc_yAxis(title = list(text = 'Km2'))  %>%
            hc_exporting(enabled = TRUE) 
          
        } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
          hc1  <<- hcNULL 
        }
      } else if (class(biotMetric)[1] == 'simpleError'){
        hc1 <<-  hcBIG %>%
          hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                 'Error:', unlist(biotMetric[1]), ''))
      }
      hc1
    }) 
  }) ## ending go button
  
  
  ##### Go buttons XXX  ----------------------  
  
  observeEvent(input$go_XXX,{
    readyLayer <- FALSE
    polDraw <- input$XXXLeaf_draw_new_feature
    if( (!is.null(polDraw) & input$aoi_XXX  == 'Draw') |
        (!is.null(shp) & input$aoi_XXX  == 'Layer')) {
      print('XXX0')
      #Valid option and layer
      if(!is.null(polDraw) & input$aoi_XXX  == 'Draw'){
        coordMat <- do.call(rbind, polDraw$geometry$coordinates[[1]])
        gwkt <- gsub( ' ', '%20',paste0('POLYGON((', paste(apply(coordMat, 1, paste, collapse = ' '), collapse = ','), '))'))
        
      } else if (!is.null(shp) & input$aoi_XXX  == 'Layer'){ 
        gwkt <- gsub( ' ', '%20',writeWKT(shp, byid = F))
        
      }
      
      biotKm2 <- biotablero(server = 'web', webURL = aws, port = aws_port,
                            endpoint = 'polsizekm2', pol = gwkt, printURL = TRUE)
      print(sprintf('Area: %s',biotKm2$polsizekm2))
      
      output$XXXPlot1 <- renderHighchart({
        biotMetric <- biotablero(server = 'web', webURL = aws, port = aws_port,
                                 endpoint = 'biotablero', metric = 'XXX',
                                 pol = gwkt, printURL = TRUE, rasterLayer = TRUE)
        
        if(class(biotMetric)[1] == 'list'){
          if(class(biotMetric$result) == 'data.frame'){
            
            print('plots XXX')
            
            hc1 <<- biotMetric$result %>%
              hchart('column', hcaes(x = name, y = km2), 'Bosque seco')  %>% 
              hc_xAxis(title = list(text = 'Bosque seco')) %>% hc_yAxis(title = list(text = 'Km2'))
            
            
            pieCh <- data.frame(Km2 = c(sum(biotMetric$result$km2), 
                                        biotKm2$polsizekm2 - sum(biotMetric$result$km2)), 
                                label = c('Bosque seco', 'No Bosque'))
            
            pieCh$porc <- round(pieCh$Km2/sum(pieCh$Km2)*100, 2)
            pieCh$label2 <- paste0(pieCh$label, ' (', pieCh$porc, '%)')
            
            hc2 <<- hchart(pieCh, "pie", hcaes(x = label2, y = Km2), name = "Km2") %>%
              hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2'))
            
          } else if(class(biotMetric$result) == 'integer' | length(biotMetric$result) == 0){
            hc1 <<- hc2 <<- hcNULL 
          }
        } else if (class(biotMetric)[1] == 'simpleError'){
          hc1 <<- hc2 <<- hcBIG %>%
            hc_title(text = paste( 'Total area:', biotKm2$polsizekm2, ' km2\n', 
                                   'Error:', unlist(biotMetric[1]), ''))
        }
        hc1
      }) 
      
      output$XXXPlot2 <- renderHighchart({hc2})
    }
  }) ## ending go button
  
  
  output$uicn_xls <- downloadHandler(
    filename = 'spListOriginal.csv', 
    content = function(file) {
      write.csv(xls_uicn, file, row.names = FALSE)
    })
  
  output$biod_xls <- downloadHandler(
    filename = 'spListOriginal.csv', 
    content = function(file) {
      write.csv(xls_biod,  file, row.names = FALSE)
    })
  
  output$recs_xls <- downloadHandler(
    filename = 'spListOriginal.csv', 
    content = function(file) {
      write.csv(xls_recs, file, row.names = FALSE)
      #xlsx::write.xlsx
    })
  
} # End server

# setwd('C:/GoogleDrive/IAvH/V2/Others/polyg_draw/app8_upload/')
# http://ec2-3-15-208-233.us-east-2.compute.amazonaws.com:8080/polsizekm2?pol=POLYGON((-74.133545 4.144818,-73.817139 3.741479,-74.572998 3.390597,-74.133545 4.144818))
shinyApp(ui, server)
