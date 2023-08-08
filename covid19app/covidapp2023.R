library(xts)
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(ggplot2)
library(ggthemes)
library(shinycssloaders)
library(tidyverse)
library(lubridate)
library(gifski)
library(scales)
library(leaflet)
library(countrycode)
library(rgdal)
library(zoo)
library(plotly)
library(forecast)
library(summaryBox)
library(remotes)
library(bslib)
library(bsplus)
#crna #131515
#bela #FFFAFB
#svetla #7DE2D1
#temna #339989
#skori crna #2B2C28
# #1e5c52


theme2 <- bslib::bs_theme(version = 4)
#download data----
initial_data <- readr::read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
basic_data <- initial_data[, c(2:6,8,9,11,12,14,15,37,39,41,42)] #extract basics
basic_data2 <- initial_data[, c(2:6,8,9,11,12,14,15,37,39,41,42, 63, 34, 50)]
non_countries <- c("Asia", "Europe", "North America", "South America", "Oceania", "Africa", "World", "High income", "European Union", "Low income","Upper middle income","Lower middle income")
`%!in%` = Negate(`%in%`)
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
unzip("world_shape_file.zip")
 # dat <- iso3166[,c("a3", "ISOname")] 
 # dat2 <- dat
 # dat <- rename(dat, "location" = ISOname)
 # dat <- rename(dat, "ISO3" = a3)
 # dat2 <- rename(dat2, "country" = ISOname)
 # dat2 <- rename(dat2, "ISO3" = a3)

drzave <- unique(  basic_data[, 2])

non_countries2 <- c("Asia", "Europe", "North America", "South America", "Oceania", "Africa", "World", "High income", "European Union", "Low income","Upper middle income","Lower middle income",
                   "England", "International", "Kosovo","Micronesia (country)", "Northern Ireland", "Scotland", "Timor", "Wales")
drzave <- drzave%>%filter(location %!in% non_countries2)
drzave$iso <- countrycode(drzave$location, "country.name","iso3c")

continents <- c("Asia", "Europe", "North America", "South America", "Oceania", "Africa")
dat <- drzave
dat2 <- drzave
dat <- rename(dat, "ISO3" = iso)
dat2 <- rename(dat2, "country" = location)
dat2 <- rename(dat2, "ISO3" = iso)

#group by day and continent, new cases and deaths only
continent_day <-basic_data %>% group_by(continent,date)%>% summarise(
  new_cases = sum(new_cases),
  new_deaths = sum(new_deaths)
)
#basic data for every day and country
#countryday_data <- initial_data[, c(3,4,6,9,12,15)]
countryday_data <- initial_data[, c(3,4,6,9,12,15)]

metric_choices <- colnames(basic_data)[c(5,7,9,11,13)]
metric_names <- gsub("_", " ", metric_choices)
metric_names <- paste0(toupper(substr(metric_names,1,1)), substr(metric_names, 2, nchar(metric_names)))

metric_list <- as.list(metric_choices)
names(metric_list) <- metric_names

name_fix <- function(x){
  s1 <- gsub("_", " ", x)
  s2 <- paste0(toupper(substr(s1,1,1)), substr(s1, 2, nchar(s1)))
  return (s2)
}



ui <- 
  navbarPage(
    
    icon("virus-covid"),
    header = tagList(
      useShinydashboard()
    ),
    
    id="selector",
    #homepage----
    tabPanel("Homepage",
             withSpinner(
             fluidPage( 
               
               fluidRow(
                 valueBoxOutput("valuebox_cases"),
                 valueBoxOutput("valuebox_deaths"),
                 valueBoxOutput("valuebox_vacc"),
               ),
               br(),
               fluidRow(
                 column(
                   width=4,class = "leaderboard",
                   box(width=12,
                   
                   #fluidRow("Leaderboard", width=12, class="lead"),
                   fluidRow(
                     infoBoxOutput("leader_1", width = 12)),
                   fluidRow(
                     infoBoxOutput("leader_2", width=12)),
                   fluidRow(
                     
                       infoBoxOutput("leader_3", width = 12)),
                   fluidRow(
                     
                       infoBoxOutput("leader_4", width = 12)),
                 )),
                 column(
                   width = 8, 
                   class = "plotout",
                   fluidRow(
                     tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; border-top: none;-moz-box-shadow: none;box-shadow: none;}'))),
                     box(
                       width = 12, 
                       class="mainplot",
                       div(class="firstplot", plotlyOutput("home_plot2")
                           #%>% withSpinner(color="#7DE2D1", type = 7)
                           )
                      )
                   )
                 )
                 
               ),
               br(),
                
               fluidRow(
                 column(
                   width = 12,
                   box(width=12, class="homeplot_d", plotOutput("home_hist"))
                 )
                 )
             ),color = "#7DE2D1", type = 7)),
    #data by country----
    navbarMenu("Country data",
               tabPanel("Data by country",
                        fluidPage( 
                        
                        
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            width = 3,
                            uiOutput("metric_country_card"), br(),
                            uiOutput("country_card"), br(),
                            uiOutput("daterange_card"),br(),
                            uiOutput("moving_av_country2"),
                            uiOutput("moving_av_days_country2")
                            
                            
                          ),
                          mainPanel(
                            width = 9,
                            div(class ="forecast_panel",plotlyOutput("single_country_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                            br(),
                            uiOutput("forecast_panel3")
                            
                            
                          )),
                        fluidRow(
                          
                          column(
                            width=12,
                            fluidRow( 
                              uiOutput("countryboxes"),
                              class="podatki"
                              
                            )),
                          
                        ),
                        br(),
                        
                        )),
               tabPanel("Compare Countries",
                        fluidPage(
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("metric_country"), br(),
                              uiOutput("country_country"), br(),
                              uiOutput("daterange_country"),br(),
                              uiOutput("moving_av_country"),br(),
                              uiOutput("moving_av_days_country"),br(),
                            ),
                            mainPanel(
                              div(class ="forecast_panel",plotlyOutput("country_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                              br(),
                              uiOutput("forecast_panel")
                            ))
                        
                          
                        ))
    ),
    navbarMenu("Continent data",
               tabPanel("Data by continent",
                        fluidPage( 
                          
                          
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              width = 3,
                              uiOutput("metric_continent_card"), br(),
                              uiOutput("continent_card"), br(),
                              uiOutput("daterange_card_continent"),br(),
                              uiOutput("moving_av_country3"),
                              uiOutput("moving_av_days_country3")
                              
                            ),
                            mainPanel(
                              width = 9,
                              div(class ="forecast_panel",plotlyOutput("single_continent_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                              br(),
                              uiOutput("forecast_panel4")
                              
                            )),
                          fluidRow(
                            
                            column(
                              width=12,
                              fluidRow( 
                                uiOutput("continentboxes"),
                                class="podatki"
                                
                              )),
                            
                          ),
                          br(),
                          
                        )),
               tabPanel("Compare Continents",
                        fluidPage(
                          br(),
                          sidebarLayout(
                            sidebarPanel(
                              uiOutput("metric_continent"), br(),
                              uiOutput("country_continent"), br(),
                              uiOutput("daterange_continent"),br(),
                              uiOutput("moving_av_continent"),br(),
                              uiOutput("moving_av_days_continent"),br(),
                            ),
                            mainPanel(
                              div(class ="forecast_panel",plotlyOutput("continent_plot")%>% withSpinner(color="#7DE2D1", type = 7)),
                              br(),
                              uiOutput("forecast_panel2")
                            ))
                          
                        ))
    ),
    #map----
    tabPanel("Map",
             fluidPage( 
               
               fluidRow(
                 class="input-row",
                   #checkboxInput(inputId = "cgf", label = "Metrics per million")),
                 column( 
                   width=4,
                   uiOutput("map_metrics")),
                 column( 
                   width=4,
                   uiOutput("map_date")
                 ),
                 column( 
                   width=4,
                   uiOutput("checkbox_map"))
                 ),
               br(),
               #fluidRow(class="plot4",plotOutput("plot4"))
               fluidRow(class = "map1",leafletOutput("world_map")%>% withSpinner(color="#7DE2D1", type = 7))  
             )),
    
    
    tabPanel("Data",
             uiOutput("downloadButton"),
             br(),
             fluidRow(
               column( width=12, class = "data",
                       box(title = "Data",
                           
                           status = "primary",
                           solidHeader = F,
                           collapsible = T,
                           width = 12,
                           #column(12, align="center", tableOutput('top5')))
                           dataTableOutput ("countrytable")), align="center"
               ))),
    tabPanel("About",
             h3("About the project"),
             p("This application was made as a part of final thesis for a Bachelor's degree in Computer Science."),
             p("The web application is built using R and RShiny package, and it interactively displays data of the entire Covid-19 pandemic."),
             br(),
             p("Author: Lara Komel"), 
             p('Mentor: Prof. Dr. Uroš Godnov'),br(),
             p("School: University of Primorska, The Faculty of Mathematics, Natural Sciences and Information Technologies (UP FAMNIT)"),br(),
             p("April 2023"),
             tags$img(src = "https://www.famnit.upr.si/sl/resources/images/studenti/alumni-famnit/logofamnit-02.png", width = "400px")
             
             
             ),
    
    #tags$head(
    #tags$link(rel = "stylesheet", type = "text/css", href = "appboxs.css")
    #),
    #css----
    
    tags$style(HTML(
      
      "body { background-color: #F3F3F3; }",
      ".firstplot {background-color: #ffffff; padding:20px;}",
      "#home_hist {margin-bottom: 70px; margin-top: -20px;}",
      ".forecast-bttn {padding-top: 65px;}",
      ".box {background-color: #F3F3F3;}",
      ".info-box-text {font-size: 17px}",
      ".forecast_panel {margin-left:80px, padding-left:80px}",
      ".well {background-color: #ffffff;}",
      ".item.active {color:#339989; background-color:#339989 !important;}",
      #".paginate-button.active > li > a {color:#339989; background-color:#339989 !important;}",
      "#moving_av_button_country {color:#FEFCFD; background-color:#1e5c52!important;}",
      ".btn.btn-default.action-button.btn.btn-sm.btn-primary.shiny-bound-input {color:#FEFCFD; background-color:#2B2C28!important;}",
      "#forecast_button > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#forecast_button4 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#remove_forecast_button4 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#forecast_button3 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#remove_forecast_button3 > button {color:#FEFCFD; background-color:#1e5c52!important;}",
      "#world_map {height: calc(100vh - 160px) !important;}",
      #"body { background-color: #2B2C28; }",
      ".input-row { background-color: #FEFCFD; padding:3px; border-radius: 5px; }",
      ".lead {padding:5px; font-family: Arial; font-size: 18px; color:#FFFAFB}",
      ".info-box {border-radius: 5px; }",
      ".info-box-icon.bg-light-blue{border-radius: 5px 0 0 5px ;}",
      ".shiny-html-output col-sm-4 shiny-bound-output{width:100%;border-radius: 5px; }",
      #".box-body {background-color: #2B2C28 !important;}",
      #".leaderboard { border: solid 3px #339989;  }",
      ".data { background-color: #FFFAFB; }",
       ".small-box.bg-red { background-color: #1e5c52 !important; border-radius: 8px; }",
       ".small-box.bg-green { background-color: #339989 !important; border-radius: 8px; }",
      ".small-box.bg-light-blue { background-color: #7DE2D1 !important; border-radius: 8px; }",
      ".info-box-icon.bg-light-blue { background-color: #339989 !important;}",
      ".navbar { background-color: #FFFAFB;
      
                           font-family: Copperplate;
                           font-size: 18px;
                           color: #2B2C28;
                           }",
      
      ".plot4 {width: 800px; margin:auto;}",
      ".paginate_button .active >a {background-color:black;}",
      ".dropdown-menu > .active > a {background-color:#7DE2D1 !important;}",
      ".dropdown-menu > .active > a:hover {background-color:#7DE2D1;}",
      ".navbar-default .navbar-nav > li > a {color:#2B2C28;}",
      ".navbar-default .navbar-nav > li > a:hover {color:#7DE2D1; }",
      ".navbar-default .navbar-nav > .active > a:hover {color:#7DE2D1;}",    
      ".navbar-default .navbar-nav > .active > a {color:#339989; background-color: #FFFAFB;}",
      ".navbar-default .navbar-nav > .active > a:focus {color:#339989; background-color: #FFFAFB;}",
      ".navbar-header {text-align: right;}",
      ".active > a:hover {color:red; background-color:red}",
      ".navbar-default .navbar-brand {color: #339989; font-family: Copperplate; font-size: 35px; text-align: center; padding: 8px; margin-left: 20px;}",
      ".homeplot_d { border-radius: 8px;  }",
      ".mainplot {border-radius: 8px; }",
      ".btn3 {background-color: #1e5c52 !important; width:100%; color:white; border-radius: 12px;}",
      ".countbpx {background-color: #1e5c52 !important;  color:white; border-radius: 12px; font-size: 20px}",
      ".box11 {border: 3px solid #1e5c52 !important; border-radius: 12px; display: flex; justify-content: center; align-items: center;}",
      ".countryinput {background-color: white; }",
      ".hi {background-color: white!important; border-radius: 0 0 8px 8px;  height: 400px !important;}",
      ".box.box-solid.bg-black {background-color: #339989 !important;border-radius: 8px; }",
      ".blabla {background-color: #339989 !important; margin:10px; padding:10px; height:40px;border-radius: 5px; justify-content: center; align-items: center; text-align:center; }",
      ".blabla2 {background-color: #339989 !important; margin:10px; padding:10px; height:70px;border-radius: 5px; justify-content: center; align-items: center; text-align:center; height:120px; color: white;}",
      ".stolpec {text-align:left; } ",
      
      
      
      ".podatki {margin:10px; padding:10px; height:70px;border-radius: 5px; justify-content: center; align-items: center; text-align:center;}",
      "#italy > .col-sm-4 > .small-box.bg-yellow > .inner > h3 {font-size: 18px;}",
      "#italy > .col-sm-4 > .small-box.bg-blue > .inner > h3 {font-size: 18px;}",
      "#italy > .col-sm-4 > .small-box.bg-navy > .inner > h3 {font-size: 18px;}",
      
      "#italy > .col-sm-4 > .small-box.bg-yellow > .icon-large > .fas.fa-virus-covid {font-size: 40px; color: #1e5c52 !important; opacity: 0.3;}",
      "#italy > .col-sm-4 > .small-box.bg-blue > .icon-large > .fas.fa-virus-covid {font-size: 40px; color: #339989 !important; opacity: 0.3;}",
      "#italy > .col-sm-4 > .small-box.bg-navy > .icon-large > .fas.fa-virus-covid {font-size: 40px; color: #7DE2D1 !important; opacity: 0.3;}",
      ".box-body.hi.countbox {color: #2B2C28; font-size: 16px; font-weight: bold;}",
      ".box-title {color: white; font-size: 20px; font-weight: bold;}",
      ".small-box.bg-yellow { height: 72px; color: #1e5c52 !important; border-radius: 8px; background-color: #ffffff !important; font-weight: bold !important;}",
      ".small-box.bg-blue {height: 72px; color: #339989 !important; border-radius: 8px; background-color: #ffffff !important; font-weight: bold !important;}",
      ".small-box.bg-navy { height: 72px; color: #7DE2D1 !important; border-radius: 8px; background-color: #ffffff !important; font-weight: bold !important;}",
      
      
      
      
      
      
    )),
  
  )

server <- function(input, output) {
  
  
  ##delete
  Stage = c("Survey", "Work Sample", "Interview", "Stats Test")
  Score = c("+33.7%", "+14.8%", "+20.8%", "+28.17%")
  no1_cand = data.frame(Stage, Score)
  Score =c("+37.1%", "+14.2%", "+19.3%", "+26.3%")
  no2_cand = data.frame(Stage, Score)
  Score = c("+33.1%", "+22.2%", "+17.3%", "+25.8%")
  no3_cand = data.frame(Stage, Score)
  Score = c("+29.1%", "+17.2%", "+15.3%", "+23.3%")
  no4_cand = data.frame(Stage, Score)
  Score = c("+22.1%", "+12.5%", "+11.4%", "+19.5%")
  no5_cand = data.frame(Stage, Score)
  
  top5_data <- data.frame(no1_cand, no2_cand, no3_cand, no4_cand)
  output$top5 = renderTable({
    top5_data  
  })
  output$top52 = renderTable({
    top5_data  
  })
  output$plot <- renderPlot({
    ggplot(data = diamonds, aes(x = cut, fill = cut)) +
      
      geom_bar( alpha = 0.8) +
      theme_solarized_2(light=FALSE)+
      theme(
        text = element_text(color="white"),
        title = element_text(color = "white"),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(fill = "#1e5c52", color = NA),
        panel.border = element_rect(fill = NA, color = "#1e5c52"), 
        legend.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5)
        #legend.position = "bottom"
      )+
      scale_color_brewer(palette = "Pastel1")
    
  })
  
  
  output$plot2 <- renderPlot({
    ggplot(data = diamonds, aes(x=cut, fill = cut)) +
      geom_bar(alpha = 0.8)
    
  })
  output$plot22 <- renderPlot({
    ggplot(data = diamonds, aes(x=cut, fill = cut)) +
      geom_bar(alpha = 0.8)
    
  })
  output$plot3 <- renderPlot({
    ggplot(data = diamonds, aes(x=cut, fill = cut)) +
      geom_bar(alpha = 0.8)
    
  })
  output$plot4 <- renderPlot({
    ggplot(data = diamonds, aes(x=cut, fill = cut)) +
      geom_bar(alpha = 0.8)
    
  })
  output$plot5 <- renderPlot({
    ggplot(data = diamonds, aes(x=cut, fill = cut)) +
      geom_bar(alpha = 0.8)
    
  })
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = 0, lat = 0, zoom = 2)
  })
  ##delete-
  #data clean-up ----
  #homepage plot data----
  data_home_plot0 <- reactive({x1<-basic_data %>%
    filter(location %!in% non_countries) %>% 
    select(location, date, new_cases )%>%
    setNames(c("location","date", "cases"))})
  data_home_plot_cleanup <- function(dat1){
    dat1$date <- as.Date( dat1$date)
    dat1$Month_Yr <- format(as.Date(dat1$date), "%Y-%m")
    dat1$location<- NULL
    dat1$date<- NULL
    
    return(dat1)
  }
  data_home_plot <- reactive({x2<- data_home_plot_cleanup(data_home_plot0()) %>% drop_na(cases)%>%
    group_by(Month_Yr) %>% 
    summarise(num = n(),
              total_c = sum(cases)) })
  
  homeplot_fixdate <- function(dat2){
    dat2$neki <- as.Date(as.yearmon(dat2$Month_Yr))
    return(dat2)
  }
  
  homepage_plot <- function(){
    graph3 = homeplot_fixdate(data_home_plot()) %>%
      ggplot(aes(x=neki, y=total_c)) +
      geom_line(size = 2, alpha = 0.75, col = "#339989")+
      labs(title = "Total world cases by month",
           x= NULL,
           y = NULL)+ 
      scale_y_continuous(labels = label_comma())+
      transition_reveal(neki)
    gif<- animate(graph3, renderer = gifski_renderer())
    #return(animate(graph3, renderer = gifski_renderer()))
    gif_file <- "animated_plot.gif"
    anim_save(gif_file, gif)
  }
  
  homepage_plot_merge <- function(){
    data3 <- homeplot_deaths_fixdate(data_home_plot_vacc())
    data2 <- homeplot_deaths_fixdate(data_home_plot_deaths())
    data1 <- homeplot_fixdate(data_home_plot())
    
    p <- plot_ly() %>%
      add_trace(x = ~data1$neki, y = ~data1$total_c, type = 'scatter', mode = 'lines', name = "World cases", line = list(color = '#131515', width = 2)) %>%
      add_trace(x = ~data2$neki, y = ~data2$total_d, type = 'scatter', mode = 'lines', name = "World deaths", line = list(color = '#339989', width = 2)) %>%
      add_trace(x = ~data3$neki, y = ~data3$total_v, type = 'scatter', mode = 'lines', name = "World vaccinations", line = list(color = '#2B2C28', width = 2)) %>%
      layout(
             #title = "Total world cases by month",
             xaxis = list(title = ""),
             yaxis = list(title = "Total cases"),
             legend = list(title = "Legend"),
             yaxis = list(tickformat = ","))
    
    return(p)
  }
  
  homepage_hist <- function(){
    data <- leaderboard_data()%>%head(30)
    #as.numeric((leaderboard_data()[1:20,])[3])
    options(scipen = 999)
    bar <- barplot(
      data$cases, 
      #names.arg = data$location, 
      col = "#2B2C28", 
      border = "black",
      ylab = "Cases", 
      main = "Histogram by Country")
    
    modified_labels <- sapply(strsplit(data$location, " "), function(x) {
      if (length(x) > 1) {
        paste0(substr(x[1], 1, 1), ". ", paste(x[-1], collapse = " "))
      } else {
        x
      }
    })
    
    text(x = bar, 
         y = par("usr")[3] - 0.02 * (par("usr")[4] - par("usr")[3]),
         labels = modified_labels, 
         srt = 90, 
         adj = c(1, 0.5), 
         xpd = TRUE)
    
    return(bar)
  }
  output$home_hist <- renderPlot({
    #homepage_plot2()
    homepage_hist()
  })
  
  homepage_plot_merge2 <- function(){
    data3 <- homeplot_deaths_fixdate(data_home_plot_vacc());
    data2 <- homeplot_deaths_fixdate(data_home_plot_deaths());
    
    
    
    homeplot_fixdate(data_home_plot()) %>%
      ggplot() +
      geom_line(aes(x = neki, y = total_c), size = 2, alpha = 0.75, col = "#339989") +
      geom_line(data = data2, aes(x = neki, y = total_d), size = 2, alpha = 0.75, col = "#FF0000") +
      geom_line(data = data3, aes(x = neki, y = total_v), size = 2, alpha = 0.75, col = "#0000FF") +
      labs(title = "Total world cases by month",
           x= NULL,
           y = "Total cases")+ 
      scale_y_continuous(labels = label_comma())+
      scale_color_manual(values = c("#339989", "#FF0000", "#0000FF"),
                         labels = c("World cases", "World deaths", "World vaccinations")) +
      guides(color = guide_legend(title = "Legend"))
  }
  homepage_plot2 <- function(){
    homeplot_fixdate(data_home_plot()) %>%
      ggplot(aes(x=neki, y=total_c)) +
      geom_line(size = 2, alpha = 0.75, col = "#339989")+
      labs(title = "Total world cases by month",
           x= NULL,
           y = "Total cases")+ 
      scale_y_continuous(labels = label_comma())
  }
  output$home_plot2 <- renderPlotly({
    #homepage_plot2()
    homepage_plot_merge()
  })
  output$home_plot <- renderImage({
    homepage_plot()
    filename <- "animated_plot.gif"
    list(src = filename,
         contentType = "image/gif",
         width = "80%",
         height = "100%"
         )
  }, deleteFile = FALSE)
  
  
  #homepage small plots----
  data_home_plot0_deaths <- reactive({x1<-basic_data %>%
    filter(location %!in% non_countries) %>% 
    select(location, date, new_deaths )%>%
    setNames(c("location","date", "deaths"))})
  data_home_small_plot_cleanup <- function(dat1){ #zbrisi
    dat1$date <- as.Date( dat1$date)
    dat1$Month_Yr <- format(as.Date(dat1$date), "%Y-%m")
    dat1$location<- NULL
    dat1$date<- NULL
    
    return(dat1)
  }
  data_home_plot_deaths <- reactive({x2<- data_home_small_plot_cleanup(data_home_plot0_deaths()) %>% drop_na(deaths)%>%
    group_by(Month_Yr) %>% 
    summarise(num = n(),
              total_d = sum(deaths)) })
  
  homeplot_deaths_fixdate <- function(dat2){
    dat2$neki <- as.Date(as.yearmon(dat2$Month_Yr))
    return(dat2)
  }
  
  
  homepage_deaths_plot <- function(){
    homeplot_deaths_fixdate(data_home_plot_deaths()) %>%
      ggplot(aes(x=neki, y=total_d)) +
      geom_line(size = 2, alpha = 0.75, col = "#7DE2D1")+
      labs(title = "Total world deaths by month",
           x= NULL,
           y = "Total deaths")+ 
      scale_y_continuous(labels = label_comma())
  }
  output$home_plot2_deaths <- renderPlot({
    homepage_deaths_plot()
  })
  ####
  data_home_plot0_vaccine <- reactive({x1<-basic_data %>%
    filter(location %!in% non_countries) %>% 
    select(location, date, new_vaccinations )%>%
    setNames(c("location","date", "vaccines"))})
  
  data_home_plot_vacc <- reactive({x2<- data_home_small_plot_cleanup(data_home_plot0_vaccine()) %>% drop_na(vaccines)%>%
    group_by(Month_Yr) %>% 
    summarise(num = n(),
              total_v = sum(vaccines)) })
  
  homepage_vaccine_plot <- function(){
    homeplot_deaths_fixdate(data_home_plot_vacc()) %>%
      ggplot(aes(x=neki, y=total_v)) +
      geom_line(size = 2, alpha = 0.75, col = "#7DE2D1")+
      labs(title = "Total world vaccines given by month",
           x= NULL,
           y = "Total vaccinations")+ 
      scale_y_continuous(labels = label_comma())
  }
  output$home_plot2_vacc <- renderPlot({
    homepage_vaccine_plot()
  })
  
  #homepage boxes----
  home_boxes_data <- function(){
    total <-basic_data %>%filter(location %!in% non_countries) %>% 
      select( new_cases, new_deaths, new_vaccinations )%>%
      setNames(c( "cases", "deaths", "vaccinations"))
    
    total_cases <- sum((total%>%drop_na(cases))$cases)
    total_deaths <- sum((total%>%drop_na(deaths))$deaths)
    total_vacc <- sum((total%>%drop_na(vaccinations))$vaccinations)
    
    return(c(total_cases, total_deaths, total_vacc))
  }
  output$valuebox_cases <- renderValueBox({
    valueBox(
      format(home_boxes_data()[1], big.mark=","), "Total cases worldwide ", icon = icon("virus-covid"),
      color = "red"
    )
  })
  output$valuebox_deaths <- renderValueBox({
    valueBox(
      format(home_boxes_data()[2], big.mark=","), "Total deaths worldwide ", icon = icon("virus-covid"),
      color = "green"
    )
  })
  output$valuebox_vacc <- renderValueBox({
    valueBox(
      format(home_boxes_data()[3], big.mark=","), "Total vaccinations worldwide ", icon = icon("virus-covid"),
      color = "light-blue"
    )
  })
  
  #leaderboard----
  
  leaderboard_data <- function(){
    leaderboard <-basic_data %>%filter(location %!in% non_countries) %>% 
      select( location, new_cases, new_deaths, new_vaccinations )%>%
      setNames(c( "location","cases", "deaths", "vacc"))%>%group_by(location) %>% drop_na(c(cases, deaths, vacc))%>%
      summarise(num = n(),
                cases = sum(cases), deaths = sum(deaths), vacc=sum(vacc))
    leaderboard <- leaderboard[order(leaderboard$cases, decreasing = TRUE),]
    return(leaderboard)
  }
  output$leader_1 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data()[1,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data()[1,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data()[1,])[4]), big.mark=",")), icon = icon("1"),
      color = "light-blue",width = 12
    )
  })
  output$leader_2 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data()[2,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data()[2,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data()[2,])[4]), big.mark=",")), icon = icon("2"),
      color = "light-blue",width = 12
    )
  })
  output$leader_3 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data()[3,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data()[3,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data()[3,])[4]), big.mark=",")), icon = icon("3"),
      color = "light-blue",width = 12
    )
  })
  output$leader_4 <- renderInfoBox({
    infoBox(
      paste0(" ", (leaderboard_data()[4,])[1]), paste0("Total Cases: ", format(as.numeric((leaderboard_data()[4,])[3]), big.mark=",")), paste0("Total Deaths: ", format(as.numeric((leaderboard_data()[4,])[4]), big.mark=",")), icon = icon("4"),
      color = "light-blue",width = 12
    )
  }) 
  #data table ----
  clean_data_country <- reactive({cases_new <- basic_data %>% 
    filter(location %!in% non_countries) %>% 
    select(location, date, new_cases, new_deaths )%>%
    set_names(c("location", "date", "cases", "deaths"))%>%
    arrange(date)})
  clean_data_country_t <- reactive({cases_new <- basic_data %>% 
    filter(location %!in% non_countries) %>% 
    select(location, date, new_cases, new_deaths )%>%
    set_names(c("location", "date", "cases", "deaths"))%>%
    arrange(date)})
  
  output$countrytable <- renderDataTable(clean_data_country_t(), options = list(pageLength = 20))
  
  
  #map inputs----
  output$map_metrics <- renderUI({
    selectInput(
      inputId = "map_metric_i",
      label = ("Select metric"),
      choices = metric_list,
      selected = metric_list[1]
      
    )
  })
  output$map_date <- renderUI({
    dateInput(
      inputId = "map_date_i",
      label = "Select date",
      value ="2020-12-31"
    )
  })
  output$map_dates <- renderUI({
    dateRangeInput(
      inputId = "map_dates_i",
      label = "Select date range",
      start = "2020-01-01",
      end = "2020-12-31"
    )
    
  })
  output$checkbox_map <- renderUI({ 
    checkboxInput(
      inputId = "checkbox_map",
      label = div("Date range", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  #worldmap----
  clean_data_map<- reactive({xxx <- basic_data %>% 
    filter(date == input$map_date_i)%>%
    select(location, date, input$map_metric_i )%>%
    setNames(c("location", "date", "metric"))})
  
  clean_data_map_range<- reactive({xxx <- basic_data %>% 
    filter(date >= input$map_dates_i[1])%>%filter(date <= input$map_dates_i[2])%>%
    select(location, date, input$map_metric_i )%>%
    setNames(c("location", "date", "metric"))%>%
    drop_na(metric)%>%
    group_by(location) %>%
    summarize(metric = round(mean(metric)), digits=0)
    })
  
  worldmap <- function(){
    dataf = merge(x = clean_data_map(), y = dat, by = "location")
    world_spdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
    wrldx <- merge(world_spdf, dataf,  duplicateGeoms = T)
    #colnames(wrldx@data)
    wrldx$metric = as.numeric(as.character(wrldx$metric))
    
    bins = c(0, 0.5, 5, 15, 50, 100, 400, 1000, 2000, Inf)
    #"YlOrBr"
    pallmy = c("#a0ebdf","#75d1c3","#55b5a6","#339989","#237d6f","#16695c", "#0b423a", "#06332c","#011a16" )
    pal = colorBin(palette = pallmy, domain=wrldx$metric, na.color = "transparent", bins=bins)
    
    customLabel = paste("Country: ", wrldx$NAME, "<br/>", input$map_metric_i, ": ", wrldx$metric, sep = "") %>% 
      lapply(htmltools::HTML)
    
    wrldmap <- leaflet(wrldx) %>%
      addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
      addPolygons(fillColor = ~pal(metric),
                  fillOpacity = 0.9,
                  stroke = TRUE,
                  color="white",
                  highlight=highlightOptions(
                    weight = 5,
                    fillOpacity = 0.3
                  ),
                  label = customLabel,
                  weight = 0.3,
                  smoothFactor = 0.2)%>%
      addLegend(
        pal=pal,
        values = ~metric,
        position = "bottomright",
        title ="Covid-19 cases"
      )
    
    
    return(wrldmap)
  }
  
  observeEvent(input$checkbox_map, {
    if (!input$checkbox_map){
      output$map_date <- renderUI({
        dateInput(
          inputId = "map_date_i",
          label = "Select date",
          value ="2020-12-31"
        )
    })}
    else {
      output$map_date <- renderUI({
        dateRangeInput(
          inputId = "map_dates_i",
          label = "Select date range",
          start = "2020-01-01",
          end = "2020-12-31"
        )
      })
    }
  })
  
  output$world_map <- renderLeaflet({ 
    req(input$map_date_i)

    if (!input$checkbox_map){ 
    worldmap()
    }else{
      req(input$map_dates_i)
      worldmap_range()
    }
  })
  #worldmap date range
  worldmap_range <- function(){
    dataf = merge(x = clean_data_map_range(), y = dat, by = "location")
    world_spdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
    wrldx <- merge(world_spdf, dataf,  duplicateGeoms = T)
    #colnames(wrldx@data)
    wrldx$metric = as.numeric(as.character(wrldx$metric))
    
    bins = c(0, 0.5, 5, 15, 50, 100, 400, 1000, 2000, Inf)
    #"YlOrBr"
    pallmy = c("#a0ebdf","#75d1c3","#55b5a6","#339989","#237d6f","#16695c", "#0b423a", "#06332c","#011a16" )
    pal = colorBin(palette = pallmy, domain=wrldx$metric, na.color = "transparent", bins=bins)
    
    customLabel = paste("Country: ", wrldx$NAME, "<br/>", "Cases: ", wrldx$metric, sep = "") %>% 
      lapply(htmltools::HTML)
    
    wrldmap <- leaflet(wrldx) %>%
      addProviderTiles(providers$OpenStreetMap, options = tileOptions(minZoom=2, maxZoom=8)) %>%
      addPolygons(fillColor = ~pal(metric),
                  fillOpacity = 0.9,
                  stroke = TRUE,
                  color="white",
                  highlight=highlightOptions(
                    weight = 5,
                    fillOpacity = 0.3
                  ),
                  label = customLabel,
                  weight = 0.3,
                  smoothFactor = 0.2)%>%
      addLegend(
        pal=pal,
        values = ~metric,
        position = "bottomright",
        title ="Covid-19 cases"
      )
    
    
    return(wrldmap)
  }
  
  
  #data by country----
  output$country_country <- renderUI({ 
    selectInput(
      inputId = "country_country",
      multiple = TRUE,
      label = "Select one or multiple countries",
      choices = sort(unique((basic_data%>%filter(location %!in% non_countries2))$location)),
      selected = c("Slovenia", "Italy", "Croatia")
    )})
  
  output$metric_country <- renderUI({
    radioButtons(
      inputId = "metric_country",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_country <- renderUI({
    dateRangeInput(inputId = "daterange_country",
                   label = "Select date range",
                   start = "2020-01-01",
                   end = "2020-12-31")
  })
  output$moving_av_country <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$moving_av_country2 <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country2",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country2 <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country2",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country2",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$moving_av_country3 <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country3",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country3 <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country3",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country3",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$moving_av_country4 <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_country4",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_country4 <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_country4",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_country4",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$forecast_panel <- renderUI({
      #class= "jumbotron",
      div( 
        #class = "container bg-secondary",
        fluidRow( 
        h4("Forecast"),
        
        uiOutput("forecast"),
         div(
          div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button")),
          div(style="display: inline-block; width: 25px ;"),
          div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button"))))
      )
  })
  output$forecast_panel2 <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        h4("Forecast"),
        
        uiOutput("forecast2"),
        div(
          div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button2")),
          div(style="display: inline-block; width: 25px ;"),
          div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button2"))))
    )
  })
  output$forecast <- renderUI({
    numericInput(
      inputId = "forecast",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast_button <- renderUI({
    actionButton(inputId = "forecast_button",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button <- renderUI({
    actionButton(inputId = "remove_forecast_button",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  #country data card----
  output$countrybox <- renderUI({
    req(input$country_card)
    data <- func_card_info(input$country_card, input$daterange_card[1], input$daterange_card[2])
    data2 <- func_card_info2(input$country_card, input$daterange_card[1], input$daterange_card[2])
    box(
      class="hi",
      title = input$country_card, 
      background = "black",
      
      fluidRow(
        div(id="italy",
            valueBox(
              5846843, "Total cases ", icon = icon("virus-covid"),
              color = "yellow"
            )),
        div(id="italy",
            valueBox(
              5846, "Total deaths ", icon = icon("virus-covid"),
              color = "blue"
            )),
        div(id="italy",
            valueBox(
              5846843, "Total vaccinations ", icon = icon("virus-covid",),
              color = "navy"
            ))
      ),
      fluidRow(
        column(width=12,
               fluidRow( 
                 div(paste0( "Date range: ",input$daterange_card[1], " to ", input$daterange_card[2]), br(), br(),
                     column(width=6, paste0("Cases: ", data2[2]), br(), paste0("Deaths: ", data2[3]), class="stolpec" ),
                     column(width = 6, paste0("Vaccinations: ", data2[4]), br(), "Deaths: 534", class="stolpec"),
                     br(),
                     class="blabla2"),
               )),
      ),
      paste0( "Population: ", data[1]), br(), 
      paste0( "Mortality rate: ",data[5]), br(),
      paste0("First case: ", data2$date), br(),
      paste0("Population vaccinated: ", data[3]), br(),
      paste0( "People vaccinated: ",data[2]), br(),
      paste0("Avarage age: ", data[4]), br(),
      br(),
      br(),
      br(),
      width=12, class="countbox"
    )
  })
  
  #country data boxes top
  output$countryboxes <- renderUI({
    req(input$country_card)
    data2 <- func_card_info2(input$country_card, input$daterange_card[1], input$daterange_card[2])
    data <- leaderboard_data()%>%filter(location == input$country_card)
   
      
      fluidRow(
        div(id="italy",
            valueBox(
              paste0("Total cases: ", data[3]), paste0("In date-range: ", data2[2]), icon = icon("virus-covid"),
              color = "yellow"
            )),
        div(id="italy",
            valueBox(
              paste0("Total deaths: ", data[4]), paste0("In date-range: ", data2[3]), icon = icon("virus-covid"),
              color = "blue"
            )),
        div(id="italy",
            valueBox(
              paste0("Total vaccinations: ", data[5]), paste0("In date-range: ", data2[4]), icon = icon("virus-covid"),
              color = "navy"
            ))
    )
  })
  
  
  
  func_card_info <- function(country, date1, date2){
    count <-basic_data2 %>%filter(location == country) %>% 
      filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
    
    pop <- count%>%filter(date==as.Date(date2))
    pop <- (pop[1,])[16]
    
    people_vacc <- count%>%filter(date==as.Date(date2))
    people_vacc <- (people_vacc[1,])[12]
    perc_vacc <- ((100*people_vacc)/pop)
    med_age <- count%>%filter(date==as.Date(date2))
    med_age <- (med_age[1,])[18]
    
    
    total_c <- count%>%filter(date==as.Date(date2))
    total_c <- (total_c[1,])[4]
    total_d <- count%>%filter(date==as.Date(date2))
    total_d <- ((total_d[1,])[6])  
    
    fatality <- ((total_d/total_c)*100)
    
    rez <- c(pop, people_vacc, perc_vacc, med_age , fatality)
    return(rez)
  }
  
  func_card_info2 <- function(country, date1, date2){
    count <- (basic_data2 %>%filter(location == country)%>%drop_na(new_cases)%>%filter(new_cases != 0))
    first_case <- (count[1,])[3]
    data <- basic_data2 %>%filter(location == country)%>%filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
    
    cases <- data%>%drop_na(new_cases)%>%summarise(all_cases = sum(new_cases))
    deaths <- data%>%drop_na(new_deaths)%>%summarise(all_deaths = sum(new_deaths))
    vaccine <- data%>%drop_na(new_vaccinations)%>%summarise(all_vacc = sum(new_vaccinations))
    
    
    return(c(first_case, cases, deaths, vaccine))
  }
  
  
  card_data <- reactive({data <- func_card_info(input$country_card, input$date1_card, input$date2_card)})
  output$country_card <- renderUI({ 
    selectInput(
      inputId = "country_card",
      multiple = FALSE,
      label = "Select one country",
      choices = sort(unique((basic_data%>%filter(location %!in% non_countries2))$location)),
      selected = "Slovenia"
    )})
  
  output$metric_country_card <- renderUI({
    radioButtons(
      inputId = "metric_country_card",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_card <- renderUI({
    dateRangeInput(inputId = "daterange_card",
                   label = "Select date range",
                   start = "2020-01-01",
                   end = "2020-12-31")
  })
  #plot country----
  clean_data_country_card <- reactive({cases_new <- basic_data %>% 
    filter(location == input$country_card) %>% 
    filter(date >= input$daterange_card[1])%>%filter(date <= input$daterange_card[2])%>%
    select(location, date,input$metric_country_card )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)}) 
  make_forecast_c <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button3, {
    make_forecast_c$value <- 1
  })
  observeEvent(input$remove_forecast_button3, {
    make_forecast_c$value <- 0
  })
  make_forecast_c2 <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button4, {
    make_forecast_c2$value <- 1
  })
  observeEvent(input$remove_forecast_button4, {
    make_forecast_c2$value <- 0
  })
  
  plot_by_country_card <- function(){
    #dat_ma <- clean_data_country_card()
    ifelse(input$moving_av_country2 ==T & !is.null(input$moving_av_days_country2),
           dat_ma <- clean_data_country_card()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days3(),mean,align='right',fill=NA)),
           dat_ma <- clean_data_country_card()
    )
    
    
    plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                             line = list(color = 'black'),
                             hovertemplate = paste(
                               paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_country2==T & !is.null(input$moving_av_days_country2)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot", color='#7DE2D1'), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x}')) )
    }
    highlight(plt)
    
    
  }
  plot_by_country_card_forecast <- function(){
    #dat_ma <- clean_data_country_card()
    ifelse(input$moving_av_country2 ==T & !is.null(input$moving_av_days_country2),
           dat_ma <- clean_data_country_card()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days3(),mean,align='right',fill=NA)),
           dat_ma <- clean_data_country_card()
    )
    ifelse(input$moving_av_country2 ==T & !is.null(input$moving_av_days_country2),
           forecastdata <- forecast_data_c()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days3(),mean,align='right',fill=NA)),
           forecastdata <- forecast_data_c()
    )
    
    forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                              forecastdata%>%filter(forecast==1))
    
    plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
    #svetla #7DE2D1
    #temna #339989
    
    #plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                             line = list(color = 'black'),
                             hovertemplate = paste(
                               paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_country2==T & !is.null(input$moving_av_days_country2)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot", color="#7DE2D1"), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_country_card),': %{y}\nDate: %{x}')) )
    }
    plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color="#339989", showlegend = F,
                            type="scatter", mode="lines", line=list(color=~location, dash="dot"),
                            hovertemplate = paste(
                              paste0('<extra>Forecast</extra>Country Name: %{text}\n',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                            ))
    highlight(plt)
    
    
  }
  output$single_country_plot <- renderPlotly({
   req(input$country_card)
   #return(plot_by_country_card())
   ifelse(make_forecast_c$value==0, return (plot_by_country_card()), return(plot_by_country_card_forecast()))
    
  })
  
  #plot country compare----
  make_forecast <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button, {
    make_forecast$value <- 1
  })
  observeEvent(input$remove_forecast_button, {
    make_forecast$value <- 0
  })
  
  
  output$country_plot <- renderPlotly({
    req(input$country_country)
    ifelse(make_forecast$value==0, return (plot_by_country()), return(plot_data_country_forecast()))
    
  })
  clean_data_country <- reactive({cases_new <- basic_data %>% 
    filter(location %in% input$country_country) %>% 
    filter(date >= input$daterange_country[1])%>%filter(date <= input$daterange_country[2])%>%
    select(location, date,input$metric_country )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)})
  clean_data_country_c <- reactive({cases_new <- basic_data %>% 
    filter(location %in% input$country_card) %>% 
    filter(date >= input$daterange_card[1])%>%filter(date <= input$daterange_card[2])%>%
    select(location, date,input$metric_card )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)})
  
  forecast_data <- function(){
    unforecasted_data <- clean_data_country()
    unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
    forecasted_data <- predictions_by_country()
    forecasted_data <- do.call(rbind,forecasted_data)
    rbind(unforecasted_data,forecasted_data) 
  }
  forecast_data_c <- function(){
    unforecasted_data <- clean_data_country_card()
    unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
    forecasted_data <- predictions_by_country_c()
    forecasted_data <- do.call(rbind,forecasted_data)
    rbind(unforecasted_data,forecasted_data) 
  }
  forecast_data_c2 <- function(){
    unforecasted_data <- clean_data_continent_card()
    unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
    forecasted_data <- predictions_by_continent_c()
    forecasted_data <- do.call(rbind,forecasted_data)
    rbind(unforecasted_data,forecasted_data) 
  }
  create_forecast <- function(dat, num_forecasts){
    name_country <- unique(dat$location)
    auto_forecast <-  forecast(auto.arima(dat$metric),num_forecasts)$mean
    max_date <- max(dat$date)
    new_dates <- max_date + c(1:num_forecasts)
    new_forecast <- tibble( location = name_country, date = new_dates , metric = as.vector(auto_forecast), forecast = 1 )
    return(new_forecast)
  }
  
  predictions_by_country <- function(){
    clean_data_country() %>%
      group_by(location) %>%
      group_map(~ create_forecast(.x, num_forecasts=input$forecast), .keep=T )
  }
  predictions_by_country_c <- function(){
    clean_data_country_card() %>%
      group_by(location) %>%
      group_map(~ create_forecast(.x, num_forecasts=input$forecast3), .keep=T )
  }
  
  ma_days <- eventReactive(input$moving_av_button_country,
                           {
                             req(input$moving_av_days_country)
                             input$moving_av_days_country
                           }, ignoreNULL = FALSE)
  ma_days3 <- eventReactive(input$moving_av_button_country2, #single country
                           {
                             req(input$moving_av_days_country2)
                             input$moving_av_days_country2
                           }, ignoreNULL = FALSE)
  ma_days4 <- eventReactive(input$moving_av_button_country3, #single country
                            {
                              req(input$moving_av_days_country3)
                              input$moving_av_days_country3
                            }, ignoreNULL = FALSE)
  
  plot_by_country <- function(){
    ifelse(input$moving_av_country ==T & !is.null(input$moving_av_days_country),
           dat_ma <- clean_data_country()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days(),mean,align='right',fill=NA)),
           dat_ma <- clean_data_country()
    )
    
    plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                             hovertemplate = paste(
                               paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_country==T & !is.null(input$moving_av_days_country)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_country),': %{y}\nDate: %{x}')) )
    }
    highlight(plt)
    
    
  }
  plot_data_country_forecast <- function(){
    
    ifelse(input$moving_av_country ==T & !is.null(input$moving_av_days_country),
           forecastdata <- forecast_data()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days(),mean,align='right',fill=NA)),
           forecastdata <- forecast_data()
    )
    
    forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                              forecastdata%>%filter(forecast==1))
    
    plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines+markers',
                             hovertemplate = paste(
                               paste0('<extra></extra>Country Name: %{text}\n',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_country==T & !is.null(input$moving_av_days_country)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Country Name: %{text}\n",name_fix(input$metric_country),': %{y}\nDate: %{x}')) )
    }
    plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color=~location, showlegend = F,
                            type="scatter", mode="lines", line=list(color=~location, dash="dot"),
                            hovertemplate = paste(
                              paste0('<extra>Forecast</extra>Country Name: %{text}\n',name_fix(input$metric_country),': %{y}\nDate: %{x} ')
                            ))
    highlight(plt)
  }
  #data by continent----
  output$country_continent <- renderUI({ 
    selectInput(
      inputId = "country_continent",
      multiple = TRUE,
      label = "Select one or multiple continent",
      choices = continents,
      selected = c("Europe", "Asia")
    )})
  
  output$metric_continent <- renderUI({
    radioButtons(
      inputId = "metric_continent",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_continent <- renderUI({
    dateRangeInput(inputId = "daterange_continent",
                   label = "Select date range",
                   start = "2020-01-01",
                   end = "2020-12-31")
  })
  output$moving_av_continent <- renderUI({ 
    checkboxInput(
      inputId = "moving_av_continent",
      label = div("Moving avarage", style = "font-size: 10pt"),
      value = FALSE
    )
  })
  output$moving_av_days_continent <- renderUI({
    div( 
      numericInput(
        inputId = "moving_av_days_continent",
        label = "Number of days for moving avarage",
        value= 5,
        min= 0,
        step = 1
      ),
      actionButton(
        inputId = "moving_av_button_continent",
        label = "Update",
        type = "default",
        class = "btn-sm"
      ))
  })
  output$forecast_panel3 <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        column(
        width = 4,
        h4("Forecast"),
        
        uiOutput("forecast3")),
        column(
          width = 4,
        div( class="forecast-bttn",
          div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button3")),
          div(style="display: inline-block; width: 25px ;"),
          div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button3"))))
    ))
  })
  output$forecast_panel4 <- renderUI({
    #class= "jumbotron",
    div( 
      #class = "container bg-secondary",
      fluidRow( 
        column(
          width = 4,
          h4("Forecast"),
          
          uiOutput("forecast4")),
        column(
          width = 4,
          div( class="forecast-bttn",
               div(style="display: inline-block; width: 95px ;", uiOutput("forecast_button4")),
               div(style="display: inline-block; width: 25px ;"),
               div(style="display: inline-block; width: 95px ;", uiOutput("remove_forecast_button4"))))
      ))
  })
  output$forecast2 <- renderUI({
    numericInput(
      inputId = "forecast2",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast3 <- renderUI({
    numericInput(
      inputId = "forecast3",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast4 <- renderUI({
    numericInput(
      inputId = "forecast4",
      label = "Number of days to forecast",
      value = 20,
      step = 1,
      min = 0, 
      max = 100
      
    )
  })
  output$forecast_button2 <- renderUI({
    actionButton(inputId = "forecast_button2",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button2 <- renderUI({
    actionButton(inputId = "remove_forecast_button2",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$forecast_button3 <- renderUI({
    actionButton(inputId = "forecast_button3",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button3 <- renderUI({
    actionButton(inputId = "remove_forecast_button3",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$forecast_button4 <- renderUI({
    actionButton(inputId = "forecast_button4",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Make a forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  output$remove_forecast_button4 <- renderUI({
    actionButton(inputId = "remove_forecast_button4",
                 #icon = icon("tree-deciduous", lib = "glyphicon"),
                 style = "color:white",
                 label = "Remove forecast",
                 class = "btn btn-sm btn-primary"
    )
  })
  #continent data card----
  output$continentbox <- renderUI({
    req(input$continent_card)
    data <- func_card_info_c(input$continent_card, input$daterange_card_continent[1], input$daterange_card_continent[2])
    data2 <- func_card_info2_c(input$continent_card, input$daterange_card_continent[1], input$daterange_card_continent[2])
    box(
      class="hi",
      title = input$continent_card, 
      background = "black",
      
      fluidRow(
        div(id="italy",
            valueBox(
              5846843, "Total cases ", icon = icon("virus-covid"),
              color = "yellow"
            )),
        div(id="italy",
            valueBox(
              5846, "Total deaths ", icon = icon("virus-covid"),
              color = "blue"
            )),
        div(id="italy",
            valueBox(
              5846843, "Total vaccinations ", icon = icon("virus-covid",),
              color = "navy"
            ))
      ),
      fluidRow(
        column(width=12,
               fluidRow( 
                 div(paste0( "Date range: ",input$daterange_card_continent[1], " to ", input$daterange_card_continent[2]), br(), br(),
                     column(width=6, paste0("Cases: ", data2[2]), br(), paste0("Deaths: ", data2[3]), class="stolpec" ),
                     column(width = 6, paste0("Vaccinations: ", data2[4]), br(), "Deaths: 534", class="stolpec"),
                     br(),
                     class="blabla2"),
               )),
      ),
      paste0( "Population: ", data[1]), br(), 
      paste0( "Mortality rate: ",data[5]), br(),
      paste0("First case: ", data2$date), br(),
      paste0("Population vaccinated: ", data[3]), br(),
      paste0( "People vaccinated: ",data[2]), br(),
      paste0("Avarage age: ", data[4]), br(),
      br(),
      br(),
      br(),
      width=12, class="countbox"
    )
  })
  output$continentboxes <- renderUI({
    req(input$continent_card)
    data <-basic_data2 %>%filter(location == input$continent_card) %>% 
      select( location, new_cases, new_deaths, new_vaccinations )%>%
      setNames(c( "location","cases", "deaths", "vacc"))%>%group_by(location) %>% drop_na(c(cases, deaths, vacc))%>%
      summarise(num = n(),
                cases = sum(cases), deaths = sum(deaths), vacc = sum(vacc))
    #leaderboard <- leaderboard[order(leaderboard$cases, decreasing = TRUE),]
    data2 <-basic_data2 %>%filter(location == input$continent_card) %>% filter(date >= as.Date(input$daterange_card_continent[1]))%>%filter(date <= as.Date(input$daterange_card_continent[2]))%>%
      select( location, new_cases, new_deaths, new_vaccinations )%>%
      setNames(c( "location","cases", "deaths", "vacc"))%>%group_by(location) %>% drop_na(c(cases, deaths, vacc))%>%
      summarise(num = n(),
                cases = sum(cases), deaths = sum(deaths), vacc = sum(vacc))
    data23 <- func_card_info2_c(input$continent_card, input$daterange_card_continent[1], input$daterange_card_continent[2])
    #data <- data%>%filter(location == input$continent_card)
    
    
    fluidRow(
      div(id="italy",
          valueBox(
            paste0("Total cases: ", data[3]), paste0("In date-range: ", data2[3]), icon = icon("virus-covid"),
            color = "yellow"
          )),
      div(id="italy",
          valueBox(
            paste0("Total deaths: ", data[4]), paste0("In date-range: ", data2[4]), icon = icon("virus-covid"),
            color = "blue"
          )),
      div(id="italy",
          valueBox(
            paste0("Total vaccinations: ", data[5]), paste0("In date-range: ", data2[5]), icon = icon("virus-covid"),
            color = "navy"
          ))
    )
  })
  
  
  
  func_card_info_c <- function(continent, date1, date2){
    count <-basic_data2 %>%filter(location == continent) %>% 
      filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
    
    pop <- count%>%filter(date==as.Date(date2))
    pop <- (pop[1,])[16]
    
    people_vacc <- count%>%filter(date==as.Date(date2))
    people_vacc <- (people_vacc[1,])[12]
    perc_vacc <- ((100*people_vacc)/pop)
    med_age <- count%>%filter(date==as.Date(date2))
    med_age <- (med_age[1,])[18]
    
    
    total_c <- count%>%filter(date==as.Date(date2))
    total_c <- (total_c[1,])[4]
    total_d <- count%>%filter(date==as.Date(date2))
    total_d <- ((total_d[1,])[6])  
    
    fatality <- ((total_d/total_c)*100)
    
    rez <- c(pop, people_vacc, perc_vacc, med_age , fatality)
    return(rez)
  }
  
  func_card_info2_c <- function(continent, date1, date2){
    count <- (basic_data2 %>%filter(location == continent)%>%drop_na(new_cases)%>%filter(new_cases != 0))
    first_case <- (count[1,])[3]
    data <- basic_data2 %>%filter(location == continent)%>%filter(date >= as.Date(date1))%>%filter(date <= as.Date(date2))
    
    cases <- data%>%drop_na(new_cases)%>%summarise(all_cases = sum(new_cases))
    deaths <- data%>%drop_na(new_deaths)%>%summarise(all_deaths = sum(new_deaths))
    vaccine <- data%>%drop_na(new_vaccinations)%>%summarise(all_vacc = sum(new_vaccinations))
    
    
    return(c(first_case, cases, deaths, vaccine))
  }
  
  
  card_data_continent <- reactive({data <- func_card_info_c(input$continent_card, input$date1_card, input$date2_card)})
  output$continent_card <- renderUI({ 
    selectInput(
      inputId = "continent_card",
      multiple = FALSE,
      label = "Select one continent",
      choices = continents,
      selected = "Europe"
    )})
  
  output$metric_continent_card <- renderUI({
    radioButtons(
      inputId = "metric_continent_card",
      label = "Select the metric",
      choices = metric_list)
  })
  output$daterange_card_continent <- renderUI({
    dateRangeInput(inputId = "daterange_card_continent",
                   label = "Select date range",
                   start = "2020-01-01",
                   end = "2020-12-31")
  })
  #plot continent----
  clean_data_continent_card <- reactive({cases_new <- basic_data %>% 
    filter(location == input$continent_card) %>% 
    filter(date >= input$daterange_card_continent[1])%>%filter(date <= input$daterange_card_continent[2])%>%
    select(location, date,input$metric_continent_card )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)}) 
  
  plot_by_continent_card <- function(){
    ifelse(input$moving_av_country3 ==T & !is.null(input$moving_av_days_country3),
           dat_ma <- clean_data_continent_card()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days4(),mean,align='right',fill=NA)),
           dat_ma <- clean_data_continent_card()
    )
    
    
    plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines', line = list(color = 'black'),
                             hovertemplate = paste(
                               paste0('<extra></extra>Continent Name: %{text}\n','Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_country3==T & !is.null(input$moving_av_days_country3)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot", color='#7DE2D1'), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Continent Name: %{text}\n",'Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x}')) )
    }
    highlight(plt)
    
    
  }
  plot_by_continent_card_forecast <- function(){
    ifelse(input$moving_av_country3 ==T & !is.null(input$moving_av_days_country3),
           dat_ma <- clean_data_continent_card()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days4(),mean,align='right',fill=NA)),
           dat_ma <- clean_data_continent_card()
    )
    ifelse(input$moving_av_country3 ==T & !is.null(input$moving_av_days_country3),
           forecastdata <- forecast_data_c2()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days4(),mean,align='right',fill=NA)),
           forecastdata <- forecast_data_c2()
    )
    
    forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                              forecastdata%>%filter(forecast==1))
    #dat_ma <- clean_data_continent_card()
    plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
    #svetla #7DE2D1
    #temna #339989
    
    #plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                             line = list(color = 'black'),
                             hovertemplate = paste(
                               paste0('<extra></extra>Continent Name: %{text}\n','Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_country3==T & !is.null(input$moving_av_days_country3)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot",color='#7DE2D1'), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Continent Name: %{text}\n",'Metric: ',name_fix(input$metric_continent_card),': %{y}\nDate: %{x}')) )
    }
    plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color='#339989', showlegend = F,
                            type="scatter", mode="lines", line=list(color='#339989', dash="dot"),
                            hovertemplate = paste(
                              paste0('<extra>Forecast</extra>Continent Name: %{text}\n',name_fix(input$metric_continent_card),': %{y}\nDate: %{x} ')
                            ))
    highlight(plt)
    ###########
    
    
    
    
    
  }
  
  output$single_continent_plot <- renderPlotly({
    req(input$continent_card)
    ifelse(make_forecast_c2$value==0, return (plot_by_continent_card()), return(plot_by_continent_card_forecast()))
    #return(plot_by_continent_card())
    
  })
  
  #plot continent compare----
  make_forecast2 <- reactiveValues(value=0)
  
  observeEvent(input$forecast_button2, {
    make_forecast2$value <- 1
  })
  observeEvent(input$remove_forecast_button2, {
    make_forecast2$value <- 0
  })
  
  
  output$continent_plot <- renderPlotly({
    req(input$country_continent)
    ifelse(make_forecast2$value==0, return (plot_by_continent()), return(plot_data_continent_forecast()))
    
  })
  clean_data_continent <- reactive({cases_new <- basic_data %>% 
    filter(location %in% input$country_continent) %>% 
    filter(date >= input$daterange_continent[1])%>%filter(date <= input$daterange_continent[2])%>%
    select(location, date,input$metric_continent )%>%
    set_names(c("location", "date", "metric"))%>%
    arrange(date)})
  
  forecast_data2 <- function(){
    unforecasted_data <- clean_data_continent()
    unforecasted_data$forecast <- 0 #forecast je 0 če je to datum s podatki in 1 če imamo forecast podatkov
    forecasted_data <- predictions_by_continent()
    forecasted_data <- do.call(rbind,forecasted_data)
    rbind(unforecasted_data,forecasted_data) 
  }
  create_forecast2 <- function(dat, num_forecasts){
    name_country <- unique(dat$location)
    auto_forecast <-  forecast(auto.arima(dat$metric),num_forecasts)$mean
    max_date <- max(dat$date)
    new_dates <- max_date + c(1:num_forecasts)
    new_forecast <- tibble( location = name_country, date = new_dates , metric = as.vector(auto_forecast), forecast = 1 )
    return(new_forecast)
  }
  
  predictions_by_continent <- function(){
    clean_data_continent() %>%
      group_by(location) %>%
      group_map(~ create_forecast2(.x, num_forecasts=input$forecast2), .keep=T )
  }
  predictions_by_continent_c <- function(){
    clean_data_continent_card() %>%
      group_by(location) %>%
      group_map(~ create_forecast2(.x, num_forecasts=input$forecast4), .keep=T )
  }
  
  ma_days2 <- eventReactive(input$moving_av_button_continent,
                           {
                             req(input$moving_av_days_continent)
                             input$moving_av_days_continent
                           }, ignoreNULL = FALSE)
  
  plot_by_continent <- function(){
    ifelse(input$moving_av_continent ==T & !is.null(input$moving_av_days_continent),
           dat_ma <- clean_data_continent()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days2(),mean,align='right',fill=NA)),
           dat_ma <- clean_data_continent()
    )
    
    plt <- plot_ly (data = dat_ma, x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines',
                             hovertemplate = paste(
                               paste0('<extra></extra>Country Name: %{text}\n','Metric: ',name_fix(input$metric_continent),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_continent==T & !is.null(input$moving_av_days_continent)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Country Name: %{text}\n",'Metric: ',name_fix(input$metric_continent),': %{y}\nDate: %{x}')) )
    }
    highlight(plt)
    
    
  }
  plot_data_continent_forecast <- function(){
    
    ifelse(input$moving_av_continent ==T & !is.null(input$moving_av_days_continent),
           forecastdata <- forecast_data2()%>%group_by(location)%>%mutate(ma2=rollapply(metric,ma_days2(),mean,align='right',fill=NA)),
           forecastdata <- forecast_data2()
    )
    
    forecasted_data2 <- rbind(forecastdata%>%filter(forecast==0)%>%filter(date==max(date)),
                              forecastdata%>%filter(forecast==1))
    
    plt <- plot_ly (data = forecastdata%>%filter(forecast==0), x=~date, color=~location, text = ~location)
    plt <- plt %>% add_trace(y=~metric, type='scatter', mode='lines+markers',
                             hovertemplate = paste(
                               paste0('<extra></extra>Continent Name: %{text}\n',name_fix(input$metric_continent),': %{y}\nDate: %{x} ')
                             ))
    if(input$moving_av_continent==T & !is.null(input$moving_av_days_country)){ 
      plt <- plt %>% add_trace(y=~ma2, type='scatter', mode='lines', line=list(dash="dot"), showlegend=F,
                               hovertemplate = paste(
                                 paste0("<extra>Moving Average</extra>Country Name: %{text}\n",name_fix(input$metric_continent),': %{y}\nDate: %{x}')) )
    }
    plt <- plt%>% add_trace(data = forecasted_data2 , y=~metric, x=~date, color=~location, showlegend = F,
                            type="scatter", mode="lines", line=list(color=~location, dash="dot"),
                            hovertemplate = paste(
                              paste0('<extra>Forecast</extra>Country Name: %{text}\n',name_fix(input$metric_continent),': %{y}\nDate: %{x} ')
                            ))
    highlight(plt)
  }
  
  
  #data buttons----
  output$download_csv2 <- renderUI({
    fluidRow( class="btns-dwnl",
    column( width=2,
    downloadButton(
                 "download_csv",
                 inputId = "remove_forecast_buttonx",
                 icon = icon("glyphicon glyphicon-download", lib = "glyphicon"),
                 
                 label = "Download as CSV",
                 class = "btn3"
    )),
    )
    
  })
  output$downloadButton<- renderUI({
    
                      downloadButton(
                        "download_csv",
                        label = "Download as CSV",
                        class = "btn3"
                      
    )
    
  })
  output$download_csv <- downloadHandler(
    filename = function() {
      "data.csv"
    },
    content = function(file) {
      write.csv(clean_data_country_t(), file, row.names = FALSE)
    }
  )
  #country data boxes----
  output$country_boxes <- renderUI({
    fluidRow( class="country_box_row",
              column(width=4, box(class="countbpx","TOTAL CASES",br(), "328765376", icon = icon("virus-covid")) ),
              column( width=4,box(class="countbpx",title = "drzava", "328765376")
                      ),
              column( width=4,box(class="countbpx",title = "drzava", "328765376")
                      ))
  })
  output$play <- renderUI({
    fluidRow(
      summaryBox2("Italy total cases", "402,450", width = 3, icon = "fa-solid fa-virus", style = "primary"),
      summaryBox2("Italy total deaths", "29,332", width = 3, icon = "fa-solid fa-viruses", style = "success"),
      summaryBox2("Italy total vaccinations", "346,283", width = 3, icon = "fa-solid fa-syringe", style = "danger"),
      summaryBox2("Italy mortality rate", "1.46", width = 3, icon = "fa-solid fa-percent", style = "primary")
    )#spodaj- v date rangu, people vaccinated, poglej og dataframe
  })
   
}





shinyApp(ui, server)