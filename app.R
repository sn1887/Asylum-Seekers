packs <- c("dplyr","ggplot2","shiny","DT","ggrepel","tidyr","shinycssloaders",
           "shinythemes","ggfortify","leaflet","ggthemes")
#install.packages(packs)
library(e1071)
library(naivebayes)
library(randomForest)
library(caret)
library(class)
library(lightgbm)
library(randomForest)
library(gbm)
library(xgboost)

library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(ggfortify)
library(leaflet)
library(ggthemes)

#Import Data
d3 = read.csv("asylum_seekers_refugee_status.csv", skip = 3, header = T)


# Changing variables from character to numerical variables:

for (i in names(d3[-c(2,3,4)])){
  new_i <- c()
  for (j in d3[i]){
    new_i <- c(new_i, as.numeric(j))
  }
  d3[i] <- new_i
}
d3 = na.omit(d3)

# find the country list

country <- data.frame('country' = sort(unique(d3[[3]])), 'log'=NA, 'lat'= NA, "variable"=NA)
map<- read.csv('https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv')
for (row in 1:nrow(country)){
  i <- country[row,1]
  if (i %in% map[[2]]){
    m <- subset(map, Country==i)
    country[row,c(2,3)] <- m[c(4,3)]
  }
}
keep <- as.data.frame(rbind(c('Central African Rep.',20.9394,6.6111,NA),
                            c('China, Macao SAR',113.5439,22.1987,NA),
                            c('Iran (Islamic Rep. of)',53.6880,32.4279,NA),
                            c('China, Hong Kong SAR',114.1694,22.3193,NA),
                            c('Rep. of Korea',127.7669,35.9078,NA),
                            c('Rep. of Moldova',28.3699,47.4116,NA),
                            c('South Sudan',31.3070,6.8770,NA),
                            c('Viet Nam',108.2772,14.0583,NA)))
names(keep) <- names(country)
country <- as.data.frame(rbind(country,keep))
country[4] <- 1
country <- na.omit(country)

country <- country[order(country$country),]

country <- data.frame(Country=country$country, log=country$log,lat=country$lat, variable=NA)

countrylist <- sort(country[[1]])







log_data = d3

log_data[, sapply(d3, is.numeric)] = log(d3[, sapply(d3, is.numeric)])
log_data[, sapply(d3, is.numeric)] = sapply(log_data[, sapply(d3, is.numeric)], 
                                            function (x) ifelse(is.finite(x) == F, NA, x) )

log_data = na.omit(log_data)

sample_data = sample_n(d3, 5000)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

ui <- fluidPage(
  #Navbar structure for UI
  navbarPage("Pied Piper", theme = shinytheme("flatly"),
             navbarMenu("About", icon = icon("info-circle"),
                        tabPanel("Our Work", fluid = TRUE,
                                 fluidRow(
                                   column(6,
                                          #br(),
                                          h4(p("ABOUT THE PROJECT")),
                                          h5(p("In response to the growing need for empirical insights into the experiences of asylum seekers, our project focuses on analyzing a 
                                               comprehensive dataset using RShiny, a web application framework in R. Through this endeavor, and we aim to provide a nuanced understanding of 
                                               asylum migration dynamics, exploring patterns, trends, and potential predictors of asylum outcomes.")),
                                          br(),
                                          h5(p("Our interactive application offers seamless access to a vast repository of over 100,000 data points spanning 14 variables across a 15-year timeframe.
                                               Our endeavor extends beyond mere data dissemination; it aims to serve as a resource for individuals seeking to deepen their understanding of this 
                                               pressing humanitarian issue. ")),
                                          br(),
                                          h5(p("We provide a range of services: from smart searching and exploration of the data, to easy to understand visuals (including maps, plots and Machine-Learning models).")),
                                          br(),
                                          h5(p("I hope you find it interesting and/or useful.  Any comments or questions are welcome at piedpipers@gmail.com"),
                                             p("The source code for this Shiny app is available on ", a("Github", href = "https://github.com/sn1887/Asylum-Seekers/tree/main"), "."))
                                          
                                          #hr(),
                                          
                                   ),
                                   column(6,
                                          #br(),
                                          #             HTML('<img src="GregPicCrop.png", height="110px"
                                          # style="float:right"/>','<p style="color:black"></p>'),
                                          h4(h4(p("ABOUT THE Data")),
                                             h5(p("This data was acquired from the UNHCR Population Statistics database. It presents information about asylum applications lodged in almost 200 
                                                  countries across the globe between the years 2000 and 2014. Data are broken down by month and country of origin. "),
                                                
                                                p("UNHCR's populations of concern include refugees, asylum-seekers, Internally Displaced Persons (IDPs), 
                                                stateless persons, and individuals who do not necessarily fall directly into any of the 
                                                groups above but to whom UNHCR extends its protection 
                                                and assistance services based on humanitarian or other special grounds. "),
                                                
                                                p("Sources include UNHCR Population Statistics and UNHCR Populations of Concern."),
                                             )),
                                          br()
                                   )
                                 ),
                                 br(),
                                 hr(),
                                 h5("Sources:"),
                                 h6(
                                   p("Asylum Information from ",
                                     a("asylum-applications",
                                       href = "https://data.world/unhcr/asylum-applications/"))),
                                 h5("Built with",
                                    img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                    
                                 ))),
             
             tabPanel('Location Finder', icon = icon("globe-americas"),
                      headerPanel(''),
                      
                      # Input values
                      sidebarPanel(
                        h3(strong('MAP')),
                        
                        
                        
                        selectInput("country1", label = "Country (if available):", 
                                    choices = countrylist,
                                    selected =1,
                                    multiple=TRUE),
                        
                        fluidRow(
                          column(12,selectInput("num_var", label = "Choose variable:", 
                                                choices = as.list(colnames(d3))[-c(1,2,3,4)],
                                                selected ="Total.persons.pending.start.year"))),
                        
                        
                        sliderInput("year1", "Year:",
                                    min = 2000, max = 2014,
                                    value = 2000, sep = ""),
                        
                        actionButton("button", "Submit", icon = icon("play"), class = "btn btn-primary"), width = 3
                      ),
                      
                      mainPanel(
                        tags$label(h3(strong("Let's explore the map!"))),
                        tags$div(class = "widget-user-header bg-red-active text-center"),
                        leafletOutput("mymap", height = "500"),# Status/Output Text Box
                        
                      )),
             tabPanel('Data Visualization', icon = icon("chart-bar"),
                      sidebarLayout(
                        sidebarPanel(
                          titlePanel("Customize your plot:")
                          ,
                          # Application title
                          tabPanel("my plot", width = 2,
                                   selectInput("select1", label = h3("Select x-axis"), 
                                               choices = list("Year"='Year'                                
                                                              ,"Country/Territory of asylum residence"='Country...territory.of.asylum.residence'
                                                              ,"Origin"='Origin'), 
                                               selected = 1),
                                   selectInput("select2", label = h3("Select y-axis"), 
                                               choices = list("Total people pending at the start of the year" = "Total.persons.pending.start.year",
                                                              "People UNHCR assisted " = "of.which.UNHCR.assisted ",
                                                              "People who applied during the year" = "Applied.during.year",
                                                              "Decision recognized" = "statistics.filter.decisions_recognized",
                                                              "Filter decision other" = "statistics.filter.decisions_other",
                                                              "Rejected" = "Rejected",
                                                              "Otherwise closed " = "Otherwise.closed ",
                                                              "Total decisions" = "Total.decisions ",
                                                              "Total pending at the end of year" = "Total.persons.pending.end.year",
                                                              "Of which UNHCR assisted" = "of.which.UNHCR.assisted.1"), 
                                               selected = 1),
                                   selectizeInput('countries',
                                                  label="Choose up to 6 countries to show (if available):",
                                                  choices=sort(unique(c(d3[[2]],d3[[3]]))),
                                                  selected = c("Turkey","South Africa","Ukraine","United States of America"),
                                                  multiple = TRUE,
                                                  options = list(maxItems = 6)
                                   ),
                                   
                                   sliderInput("size", label = h3("Size of point characters:"),
                                               min = 0.1, max = 8, value = 1.5),
                                   
                                   sliderInput("slider1", label = h3("Year Range:"), min = 2000, 
                                               max = 2014, value = c(2008, 2012), sep = ""),
                                   
                                   selectInput("face", label = h3("facet_wrap"),
                                               choices = list('-'='',
                                                              "Year"='Year'                                
                                                              ,"Country...territory.of.asylum.residence"='Country...territory.of.asylum.residence'
                                                              ,"Origin"='Origin' )),
                                   
                                   
                                   selectInput("color", label = h3("Choose the color"), 
                                               choices = list("black"='color="black")'      
                                                              ,"red"='color="red")'     
                                                              ,"dark red"='color="dark red")'
                                                              ,"blue"='color="blue")'                  
                                                              ,"yellow"='color="yellow")' 
                                                              ,"purple"='color="purple")'     
                                                              ,'green'='color="green")'
                                                              ,'dark green'='color="dark green")'
                                                              ,"pink"='color="pink")'
                                                              ,"white"='color="white")'
                                               )),
                                   
                                   selectInput("theme", label = h3("Change the theme"), 
                                               choices = list('Original theme'='',
                                                              'classic'='+theme_classic()'
                                                              ,'economist'='+theme_economist()'
                                                              ,'stata'='+theme_stata()'
                                                              ,'base'='+theme_base()'
                                                              ,'dark'='+theme_dark()'
                                                              ,'get'='+theme_get()'),
                                               selected=1),
                                   
                                   
                                   
                                   
                                   
                                   titlePanel("Extra modifications:"),
                                   
                                   checkboxInput("boxplot", label = "Box Plot", value = FALSE),
                                   checkboxInput("violin", label = "Violin Plot", value = FALSE),
                                   checkboxInput("checkbox", label = "Linear Regression", value = FALSE),
                                   checkboxInput("outlier", label = "Remove Outliers", value = FALSE),
                                   checkboxInput("jitter", label = "jitter", value = TRUE),
                                   checkboxInput("lognomial", label = "Log Plot", value = TRUE),
                                   downloadButton("downloader", "Download"),
                                   
                                   
                                   hr(),
                                   fluidRow(column(2, verbatimTextOutput("value"))
                                   )
                                   
                                   
                                   
                          )),
                        
                        
                        
                        
                        mainPanel(
                          
                          h3(textOutput("caption")),
                          
                          plotOutput("myplot")
                        )
                        
                        
                      )
             ),
             ##
             tabPanel('Model Analysis', icon=icon("external-link-square"),
                      sidebarLayout(
                        sidebarPanel(
                          fluidRow(
                            h5(strong('>>  Model: Analysis')), h5(strong('>>  Linear Regression (OLS)')), 
                            h5(strong('>> Data: Asylum Seekers'))
                          ),
                          fluidRow( 
                            column(12,selectInput("outcome", label = h5(strong("Outcome:")),  
                                                  choices = list(
                                                    "Total people pending at the start of the year" = "Total.persons.pending.start.year",
                                                    "People UNHCR assisted " = "of.which.UNHCR.assisted",
                                                    "People who applied during the year" = "Applied.during.year",
                                                    "Decision recognized" = "statistics.filter.decisions_recognized",
                                                    "Filter decision other" = "statistics.filter.decisions_other",
                                                    "Rejected" = "Rejected",
                                                    "Otherwise closed " = "Otherwise.closed",
                                                    "Total decisions" = "Total.decisions",
                                                    "Total pending at the end of year" = "Total.persons.pending.end.year",
                                                    "Of which UNHCR assisted" = "of.which.UNHCR.assisted.1"), selected = 1))),
                          fluidRow(
                            column(12,selectInput("indepvar", label = h5(strong("Explanatory variable:")),
                                                  choices = list(
                                                    "Total people pending at the start of the year" = "Total.persons.pending.start.year",
                                                    "People UNHCR assisted " = "of.which.UNHCR.assisted",
                                                    "People who applied during the year" = "Applied.during.year",
                                                    "Decision recognized" = "statistics.filter.decisions_recognized",
                                                    "Filter decision other" = "statistics.filter.decisions_other",
                                                    "Rejected" = "Rejected",
                                                    "Otherwise closed " = "Otherwise.closed",
                                                    "Total decisions" = "Total.decisions",
                                                    "Total pending at the end of year" = "Total.persons.pending.end.year",
                                                    "Of which UNHCR assisted" = "of.which.UNHCR.assisted.1"), selected = 1))), 
                          selectInput("model_choice", label = h5("Choose a model"), 
                                      choices = c(
                                        "MLR", "SVR", "Random Forest"
                                        , "Naive Bayes", 'K-Nearest Neighbors', "XGBoost", "LightGBM"
                                      )),
                          
                          fluidRow(
                            column(12,
                                   ailgn="center",
                                   textInput(inputId = "pred", label = h5(strong("Prediction input value:"))))),
                          fluidRow(
                            column(3,actionButton(inputId = 'go', label = "Predict!",icon = icon("play"), align = "center"))
                          ),
                          
                          width = 2
                          
                          
                        ),
                        
                        mainPanel(
                          
                          tabsetPanel(type = "tabs",
                                      
                                      tabPanel("Scatterplot",
                                               fluidRow(
                                                 column(4, plotOutput("scatterplot"), offset = 2),
                                                 column(4, plotOutput("heatmap"))),
                                               fluidRow(
                                                 column(4, plotOutput("distribution1"), offset = 2),
                                                 column(4, plotOutput("distribution2")))
                                      ), # Plot
                                      tabPanel("Linear Assumption", # Plots of distributions
                                               fluidRow(
                                                 column(4, plotOutput("LinearityAssumptions1")))
                                               
                                      ),
                                      tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                                      tabPanel("Prediction", verbatimTextOutput('predict') ) # Data as datatable
                                      
                          )))),
             tabPanel('Data Explorer',icon = icon("table"),
                      titlePanel("Exploring the Dataset: Asylum Seekers"),
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("Year11", label = h3("Year"),
                                      min = 2000, max = 2014,
                                      value = c(2000,2014),sep = ""),
                          selectInput("CountryofOrigin11", label = h3("Country of Origin"),
                                      choices = sort(unique(d3[["Origin"]])), selected = 0,
                                      multiple = TRUE),
                          selectInput("CountryofAsylum11", label = h3("Country of Asylum"),
                                      choices = sort(unique(d3[["Country...territory.of.asylum.residence"]])), selected = 0,
                                      multiple = TRUE),
                          selectInput('checkGroup', label = h3('Choose the variables to view the summary'), 
                                      choices = list('Year'='Year'                                
                                                     ,'Country...territory.of.asylum.residence'='Country...territory.of.asylum.residence'
                                                     ,'Origin'='Origin'                              
                                                     ,'RSD.procedure.type...level'='RSD.procedure.type...level'            
                                                     ,'Total.persons.pending.start.year'='Total.persons.pending.start.year'      
                                                     ,'of.which.UNHCR.assisted'='of.which.UNHCR.assisted'               
                                                     ,'Applied.during.year'='Applied.during.year'                  
                                                     ,'statistics.filter.decisions_recognized'='statistics.filter.decisions_recognized' 
                                                     ,'statistics.filter.decisions_other'='statistics.filter.decisions_other'     
                                                     ,'Rejected'='Rejected'
                                                     ,'Otherwise.closed'='Otherwise.closed'                   
                                                     ,'Total.decisions'='Total.decisions'                    
                                                     ,'Total.persons.pending.end.year'='Total.persons.pending.end.year'      
                                                     ,'of.which.UNHCR.assisted.1'='of.which.UNHCR.assisted.1'),
                                      selected = 1,
                                      multiple=TRUE),
                          actionButton("update11", "Update View", icon = icon("refresh"))),
                        
                        
                        mainPanel(
                          
                          tabsetPanel(type = "tabs",
                                      tabPanel("Data", DT::dataTableOutput('tbl11')), # Data as datatable
                                      tabPanel("Summary",
                                               verbatimTextOutput('summary11'))
                                      # Summary  
                                      
                          )
                        )
                        
                      )
                      
                      
             ),
             tabPanel("Questions",icon = icon("question-circle"),
                      fluid = TRUE,
                      fluidRow(
                        column(6,
                               h3(p("Some questions that could be researched on based on our shiny app:")),
                               h5(p("Do refugees and Venezuelans displaced abroad lived in countries neighbouring their countries of origin?"),
                                  p("Do refugees usually returne or resettle from the new country they have moved to?"),
                                  p("At the times that UNHCR assisted, did the number of rejected applications change?"),
                                  p("What were the countries with the most number of people taking refugee from them?"),
                                  p("What were the countries with the most number of people taking refugee to them?"),
                                  p("Do famous developing countries host more refugees then others?"),
                                  p("Analysis of top international displacement situations by country of origin"),
                                  p("How is the refugee density different in different times of world crisis? (Such as the 2007-2008 food crisis or the 2011 East Africa drought, etc.)"),
                                  p(""),
                                  p(""),
                                  p(""),
                                  p(""),
                                  p(""),
                               )
                        )
                      ))
  ))


















# Define server
server <- function(input, output) {
  DATA <- eventReactive(input$update11, {
    d3 = read.csv("asylum_seekers_refugee_status.csv", skip = 3, header = T)
    
    for (i in names(d3[-c(2,3,4)])){
      new_i <- c()
      for (j in d3[i]){
        new_i <- c(new_i, as.numeric(j))
      }
      d3[i] <- new_i
    }
    if(is.null(input$CountryofAsylum11) & is.null(input$CountryofOrigin11)){
      subset(d3, (Year>=input$Year11[1] & Year<=input$Year11[2]))
    }else if(is.null(input$CountryofAsylum11)){
      subset(d3, (Origin == input$CountryofOrigin11 & Year>=input$Year11[1] & Year<=input$Year11[2]))
    }else if(is.null(input$CountryofOrigin11)){
      subset(d3, (Country...territory.of.asylum.residence== input$CountryofAsylum11 & Year>=input$Year11[1] & Year<=input$Year11[2]))
    }else{
      d3 <- subset(d3, (Origin == input$CountryofOrigin11 & Year>=input$Year11[1] & Year<=input$Year11[2]))
      subset(d3 ,Country...territory.of.asylum.residence== input$CountryofAsylum11)
    }
  })
  
  # Data output
  output$tbl11 = renderDataTable({
    
    DT::datatable(DATA(), options = list(lengthChange = FALSE))
  })
  
  
  
  summaryfunc <- eventReactive(input$update11, {
    d3 = read.csv("asylum_seekers_refugee_status.csv", skip = 3, header = T)
    
    for (i in names(d3[-c(2,3,4)])){
      new_i <- c()
      for (j in d3[i]){
        new_i <- c(new_i, as.numeric(j))
      }
      d3[i] <- new_i
    }
    d3[as.character(input$checkGroup)]})
  #  Summary output 
  output$summary11 <- renderPrint({ summary(summaryfunc()) })
  
  
  
  
  observeEvent(input$button, {
    output$mymap <-renderLeaflet({
      ############
      
      
      country <- data.frame('country' = sort(unique(d3[[3]])), 'log'=NA, 'lat'= NA, "variable"=NA)
      
      
      map<- read.csv('https://raw.githubusercontent.com/albertyw/avenews/master/old/data/average-latitude-longitude-countries.csv')
      
      for (row in 1:nrow(country)){
        i <- country[row,1]
        if (i %in% map[[2]]){
          m <- subset(map, Country==i)
          country[row,c(2,3)] <- m[c(4,3)]
        }
      }
      
      
      keep <- as.data.frame(rbind(c('Central African Rep.',20.9394,6.6111,NA),
                                  c('China, Macao SAR',113.5439,22.1987,NA),
                                  c('Iran (Islamic Rep. of)',53.6880,32.4279,NA),
                                  c('China, Hong Kong SAR',114.1694,22.3193,NA),
                                  c('Rep. of Korea',127.7669,35.9078,NA),
                                  c('Rep. of Moldova',28.3699,47.4116,NA),
                                  c('South Sudan',31.3070,6.8770,NA),
                                  c('Viet Nam',108.2772,14.0583,NA)))
      names(keep) <- names(country)
      
      country <- as.data.frame(rbind(country,keep))
      
      country[4] <- 1
      
      country <- na.omit(country)
      
      country <- country[order(country$country),]
      
      
      
      
      country <- data.frame(Country=country$country, log=country$log,lat=country$lat, variable=NA)
      
      for (i in names(country[-1])){
        new_i <- c()
        for (j in country[i]){
          new_i <- c(new_i, as.numeric(j))
        }
        country[i] <- new_i
      }
      
      subset(country, Country==input$country1)
      
      
      
      d3new <- subset(d3, Origin==input$country1 & Year == input$year1)
      
      for (i in input$country1){
        m <- sum(subset(d3new, Origin==i)[[input$num_var]],na.rm=T)#####issue
        for (j in 1:nrow(country)){
          if(country[j,1]==i){
            country[j,4] <- m
          }
        }
      }
      leafdata <- na.omit(country)
      
      
      
      ##
      leafdata %>% 
        leaflet() %>%
        addTiles() %>%
        addCircles(~log, ~lat, 
                   weight = 10,
                   radius = 120,
                   popup = paste0(
                     "<b>Country: </b>", 
                     leafdata$Country,
                     "<br>",
                     "<b> Year : </b>",
                     input$year1,
                     "<br>",
                     paste0("<b>Total",input$num_var,": </b>"),
                     leafdata[[4]])
                   
        ) %>% 
        setView(lng = median(leafdata$log),
                lat = median(leafdata$lat),
                zoom =2.5)
      
    })
  })
  #######################################
  ####################################### Abtin
  #get the plot image URI and simulate click on download button
  
  
  
  
  formulaText <- function(){
    
    
    paste(input$select2,"~", input$select1)
  }
  
  
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    d3 <- na.omit(d3)
    formulaText()
  })
  
  
  
  
  output$myplot <- renderPlot({
    d3 = read.csv("asylum_seekers_refugee_status.csv", skip = 3, header = T)
    
    for (i in names(d3[-c(2,3,4)])){
      new_i <- c()
      for (j in d3[i]){
        new_i <- c(new_i, as.numeric(j))
      }
      d3[i] <- new_i
    }
    plot <- function(d3){
      
      
      d3[1] <- as.character.default(d3[[1]])
      
      if(input$select1=='Year'){
        
        
        if(input$face == 'Country...territory.of.asylum.residence'){
          
          d3 <- subset(d3, Country...territory.of.asylum.residence==input$countries)
          
        }else if(input$face == 'Origin'){
          
          d3 <- subset(d3, Origin==input$countries)
          
        }
        
        violin <- ifelse(input$violin, yes = '+geom_violin(alpha=0.3)', no = '')
        j <- paste0('+geom_jitter',"(size=",input$size, ",",input$color)
        jitter <- ifelse(input$jitter, yes = j, no='')
        lognomial <- ifelse(input$lognomial, yes = '+scale_y_log10()', no='')
        linearR <- ifelse(input$checkbox, yes = '+stat_summary(geom = "line", fun = mean, group = 1, size=1.5, color="dark red")', no = '')
        facet <- ifelse((input$face != '' & input$select1 != input$face), yes = paste0("+facet_wrap(~",input$face,")"), no = '')
        subdata <- subset(d3, (Year>=input$slider1[1] & Year<=input$slider1[2]))
        box <- ifelse(input$boxplot, yes = '+geom_boxplot(alpha=0.3)' ,no ='')
        
        t <- paste0('ggplot(data = subdata, mapping=aes_string(input$select1,input$select2))+
        geom_point',"(size=",input$size, ",",input$color,'+
        theme(axis.text.x = element_text(angle = 40, hjust = 1, size=13))',input$theme,
                    facet, jitter,linearR,violin, lognomial, box)
        eval(parse(text=t))
        
      }
      
      
      
      
      
      
      else if(input$select1=='Country...territory.of.asylum.residence' | input$select1=='Origin'){
        
        
        violin <- ifelse(input$violin, yes = '+geom_violin(alpha=0.3)', no = '')
        j <- paste0('+geom_jitter',"(size=",input$size, ",",input$color)
        jitter <- ifelse(input$jitter, yes = j, no='')
        box <- ifelse(input$boxplot, yes = '+geom_boxplot(alpha=0.3)' ,no ='')
        lognomial <- ifelse(input$lognomial, yes = '+scale_y_log10()', no='')
        linearR <- ifelse(input$checkbox, yes = '+stat_summary(geom = "line", fun = mean, group = 1, size = 1.5, color="dark red")', no = '')
        facet <- ifelse(input$face == 'Year', yes = paste0("+facet_wrap(~Year)"), no = '')
        d3 <- subset(d3, (Year>=input$slider1[1] & Year<=input$slider1[2]))
        
        d3 <- subset(d3, eval(parse(text=input$select1))==input$countries)
        
        
        t <- paste0('ggplot(data = d3, mapping=aes_string(input$select1,input$select2))+
        geom_point',"(size=",input$size, ",",input$color,'+
        theme(axis.text.x = element_text(angle = 40, hjust = 1, size=13))',input$theme,
                    facet, jitter,linearR, violin,lognomial,box)
        eval(parse(text=t))}
    }
    f <- function(d3){
      d3 <- d3[order(d3[input$select2], decreasing = TRUE),]
      top5p <- as.integer(0.01*(dim(d3)[1]))
      max <- dim(d3)[1]
      d3_new <- d3[top5p:max,]
      plot(d3_new)
    }
    if(input$outlier){
      if(input$select1=="Year"){
        f(d3)
      }else{
        
        d3 <- subset(d3, eval(parse(text=input$select1))==input$countries)
        
        f(d3)
      }
      
    }else{
      plot(d3)
    }
  }
  )
  
  
  
  
  select_plot <- function(){
    d3 = read.csv("asylum_seekers_refugee_status.csv", skip = 3, header = T)
    
    for (i in names(d3[-c(2,3,4)])){
      new_i <- c()
      for (j in d3[i]){
        new_i <- c(new_i, as.numeric(j))
      }
      d3[i] <- new_i
    }
    plot <- function(d3){
      
      
      if(input$select1=='Year'){
        
        
        if(input$face == 'Country...territory.of.asylum.residence'){
          
          d3 <- subset(d3, Country...territory.of.asylum.residence==input$countries)
          
        }else if(input$face == 'Origin'){
          
          d3 <- subset(d3, Origin==input$countries)
          
        }
        
        violin <- ifelse(input$violin, yes = '+geom_violin(alpha=0.3)', no = '')
        j <- paste0('+geom_jitter',"(size=",input$size, ",",input$color)
        jitter <- ifelse(input$jitter, yes = j, no='')
        lognomial <- ifelse(input$lognomial, yes = '+scale_x_log10()+scale_y_log10()', no='')
        linearR <- ifelse(input$checkbox, yes = '+stat_summary(geom = "line", fun = mean, group = 1, size=1.5, color="dark red")', no = '')
        facet <- ifelse((input$face != '' & input$select1 != input$face), yes = paste0("+facet_wrap(~",input$face,")"), no = '')
        subdata <- subset(d3, (Year>=input$slider1[1] & Year<=input$slider1[2]))
        box <- ifelse(input$boxplot, yes = '+geom_boxplot(alpha=0.3)' ,no ='')
        
        title <- paste(input$select2,"~", input$select1)
        
        
        t <- paste0('ggplot(data = subdata, mapping=aes_string(input$select1,input$select2))+
        geom_point',"(size=",input$size, ",",input$color,'+
        theme(axis.text.x = element_text(angle = 40, hjust = 1, size=13))',input$theme,
                    facet, jitter,linearR, violin, lognomial,box,'+
                      labs(title = "',title,'",
                            caption ="Downloded from Pied Piper shiny app")')
        eval(parse(text=t))
        
      }
      
      
      
      
      
      
      else if(input$select1=='Country...territory.of.asylum.residence' | input$select1=='Origin'){
        
        
        violin <- ifelse(input$violin, yes = '+geom_violin(alpha=0.3)', no = '')
        j <- paste0('+geom_jitter',"(size=",input$size, ",",input$color)
        jitter <- ifelse(input$jitter, yes = j, no='')
        box <- ifelse(input$boxplot, yes = '+geom_boxplot(alpha=0.3)' ,no ='')
        lognomial <- ifelse(input$lognomial, yes = '+scale_y_log10()', no='')
        linearR <- ifelse(input$checkbox, yes = '+stat_summary(geom = "line", fun = mean, group = 1, size=1.5, color="dark red")', no = '')
        facet <- ifelse(input$face == 'Year', yes = paste0("+facet_wrap(~Year)"), no = '')
        d3 <- subset(d3, (Year>=input$slider1[1] & Year<=input$slider1[2]))
        
        d3 <- subset(d3, eval(parse(text=input$select1))==input$countries)
        
        
        title <- paste(input$select2,"~", input$select1)
        
        
        t <- paste0('ggplot(data = d3, mapping=aes_string(input$select1,input$select2))+
        geom_point',"(size=",input$size, ",",input$color,'+
        theme(axis.text.x = element_text(angle = 40, hjust = 1, size=13))',input$theme,
                    facet, jitter, linearR,violin, lognomial,box,'+
                      labs(title = "',title,'",
                            caption ="Downloded from Pied Piper shiny app")')
        eval(parse(text=t))}
    }
    f <- function(d3){
      d3 <- d3[order(d3[input$select2], decreasing = TRUE),]
      top5p <- as.integer(0.01*(dim(d3)[1]))
      max <- dim(d3)[1]
      d3_new <- d3[top5p:max,]
      plot(d3_new)
    }
    if(input$outlier){
      if(input$select1=="Year"){
        f(d3)
      }else{
        
        d3 <- subset(d3, eval(parse(text=input$select1))==input$countries)
        
        f(d3)
      }
      
    }else{
      plot(d3)
    }
    
  }
  
  output$downloader <- downloadHandler(
    filename = paste0("MyPlot-",Sys.Date(),'.png') ,
    content = function(file) {
      device <- function(..., width, height) grDevices::png(..., width = 1500, height = 884)
      ggsave(file, plot = select_plot(), device = device)
      
    }
  )
  
  
  
  #######################################
  ####################################### Abdul Samad
  model <- reactive({
    y1 = d3[,input$outcome] ## outcome variable
    x1 = d3[,input$indepvar] ## independant variable
    subset_size <- 2500  # take a subset size
    x1 = x1[sample(subset_size)]
    y1 = y1[sample(subset_size)]
    
    if (input$model_choice == "MLR") {
      
      summary(lm(y1 ~ x1))
    } 
    
    
    else if (input$model_choice == "SVR") {
      model <- svm(y1 ~ x1, kernel = "linear")
      summary(model)  # Print the model summary
    }
    
    
    
    else if (input$model_choice == "Naive Bayes") {
      naiveBayes(y1 ~ x1, data = cbind(x1,y1))
    }
    
    
    ############################################################### 
    else if (input$model_choice == "Random Forest") {
      
      # Build the Random Forest model
      model <- randomForest(y1 ~ x1)
      
      # Print the model
      print(model)
      
      # Display variable importance
      var_importance <- model$importance
      print("Variable Importance:")
      print(var_importance)
    }
    else if (input$model_choice == "K-Nearest Neighbors"){
      
      data = cbind(x1,y1)
      
      parts = sample(2500 * .9)
      train = data[parts, ]
      test = data[-parts, ]
      
      
      train = data.matrix(train)
      test = data.matrix(test)
      
      train = as.matrix(train)
      test = as.matrix(test)
      
      train_scaled = scale(train[, -1])
      test_scaled = scale(test[, -1])
      
      
      test_pred <- knn(
        train = train_scaled, 
        test = test_scaled,
        cl = as.data.frame(train)$y1, 
        k=10
      )
      
      actual <- as.data.frame(test)$y1
      print(summary(test_pred))
      cm <- table(actual,test_pred)
      print(cm)
      accuracy <- sum(diag(cm))/length(actual)
      sprintf("Accuracy: %.2f%%", (1 - accuracy)*100)
      print(test_pred)
    }
    ###############################################################
    
    
    
    else if (input$model_choice == "LightGBM"){
      
      data = cbind(x1,y1)
      
      parts = sample(2500 * .9)
      train = data[parts, ]
      test = data[-parts, ]
      
      
      #define predictor and response variables in training set
      train_x1 = as.matrix(train[,-ncol(train)])
      train_y = as.matrix(train[,"y1"])
      
      #define predictor and response variables in testing set
      test_x1 = as.matrix(test[,-ncol(test)])
      test_y = as.matrix(test[,"y1"])
      
      # Data interface
      dtrain <- lgb.Dataset(train_x1, label = train_y)
      
      # Parameters
      params <- list(
        objective = "binary"
        , num_leaves = 4L
        , learning_rate = 1.0
      )
      
      # Train
      fit <- lgb.train(
        params
        , data = dtrain
        , nrounds = 10L
        , verbose = -1L
      )
      print("summary of the model results:")
      print(summary(predict(fit, test_x1) == test_y))
      print("")
      print("")
      print("")
      print("the models used:")
      print(lgb.train(
        params
        , data = dtrain
        , nrounds = 10L
        , verbose = -1L
      ))
    }
    
    
    else if (input$model_choice == "XGBoost"){
      
      data = cbind(x1,y1)
      
      parts = sample(2500 * .9)
      train = data[parts, ]
      test = data[-parts, ]
      
      
      #define predictor and response variables in training set
      train_x1 = as.matrix(train[,-ncol(train)])
      train_y = as.matrix(train[,"y1"])
      
      #define predictor and response variables in testing set
      test_x1 = as.matrix(test[,-ncol(test)])
      test_y = as.matrix(test[,"y1"])
      
      
      
      #define final training and testing sets
      xgb_train = xgb.DMatrix(data = train_x1, label = train_y)
      xgb_test = xgb.DMatrix(data = test_x1, label = test_y)
      watchlist = list(train=xgb_train, test=xgb_test)
      xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 50)
    }

  })
  
  output$summary <- renderPrint({
    model()
  })
  
  # Prediction output
  data = eventReactive(input$go, {
    as.numeric(input$pred)
  })
  output$predict <- renderPrint({
    
    y1 = d3[,input$outcome] ## outcome variable
    x1 = d3[,input$indepvar] ## independant variable
    
    fit <- lm(y1 ~ x1)
    
    data_frame = data.frame(x1 = data())
    pred_value  = predict(fit, data_frame, interval = 'confidence')
    cat('Linear regression (OLS)\nData                 : Asylum Seekers\nResponse variable    : y \nExplanatory variables: x\nInterval             : Confidence \nPrediction dataset   : Asylum Seekers')
    
    cat('\nY model prediction: ', pred_value[[1]], '\nY lower bound     : ', pred_value[[2]], '\nY upper bound     : ', pred_value[[3]] )
    
  })
  
  
  
  
  # Scatterplot output
  output$scatterplot <- renderPlot({
    ggplot(data = log_data, aes(x = log_data[,input$indepvar], log_data[,input$outcome]))+
      geom_point(color = 'steel blue')+
      geom_smooth(method = lm, color = 'indianred3')+
      xlim(0,12)+
      ylim(0,12)+
      labs(title = 'Scatterplot', x = 'X value', y = 'Y value'  )+
      theme(plot.title = element_text(hjust = 0.5))
  }, height=350, width = 400)
  
  # Heatmap
  output$heatmap = renderPlot({
    smoothScatter(log_data[,input$indepvar], log_data[,input$outcome], transformation = function(x) x ^ 0.4,
                  colramp = colorRampPalette(c("#000099", "#00FEFF", "#45FE4F",
                                               "#FCFF00", "#FF9400", "#FF3100")), 
                  main = 'HeatMap',xlab = 'X values', 
                  ylab = 'Y values', 
                  col.main = 'indianred3',
                  col.lab = 'indianred3',col = 'indianred3')
    kern <- MASS::kde2d(log_data[,input$indepvar], log_data[,input$outcome])
    contour(kern, drawlabels = FALSE, nlevels = 6,
            col = rev(heat.colors(6)), add = TRUE, lwd = 3)
  }, height=350, width = 400)
  
  # Histogram output var 1
  output$distribution1 <- renderPlot({
    ggplot(mapping = aes(x = log_data[,input$outcome] ), color = 'steel blue')+
      geom_histogram(color = 'sienna')+
      labs(title = 'Distribution of Y', x = 'Y values')+
      theme(plot.title = element_text(hjust = 0.5))
  }, height=300, width=300)
  
  # Histogram output var 2
  output$distribution2 <- renderPlot({
    ggplot(mapping = aes(x = log_data[,input$indepvar] ), color = 'steel blue')+
      geom_histogram(color = 'sienna')+
      labs(title = 'Distribution of X', x = 'X value')+
      theme(plot.title = element_text(hjust = 0.5))
  }, height=300, width=300)
  # Linearity Assumptions
  output$LinearityAssumptions1 = renderPlot({
    autoplot(lm(sample_data[,input$outcome] ~ sample_data[,input$indepvar]))
  }, height = 500, width = 700)
  
  
}
# Run the application
shinyApp(ui = ui, server = server)
