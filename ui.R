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
