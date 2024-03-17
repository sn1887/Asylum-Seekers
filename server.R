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
