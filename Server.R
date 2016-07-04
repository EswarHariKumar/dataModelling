#reading required data files
timesDataTable <- read.table("timesData_allTbl.csv", stringsAsFactors = FALSE)
timesData.all <- data.frame(timesDataTable)
firstRanked.theRestTbl <- read.table("firstRankedandtheRest.csv", stringsAsFactors = FALSE)
firstRanked.theRest <- data.frame(firstRanked.theRestTbl)
school.country_lookup <- read.csv("school_and_country_table.csv")

shinyServer(function(input, output) {
  #Completely clear all lines except axis lines and make background white
  theme1 <- theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(size = .4)
  )
  #Use theme to change axis label style
  theme2 <- theme(
    axis.title.x = element_text(
      face = "bold",
      color = "black",
      size = 10
    ),
    axis.title.y = element_text(
      face = "bold",
      color = "black",
      size = 10
    ),
    plot.title = element_text(
      face = "bold",
      color = "black",
      size = 12
    )
  )
  #init ranges when scatterplot has been double clicked
  ranges <- reactiveValues(x = NULL, y = NULL)
  #get the 2016 data from timedata.all
  filteted_by_year <- reactive({
    filter(
      timesData.all,
      timesData.all$country %in%
        c(input$asiaUni, input$theRestUni) &
        timesData.all$Year == 2016
    )
  })
  #assigned the y_values when select input has changed
  y_value <- reactive({
    if (input$pIndicators == "Teaching") {
      y_axis <- "teaching_new"
    } else if (input$pIndicators == "International") {
      y_axis <- "international_new"
    } else if (input$pIndicators == "Research") {
      y_axis <- "research_new"
    } else if (input$pIndicators == "Citations") {
      y_axis <- "citations_new"
    } else if (input$pIndicators == "Income") {
      y_axis <- "income_new"
    }
  })
  
  x_value <- reactive({
    if (input$pIndicators == "Teaching") {
      x_axis <- "teaching_new"
    } else if (input$pIndicators == "International") {
      x_axis <- "international_new"
    } else if (input$pIndicators == "Research") {
      x_axis <- "research_new"
    } else if (input$pIndicators == "Citations") {
      x_axis <- "citations_new"
    } else if (input$pIndicators == "Income") {
      x_axis <- "income_new"
    }
  })
  
  #when user has selected more than one points on the scatterplot: use brushedpoints to get all the selected points
  #when user just selected one point on the scatterplot: use nearpoint to get this selected point information.
  #assign these values into allselectedpoints
  allselectedpoints <- reactive({
    td_data2016 <- filteted_by_year()
    td.2016_part <- select(
      td_data2016,
      rank_int,
      ranking_range,
      school_name,
      country,
      teaching_new,
      international_new,
      research_new,
      citations_new,
      income_new,
      overall_new
    )
    if (is.null(input$plot1_brush)){
      nearPoints(td.2016_part,
                 input$plot1_dbclick,
                 threshold = 10,
                 addDist = TRUE,
                 maxpoints = 1,
                 xvar = "overall_new",
                 yvar = y_value()
      )
    }else{
      brushedPoints(td.2016_part,
                    input$plot1_brush,
                    xvar = "overall_new",
                    yvar = y_value())
    }
  })
  
  #plotting the leaflet map. 
  #size of the cirle calculated by sqrt(firstRanked.theRest$overall_new)*2
  #color is specified using pal(firstRanked.theRest$overall_new)
  #popup text is firstRanked.theRest$school_name
  #legend values is determined by firstRanked.theRest$overall_new
  output$mapPlot <- renderLeaflet({
    pal <- colorNumeric(
      palette = "RdPu",
      domain = firstRanked.theRest$overall_new
    )
    
    leaflet(data = firstRanked.theRest) %>% addTiles('http://server.arcgisonline.com/ArcGIS/rest/services/World_Topo_Map/MapServer/tile/{z}/{y}/{x}', attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
      addCircleMarkers(~lon, ~lat,layerId = ~country,
                       radius = ~sqrt(firstRanked.theRest$overall_new)*2,
                       color = ~pal(firstRanked.theRest$overall_new),
                       popup = paste(firstRanked.theRest$school_name,"<br>")) %>% 
      addLegend("bottomright", pal = pal,values = ~firstRanked.theRest$overall_new,
                title = "Performance",
                opacity = 1)
  })
  
  # a bar chart will show up when click on the cicle
  output$barChartUniScore <- renderPlot({
    if (is.null(input$mapPlot_marker_click)){
      return()
    }
    #get required data for no 1 university
    no_1_uni <- select(firstRanked.theRest, country,
                 teaching_new,research_new,
                 international_new,citations_new,income_new,overall_new,
                 school_name,Year,rank_int,ranking_range,lon,lat)
    #renaming the coulum name
    names(no_1_uni)[c(2,3,4,5,6,7)] <- c("Teaching","Research","International","Citations",
                                   "Income","Overall")
    #get the mean values for the other universities
    the_other_uni <- select(firstRanked.theRest,country,teachingMean2,
                  researchMean2,
                  internationalMean2,
                  citationsMean2,incomeMean2,overallMean2,
                  school_name,Year,rank_int,ranking_range,lon,lat)
    #rename the column names to be the same as no_1_uni data frame
    names(the_other_uni)[c(2,3,4,5,6,7)] <- c("Teaching","Research","International","Citations",
                                    "Income","Overall")
    #chang the values in scholl_name column to "Other Universities"
    the_other_uni[,"school_name"] <- "Other Universities"
    rbind_all <- rbind(no_1_uni,the_other_uni)
    rbind_all <- arrange(rbind_all, country)
    #filter the data based on the circle marker click event
    filter_rbind_all <- filter(rbind_all,country==input$mapPlot_marker_click[1])
    #transform the data from wide to long format
    change_shapes <- gather(filter_rbind_all, key = measurementfactor, value=measurement,2:7)
    #plot the bar chart
    p <- ggplot(change_shapes, aes(x = measurementfactor, y= as.numeric(measurement),
                    fill=school_name),
           color=as.factor(school_name))+ geom_bar(stat="identity",position = "dodge") + 
      scale_fill_brewer(palette ="RdPu")
    p <- p+ labs(
      x = "Measurements",
      y = "Mean Score",
      title = paste("Best University vs Other Universties in ", 
                    input$mapPlot_marker_click[1]))
    p <- p + theme_minimal()
    # Clear axis lines 
    p <- p + theme(panel.grid.minor = element_blank())
    p <- p + theme(panel.grid.major.y = element_blank())
    # position the legend at the bottom
    p <- p+theme(legend.position = "bottom")
    p
  })
  
  #render the scatter plot
  output$plot1 <- renderPlot({
    #based on the input chnaged the y-axis accordingly
    td_data2016 <- filteted_by_year()
    if (input$pIndicators == "Teaching") {
      y_axis <- td_data2016$teaching_new
    } else if (input$pIndicators == "International") {
      y_axis <- td_data2016$international_new
    } else if (input$pIndicators == "Research") {
      y_axis <- td_data2016$research_new
    } else if (input$pIndicators == "Citations") {
      y_axis <- td_data2016$citations_new
    } else if (input$pIndicators == "Income") {
      y_axis <- td_data2016$income_new
    }
    
    #the x-axis represent the overall_scores for the universities
    #y-axis represents the different performances indicators which user has chosen
    #the color is defined by the color
    #the size represents by the performance score. The better the university perform the bigger the dots on the plot
    p1 <-ggplot(td_data2016, 
                aes(y = y_axis,x = overall_new,color = country,size = y_axis)) + geom_point() + guides(size = FALSE)
    #add labels for x and y axises, title as well
    p2 <- p1 + labs(
      x = "Overall Scores",
      y = c(input$pIndicators),
      title = paste(input$pIndicators, " vs Overall Score")
    )
    #apply themes and colors
    p4 <- p2 + theme_bw() + theme2 +scale_color_brewer(palette="Paired")
    p4
  })
  
  #capture the scatterplot double click event
  observeEvent(input$plot1_dbclick, {
    ranges$x <- c(input$plot1_dbclick$x)
    ranges$y <- c(input$plot1_dbclick$y)
  })
  #plot the bar chart - to show universities from both counties in different rank_ranges
  output$plot2 <- renderPlot({
    #get 2016 data only
    td.2016 <- filter(timesData.all, timesData.all$Year==2016)
    #get a ranking_range, country frequency table
    df.filter <-
      ddply(td.2016, .(ranking_range, country), summarise, n = length(country))
    #filter the data based on the input country
    df.filter_by_country <-
      filter(
        df.filter,
        df.filter$country %in% c(input$asiaUni, input$theRestUni)
        & df.filter$n > 0
      )
    #create new column
    df.filter_by_country["ranking_n"] <- NA
    #assign the ranking_n based on the ranking_range e.g ranking_range 0-25 -> ranking_n will be 0
    df.filter_by_country$ranking_n <-
      as.integer(sapply(
        strsplit(df.filter_by_country$ranking_range, "-"),
        "[[",
        1
      ))
    #arrange the data based on the ranking_n
    df.filter_by_country <- arrange(df.filter_by_country, ranking_n)
    #this is for ordering in x-axis, it will order based on ranking_n
    df.filter_by_country$ranking_range_ordered <-  reorder(df.filter_by_country$ranking_range,
                                                       df.filter_by_country$ranking_n)
    #plot the bar chart -x-axis shows the ranking range, y-axis is the frequency count of number of universties fall into this rank
    # the color of the bar chart is based on the country
     p5 <-
      ggplot(
        df.filter_by_country,
        aes(
          x=ranking_range_ordered,
          y = as.numeric(df.filter_by_country$n),
          fill = country
        )
      ) +geom_bar(stat = "identity", position = "dodge") +theme_bw() + theme2+scale_fill_brewer(palette="Paired")
   
     #add labels and title for the bar chart
    p5+ labs(
      x = "University Rankings",
      y = "Number of School",
      title = paste("Rankings Comparision")
    )
  })
  #Render the time series plot for the universities from 2011-2015 based on the performance indicator
  output$tsplot <- renderPlot({
    #get ts data frame based on the selected university
    ts <- timesData.all[timesData.all$school_name %in% allselectedpoints()$school_name,]
    # print(paste("my dataset is: " ,ts))
    
    #generate y_axis based on the user selction
    if (input$pIndicators == "Teaching") {
      y_axis <- ts$teaching_new
    } else if (input$pIndicators == "International") {
      y_axis <- ts$international_new
    } else if (input$pIndicators == "Research") {
      y_axis <- ts$research_new
    } else if (input$pIndicators == "Citations") {
      y_axis <- ts$citations_new
    } else if (input$pIndicators == "Income") {
      y_axis <- ts$income_new
    }
    #plotting the line charts. The line color represent different school and the shape represent the country
    p <- ggplot(ts, aes(ts$Year, y_axis)) + geom_line(aes(color=school_name)) + geom_point(aes(shape=country), size=3)+
      theme_bw() + theme2 + theme(legend.position = "bottom") 
    #add the lables for x,y and title.
    p + labs(
      x = "Year",
      y = c(input$pIndicators),
      title = paste(input$pIndicators," from 2011-2016")
    )
  })
  #plot the parallel co-ordinate plot, this plot will change based 
  #on the user selection on the scatter plot
  output$ggparcoord <- renderPlot({
    #get data for 2016 based on the country
    td_data2016 <- filteted_by_year()
    #get the required data
    td.2016_part <- select(
      td_data2016,
      rank_int,
      ranking_range,
      school_name,
      country,
      teaching_new,
      international_new,
      research_new,
      citations_new,
      income_new,
      overall_new
    )
    #check if user select the multiple points on the scatterplot
    if (is.null(input$plot1_brush)){
      #check if user double clicked on one point on the scatter plot
      if (!is.null(ranges$x)){
        #get one point detail use nearPoints
        allpoints <- nearPoints(td.2016_part,
                                input$plot1_dbclick,
                                threshold = 10,
                                addDist = TRUE,
                                maxpoints = 1,
                                xvar = "overall_new",
                                yvar = y_value())
        #change column names
        names(allpoints)[5:10] <- c("Teaching","International","Research","Citations","Income","Overall")
        #transfer the data from wide to long format
        allpts_trans <- gather(allpoints, key = measurementfactor, 
                          value=measurement,5:10)
        #plotting the bar chart when there is only one point. x-axis shows Teaching, International, Research, Income, Citations
        #and overall scores of the universites. y-axis show the measurement values.
        ggplot(
          allpts_trans,
          aes(
            x = measurementfactor,
            y = as.numeric(measurement),width=.3, space = 2
          )
        ) +
          geom_bar(stat = "identity",fill="#FF9966") + theme1+ theme_bw()
      }
    }else{
      #when user selected multiple points on the scatterplot, use brushedPoints to get all the data
      allpoints <-  brushedPoints(td.2016_part,
                    input$plot1_brush,
                    xvar = "overall_new",
                    yvar = y_value())
      #use parallel co-ordinate plots to display multiple values. the color of the line represents  the school name
      #the shape is defined by the country.
      #change column names
      names(allpoints)[5:10] <- c("Teaching","International","Research","Citations","Income","Overall")
      p <- ggparcoord(allpoints,columns = 5:10, showPoints = TRUE,
                 mapping=aes(color=as.factor(school_name),shape=as.factor(as.character(country)))) +
      scale_color_discrete("Universities", labels = 
                             c(as.character(allpoints$school_name))) + 
        scale_shape_discrete(name="Country", labels=c(allpoints$country))
      
      p <- p + theme_minimal()
      # Decrease amount of margin around x, y values
      p <- p + scale_y_continuous(expand = c(0.02, 0.02))
      p <- p + scale_x_discrete(expand = c(0.02, 0.02))
      
      # Remove axis ticks and labels
      p <- p + theme(axis.ticks = element_blank())
      p <- p + theme(axis.title = element_blank())
      p <- p + theme(axis.text.y = element_blank())
      
      # Clear axis lines
      p <- p + theme(panel.grid.minor = element_blank())
      p <- p + theme(panel.grid.major.y = element_blank())
      # Darken vertical lines
      p <- p + theme(panel.grid.major.x = element_line(color = "#bbbbbb"))
      # Move label to bottom
      p <- p+theme(legend.position = "bottom")
      # Figure out y-axis range after GGally scales the data
      min_y <- min(p$data$value)
      max_y <- max(p$data$value)
      pad_y <- (max_y - min_y) * 0.1
      
      # Calculate label positions for each veritcal bar
      lab_x <- rep(1:6, times = 2) # 2 times, 1 for min 1 for max
      lab_y <- rep(c(min_y - pad_y, max_y + pad_y), each = 6)
      
      # Get min and max values from original dataset
      lab_z <- c(sapply(allpoints[, 5:10], min), sapply(allpoints[, 5:10], max))
      
      # Convert to character for use as labels
      lab_z <- as.character(lab_z)
      
      # Add labels to plot
      p <- p + annotate("text", x = lab_x, y = lab_y, label = lab_z, size = 3)
      p
    }
  })
})