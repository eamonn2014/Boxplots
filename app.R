#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    library(ggplot2)
    library(shiny) 
    library(nlme)
    library(VCA)
    options(max.print=1000000)
    fig.width <- 1200
    fig.height <- 550
    library(shinythemes)        # more funky looking apps
    p1 <- function(x) {formatC(x, format="f", digits=1)}
    p2 <- function(x) {formatC(x, format="f", digits=2)}
    options(width=100)
    set.seed(123)

    
    # xlabz <- "Experimental Group"
    # ylab. <- "Response"
    # xlab. <- c("Group 1","Group 2","Group 3")
    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"),
             
                shinyUI(pageWithSidebar(

#ui <-shinyUI(pageWithSidebar(
    
    headerPanel("Plotting on the log scale but labelling with antilogs"),
    
    #sidebarLayout(  #new
      # Sidebar with a slider and selection inputs
   
    sidebarPanel( 
     
        div(p("This is a simple app to show the benefits of using a transformation on skewed data when plotting.
        Imagine randomly allocating subjects to 3 groups, treating them differently and then comparing them with 
        respect to a measured laboratory response. Laboratory tests quantify the presence of analytes and 
        therefore we do not expect negative values. 
              Very often the data will be skewed, with a minority of very high values. 
              Therefore transforming the data will often prove beneficial for visualisation.
              We fabricate data with the majority of values near zero and a few comparitively very high values.
              Here we use base R boxplots. When we select 'Show me the data!' we present all the data and add random noise horizontally
              to shift all the data points for better visualisation. ")),
        
        div(
       
         
      #    br(),
          
         # actionButton("do", "Click Me"),
          actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                       onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/One-way-ANOVA/master/app.R', '_blank')"),   
          actionButton("resample", "Simulate a new sample"),
          br(), br(),
          
  
            div(strong("Select the parameters using the sliders below"),p(" ")),

            
            div(("Both the number of data points and the length of the whiskers can be varied. 
                 The default whisker length is 1.5xIQR extending below Q1 and above Q3. Selecting zero will 
                 extend the whiskers to the maximum and minimum.
                 There is the option to show all the data on the plot 'Show me the data!'. 
                 Could you guess what the data would look like?
                 No, the same boxplot can be constructed from many different data sets. 
                 Therefore if possible and feasible also plot the raw data. There is also the option
                 to 'Highlight 'outliers'', that is include the 'outliers'. Be careful as plotting the raw data also will double up the 'outlying'
                 data points. Therefore if you are programming boxplots and showing the raw data, turn off the 
                 outlier option. This advice is also pertinent if generating boxplots using the ggplot2 package.
                 Another sample can be taken from the same data generating mechanism by clicking 'Simulate a new sample'.")),
            br(),
          
       
             
          sliderInput("N",
                      "Select the total number of data points",
                      min=3, max=198, step=3, value=99, ticks=FALSE),
          
          sliderInput("Whisker",
                      "Select the length of whiskers (multiples of the IQR)",
                      min=0, max=3, step=.5, value=1.5, ticks=FALSE),
          
          sliderInput("outliers",
                      "Highlight 'outliers'",
                      min=0, max=1, step=1, value=1, ticks=FALSE),
          
          sliderInput("dp",
                      "Show me the data!",
                      min=0, max=1, step=1, value=0, ticks=FALSE),
          
            
               )
    ),
    
     #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
      mainPanel(
        
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #    tabsetPanel(type = "tabs", 
      navbarPage(       
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
        tags$style(HTML(" 
                            .navbar-default .navbar-brand {color: cyan;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: lightgrey;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}
                   ")), 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end of section to add colour     
        tabPanel("Plotting the data", 
        
        div(plotOutput("reg.plot", width=fig.width, height=fig.height)),  
    
        p(strong("I hope you agree the plot on the right gives a better understanding of the data distributions. 
                 In this programming exercise select 'Show me the data!' and deselect 'Highlight 'outliers''.")) ,
        
       div( verbatimTextOutput("summary"))  
        
        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel("Wait...what? But aren't the whiskers different?", value=3, 
                 div(plotOutput("reg.plot2", width=fig.width, height=fig.height)),  
        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel(" ", value=3, 

        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel(" ", 

            #     div(plotOutput("residual", width=1200, height=800)) ,
        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel(" ", 
                 
        #   div( verbatimTextOutput("summary2")),

        )
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        )
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        )
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
 
    )
  )
)



server <- shinyServer(function(input, output   ) {
    
 #  observeEvent(input$do, {
 #    
 #    
 #    random.sample <- reactive({
 #      
 #  
 # foo <- input$resample
 #    
 # 
 #    })
 #    
 #    
 #  })
 #  
  
  # checkedGroups <- eventReactive(input$goButton, {input$checkgroup})
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated only random noise is required to be generated
    random.sample <- reactive({

        # Dummy line to trigger off button-press
     foo <- input$resample
     Whisker<-  isolate(input$Whisker )
     outliers <- isolate(input$outliers )
     n=(input$N )
     dp=isolate(input$dp )
     
      #  whisker <- input$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
      # outliers <- input$outliers     # show or hide
    #    n <- input$N                   # divisible by 3
       # dp<- input$dp                  # show or hide data points

     return(list( n=n ,  Whisker=Whisker , outliers=outliers, dp=dp ))

    })
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    # make.regression <- reactive({
    #      
    #     sample <- random.sample()
    #     
    #     whisker <- sample$whisker        
    #   #  outlier#s <- sample$outliers   
    #     n <- sample$n   
    #    # dp<- sample$dp   
    #     
    #     return(list(   n=n,  whisker=whisker ))#, outliers=outliers, dp=dp   ))
    #     
    # })  
    
    make.data <- reactive({
      #      
     sample <- random.sample()
    
  #  data1 <- random.sample()
    # Get the current data
    #data1 <- make.regression()
    
    # rangez <-    data1$whisker       # multiples of IQR length of whiskers, 0 means out to maximum
    # outliers <-  data1$outliers   
  n<-          sample$n  
    # dp<-         data1$dp
     # 
     # whisker<-  isolate(input$Whisker )
     # outliers <- isolate(input$outliers )
     # n=isolate(input$N )
     # dp=isolate(input$dp )
     # 
    
    outlierz <- 3 
    sds <- runif(outlierz,5,9)                          # create the data
    high1 <- sample(75:99, outlierz-1, replace=T) 
    high2 <- sample(1:199, outlierz-1, replace=T)       # create v high values
    high<-c(high1, high2)
    
    N<- (n-3)/3
    y <- c( abs(rnorm(N,2,sds[1])) ,high[1] ,  
            abs(rnorm(N,2,sds[2])) ,high[2],
            abs(rnorm(N,2,sds[1])) ,high[3] )
    
    x <- factor(rep(1:3, each=n/3))
    
    d <- data.frame(x=x, y=y)
    d$logy <- log(d$y) # log the data
    
    return(list(d=d ))# rangez=rangez, outliers=outliers, n=n, dp=dp))
    
    })
    
    make.data2 <- reactive({
      #      
      sample <- random.sample()
      
      #  data1 <- random.sample()
      # Get the current data
      #data1 <- make.regression()
      
      # rangez <-    data1$whisker       # multiples of IQR length of whiskers, 0 means out to maximum
      # outliers <-  data1$outliers   
      n<-          sample$n  
      # dp<-         data1$dp
      # 
      # whisker<-  isolate(input$Whisker )
      # outliers <- isolate(input$outliers )
      # n=isolate(input$N )
      # dp=isolate(input$dp )
      # 
      
      outlierz <- 3 
      sds <- runif(outlierz,5,9)                          # create the data
      high1 <- sample(75:99, outlierz-1, replace=T) 
      high2 <- sample(1:199, outlierz-1, replace=T)       # create v high values
      high<-c(high1, high2)
      
      N<- (n-3)/3
      mu=15
      y <- c( abs(rnorm(N,mu,sds[1])) ,high[1] ,  
              abs(rnorm(N,mu,sds[2])) ,high[2],
              abs(rnorm(N,mu,sds[1])) ,high[3] )
      
      x <- factor(rep(1:3, each=n/3))
      
      d <- data.frame(x=x, y=y)
      d$logy <- log(d$y) # log the data
      
      return(list(d=d ))# rangez=rangez, outliers=outliers, n=n, dp=dp))
      
    })
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    output$reg.plot <- renderPlot({         
        
      
       d <- make.data()$d
       
       rangez <-    input$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
       outliers <-  input$outliers   
       #n<-          input$n  
       dp<-         input$dp 
      
        ticks=c(log(0.001),log(0.01), log(.1), log(1), log(10), log(100), log(1000))
        labs <- exp(ticks)
        
        xlabz <- "Experimental Group"
        ylab. <- "Response"
        xlab. <- c("Group 1","Group 2","Group 3")
       
          par(mfrow=c(1,2))
 
        #  boxplot(d$y ~ d$x)
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., #log="y",
                  outline=outliers,
                   col=terrain.colors(4) , range=rangez,
                   ylim=c(0,max(d$y)), main=paste("Presenting the data on untransformed scale, N=", input$N) )
         axis(1, at=1:3, labels=xlab.)
          axis(2,   las=2)
          grid(NA, NULL, col="cornsilk2", lty=6)


          if (dp==1) {

          # Add data points
          mylevels <- levels(d$x)
          levelProportions <- summary(d$x)/nrow(d)
          for(i in 1:length(mylevels)){

            thislevel <- mylevels[i]
            thisvalues <- d[d$x==thislevel, 'y']

            myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
            points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9))

          }

          }
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (min(d$y)<0.1) {low=0.01} else {low=0.1}
          if (max(d$y)>100) {up=1000}  else {up=100}
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,
                  outline=outliers,
                  col=terrain.colors(4) , range=rangez,
                  ylim=c(log(low),log(up)), main=paste("Presenting the same data; log the data with antilog scale, N=",input$N) )
          axis(1, at=1:3, labels=xlab.)
          axis(2, at=ticks, labels=labs, las=2)
          abline(h=ticks, col="cornsilk2", lty=6)

          if (dp==1) {
          # Add data points
          mylevels <- levels(d$x)
          levelProportions <- summary(d$x)/nrow(d)
          for(i in 1:length(mylevels)){

            thislevel <- mylevels[i]
            thisvalues <- d[d$x==thislevel, 'logy']

            # take the x-axis indices and add a jitter, proportional to the N in each level
            myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
            points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9))
          #
          }

          }
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
          par(mfrow=c(1,1))
          xx <- tapply(d$y, d$x, summary)
          return(xx=xx)
        
    })
    #---------------------------------------------------------------------------
    
    output$reg.plot2 <- renderPlot({         
      
     
      d <- make.data2()$d
      
      rangez <-    input$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
      outliers <-  input$outliers   
      #n<-          input$n  
      dp<-         input$dp 
      
      ticks=c(log(0.001),log(0.01), log(.1), log(1), log(10), log(100), log(1000))
      labs <- exp(ticks)
      
      ylab. <- " "
      xlabz  <- "Response"
      xlab. <- c("Group 1","Group 2","Group 3")
      
      par(mfrow=c(2,1))
      
      d$x=1   # change
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      bs <- (boxplot.stats(d$y)$stats)[c(1,3,5)]
      bs1 <-  (boxplot.stats(d$y)$stats)[c(1,3,5)]
      
      bs0 <- (boxplot.stats(d$y)$stats)[c(2,4)]
      bs2 <-  (boxplot.stats(d$y)$stats)[c(2,4)]
      
      boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,   horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4)[3] , range=rangez,  width=10,
              ylim=c(0,max(d$y)*1.2), main=paste("Presenting the data on untransformed scale with the box plot stats, N=", input$N))
     # axis(1, at=1:3, labels=xlab.)
      axis(1,   las=1)
     # grid(NA, NULL, col="cornsilk2", lty=6)
      
      text(x = p2(bs), labels = p2(bs1), y = 1.48)
      text(x = p2(bs0), labels = p2(bs2), y =  .53)
      if (dp==1) {
        
        
        cols <-  c(    "purple")
         
        # Add data points
        mylevels <- 1
        levelProportions <- summary(d$x)/nrow(d)
        for(i in 1:length(mylevels)){
          
          thislevel <- mylevels[i]
          thisvalues <- d[d$x==thislevel, 'y']
          library(scales)
          myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]*30)
          points( thisvalues, myjitter, pch=20, col = alpha(cols, 0.4) )   
          
        }
       
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (min(d$y)<0.1) {low=0.01} else {low=0.1}
      if (max(d$y)>100) {up=1000}  else {up=100}
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # bs <- (boxplot.stats(d$logy)$stats)
      # bs1 <- exp(boxplot.stats(d$logy)$stats)
      
      bs <- (boxplot.stats(d$logy)$stats)[c(1,3,5)]
      bs1 <-  exp(boxplot.stats(d$logy)$stats)[c(1,3,5)]
      
      bs0 <- (boxplot.stats(d$logy)$stats)[c(2,4)]
      bs2 <-  exp(boxplot.stats(d$logy)$stats)[c(2,4)]
      
      boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4) [3], range=rangez, width=10,
              ylim=c(log(low),log(up)), main=paste("Presenting the same data; log the data with antilog scale with the box plot stats, N=",input$N ) )
      #axis(1, at=1:3, labels=xlab.)
       axis(1, at=ticks, labels=labs, las=1)
      #abline(h=ticks, col="cornsilk2", lty=6)
     # text(x = p2(bs), labels = p2(bs1), y = 1.48)
      
      text(x = p2(bs), labels = p2(bs1), y = 1.48)
      text(x = p2(bs0), labels = p2(bs2), y =  .53)
      
      if (dp==1) {
        cols <-  c(    "purple")
        
        # Add data points
        mylevels <- 1
        levelProportions <- summary(d$x)/nrow(d)
        for(i in 1:length(mylevels)){
          
          thislevel <- mylevels[i]
          thisvalues <- d[d$x==thislevel, 'logy']
          
          # take the x-axis indices and add a jitter, proportional to the N in each level
          myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]*30)
          points( thisvalues, myjitter, pch=20, col = alpha(cols, 0.4) )   
          #
        }
        
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      par(mfrow=c(1,1))
      xx <- tapply(d$y, d$x, summary)
      return(xx=xx)
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    #--------------------------------------------------------------------------
    #---------------------------------------------------------------------------
    # Plot residuals 
    
   # output$summary <- reactive({     
   # 
   #     d  <- reg.plot()
   #   
   #    x<- tapply(d$y, d$x, summary)
   #  
   # })
    
    #---------------------------------------------------------------------------
 # Show the summary for the 
  #  output$reg.summary <- renderPrint({
        
        # summary <- fit.regression()$fit.summary
        # 
        # if (!is.null(summary)) {
        #     
        #     return(fit.regression()$fit.summary)
        # }
        
   # })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # the data to print
    #output$summary2 <- renderPrint({
      
     # return(make.regression()$dd)
      
    #})
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # the data to print, I wooulf like to reuse this but dont think it is possible? So I add another function to collect the same information below
    #output$byhand <- renderPrint({
      
   #   return(explain()$ANOVA)

    #})
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     output$summary <- renderPrint({
   
  
    
    #return(list( reg.plot()$xx)) 
     })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)