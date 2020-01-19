#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
# https://r.789695.n4.nabble.com/Whiskers-on-the-default-boxplot-graphics-td2195503.html
# https://www.r-bloggers.com/whisker-of-boxplot/
# https://journals.sagepub.com/doi/pdf/10.1177/1536867X0900900309
# https://journals.sagepub.com/doi/pdf/10.1177/1536867X1301300214
# https://www.stata.com/support/faqs/graphics/box-plots-and-logarithmic-scales/
# https://stats.stackexchange.com/questions/112705/boxplots-with-lognormally-distributed-data
# https://www.r-graph-gallery.com/96-boxplot-with-jitter.html
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
    set.seed(12345)

    
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
              Therefore transforming the data often proves beneficial for visualisation.
              We fabricate data with the majority of values near zero and a few comparitively very high values.
              Here we use base R boxplots. When we select 'Show me the data!' we present all the data and add random noise 
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
                      min=3, max=500, step=1, value=100, ticks=FALSE),
          
          sliderInput("Whisker",
                      "Select the length of whiskers (multiples of the IQR)",
                      min=0, max=3, step=.5, value=1.5, ticks=FALSE),
          
          sliderInput("outliers",
                      "Highlight 'outliers'",
                      min=0, max=1, step=1, value=0, ticks=FALSE),
          
          sliderInput("dp",
                      "Show me the data!",
                      min=0, max=1, step=1, value=1, ticks=FALSE),
          
            
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
    
        
        h3("Figure 1 Left panel untransformed data, right panel natural log transformation labelled with antilogs"),
        
        p(strong("Boxplots are simple graphical characterisations of continuous variable. 
        Boxplots were proposed by Tukey, J.W. in his 1977 book 'Exploratory Data Analysis' (only in 1977!).
                  In this example, we re-express the data using a natural logarithmic transformation.
                  Next, prior to presentation, perform the boxplot calculations. So first, perform the natural
                  log transformation on the data. Secondly create a boxplot, 
                  that is calculate the median, the two hinges and the whiskers. 
                  Next, we plot the boxplot, and finally replace the axis log values with the antilog values.")) ,
                  
                   
                  p(strong("There are subtlties in the boxplot calculations.
                  Often you may hear it is said, 'the limits of the boxes represent Q1 and Q3 and the 
                  whiskers extend +/- 1.5 x Q3-Q1'. 
                  This is not nessarily true. There are many variations on the box plot, it is therefore better to explicitly state what is being presented.")) ,
        div(""),
        p(strong("I hope you agree the plot on the right gives a better understanding of the data distributions. 
                 In this programming exercise select 'Show me the data!' and deselect 'Highlight 'outliers''.")) ,
        


        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel("Wait...what? Aren't the whiskers different?", value=3, 
                 div(plotOutput("reg.plot2", width=fig.width, height=fig.height)),  
                 h3("Figure 2 Ignoring grouping, top panel untransformed data, bottom panel using natural log transformation"),
                 
                 h4("Lets first look at the relationship between the hinges and quartiles.") ,
                 p(strong("Boxplot stats for the untransformed data, as present in top panel:")) ,
                 div( verbatimTextOutput("table2")),
                 p(strong("Now summarise the same data. Q1 and Q3 may not match the hinges. Paraphrasing the 'box.plot.stats' help file...'The ‘hinges’ are versions of Q1 and Q3'. See what happens when n is odd. The hinges and the quartiles match.")) ,
                 div( verbatimTextOutput("table3")),
                 
                 p(strong("Now check out the boxplot stats on the log transformed data, before exponentiating (bottom panel):")) ,
                 div( verbatimTextOutput("table4")),
                 p(strong("In the same way summarise the log transformed data before exponentiating:")) ,
                 div( verbatimTextOutput("table5")),
                 
                 h4("Lets look at the whiskers.") ,
                 
                 p(strong("The length of the whiskers is typically set at 1.5 IQR, that is the 
                 whiskers extend to the most extreme data point which is no more than 1.5 times the interquartile 
                 range from the box. A value of zero causes the whiskers to extend to the data extremes. 
                          So it may not be as simple as saying the whiskers extend  1.5 IQR.")),
                 
                 
  
                 
                 h4("Look again at how to calculate the boxplot statistics when transforming") ,
                 
                 p(strong("Apply the whisker calcualtion rule on the scale used to draw the box plots. So when using a transformation the calculations must be on that scale. 
                 That is, scales should not be mixed, that is it would be wrong to calculate the whiskers based on 1.5 IQR rule on the raw scale and then log transform 
                          to plor the boxplot. The medians and hinges will be the logs of the original medians and hinges, the step (which determines the fences)
                 will change. 
                 That is different than merely drawing the original boxplot on a logarithmic scale")) ,
        
                 
                 p(strong("")),
                 
                
                  ) ,
       
      # tableOutput("table"),
      # div( verbatimTextOutput("table2")),
      # div( verbatimTextOutput("table3")),
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel("The data", value=3, 
                 div( verbatimTextOutput("table1")),
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
     Whisker<-   isolate(input$Whisker )
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
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    #y <- rgeom(n, .01)# rexp(n, .02)
    y <- rlnorm(n, .7, 1.5)# rexp(n, .02)
    
  # y <- sort(round(y))
    x <- factor(sample(3, length(y), repl = TRUE))
 
    #split( y , sample(3, length(y) , repl = TRUE) )
    
    #is.even <- function(x){ x %% 2 == 0 }
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    
    
    
    
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
      
      rangez <-    sample$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
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
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     
     #y <- c(abs(rnorm(n-4,1,29)), sample(50:150,4, replace=FALSE))# c(rchisq(n/2,.8)*3+1 rf(n, 123,2)  #rlnorm(n, .8, 1.5)# r rlnorm(n, .9, 1.9)#   rlnorm(n, .1, 0.3)#rexp(n, .09)
       #y <- sort(round(y))
     
    # y<- abs(rcauchy(n, location =1, scale = 1))*10+3 #rlnorm(n, );x<-5.2*(x-mean(x))/sd(x)+102
     
     y<- c(rbeta(n-4, 2,6)*35,  sample(50:150,4, replace=FALSE))
     x <- factor(sample(3, length(y), repl = TRUE))
      
      d <- data.frame(x=x, y=y)
      d$logy <- log(d$y) # log the data
      
      return(list(d=d , y=d$y, rangez=rangez))# rangez=rangez, outliers=outliers, n=n, dp=dp))
      
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
        A <-seq(from=0.001, to= 0.01, by=0.001)
        A<-1
        B <-seq(from= 0.01, to= 0.1, by=0.01)
        C <-seq(from=  0.1, to =1, by=.1)
        D <-seq(from=    1, to =10, by=1)
        E <-seq(from=    10, to =100, by=10)
        FF <-seq(from=   100, to =1000, by=100)
        
        tickz <- unique(c(A,B,C,D,E,FF))
        xlabz <- "Experimental Group"
        ylab. <- "Response"
        xlab. <- c(paste0("\nGroup 1\nn=",table(d$x)[1][[1]],""),
                   paste0("\nGroup 2\nn=",table(d$x)[2][[1]],""),
                   paste0("\nGroup 3\nn=",table(d$x)[3][[1]],"")   )

       
         
          par(mfrow=c(1,2))
 
        #  boxplot(d$y ~ d$x)
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., #log="y",
                  outline=outliers,
                   col=terrain.colors(4) , range=rangez,
                   ylim=c(0,max(d$y)), main=paste("Presenting data on untransformed scale, N=", input$N) ) 
         axis(1, at=1:3, labels=xlab., tick=FALSE)
          axis(2,   las=2)
         # grid(NA, NULL, col="cornsilk2", lty=6)
          panel.first = 
            c(grid(NA, NULL, col="cornsilk2", lty=6))

          
          
          par(new=TRUE) #repeating
          
          boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., #log="y",
                  outline=outliers,
                  col=terrain.colors(4) , range=rangez,
                  ylim=c(0,max(d$y)), main=paste("Presenting data on untransformed scale, N=", input$N) ) 
          
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

          # 
          # nbGroup <- nlevels(d$x)
          # text( 
          #   x=c(1:nbGroup), 
          #   y=    -1.5,#min(d$y)-2 ,#          $stats[nrow(boundaries$stats),] + 0.5,   
          #   paste("n = ",table(d$x),sep="")  
          # )
          
          
          
          
          
          
          
          }
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          if (min(d$y)<0.1) {
            
            low=0.01
            tickz <- unique(c( B,C,D,E,FF))
            
          } else {
              low=0.1
              tickz <- unique(c( C,D,E,FF))
              }
          
          
          if (max(d$y)>100) {up=1000}  else {up=100}
          
          
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,
                  outline=outliers,
                  col=terrain.colors(4) , range=rangez,
                  ylim=c(log(low),log(up)), main=paste("Presenting the same data; log the data with antilog scale, N=",input$N) )
          axis(1, at=1:3, labels=xlab., tick=FALSE)
          axis(2, at=ticks, labels=labs, las=2)
          #abline(h=ticks, col="cornsilk2", lty=6)
          
          panel.first = 
            c( abline(h=ticks, col="cornsilk2", lty=6))
          
          
          par(new=TRUE) #repeating so grid lines are ar back
          boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,
                  outline=outliers,
                  col=terrain.colors(4) , range=rangez,
                  ylim=c(log(low),log(up)), main=paste("Presenting the same data; log the data with antilog scale, N=",input$N) )
          
          
         # rug(x = 1:3, ticksize = 0.01, side = 1)  #ticks above line
          rug(x = log(tickz), ticksize = -0.01, side = 2)
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
          
          # nbGroup <- nlevels(d$x)
          # text( 
          #   x=c(1:nbGroup), 
          #   y=    log(low) ,#          $stats[nrow(boundaries$stats),] + 0.5,   
          #   paste("n = ",table(d$x),sep="")  
          # )
          
          #title(sub="hallo", adj=1, line=3, font=2)

          }
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
          # par(mfrow=c(1,1))
          # xx <- tapply(d$y, d$x, summary)
          # return(xx=xx)
        
    })
    #---------------------------------------------------------------------------
    
    output$reg.plot2 <- renderPlot({         
      
     
      d <- make.data2()$d
      
     rangez <-    input$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
     # rangez <- make.data2()$rangez
      
      outliers <-  input$outliers   
      #n<-          input$n  
      dp<-         input$dp 
      
      A <-seq(from=0.001, to= 0.01, by=0.001)
      B <-seq(from= 0.01, to= 0.1, by=0.01)
      C <-seq(from=  0.1, to =1, by=.1)
      D <-seq(from=    1, to =10, by=1)
      E <-seq(from=    10, to =100, by=10)
      FF <-seq(from=   100, to =1000, by=100)
      
      tickz <- unique(c(A,B,C,D,E,FF))
      
      ticks=c(log(c( 0.001, 0.01,  .1,  1,  10,  100,  1000)))
      labs <- exp(ticks)
      
      ylab. <- " "
      xlabz  <- "Response"
      xlab. <- c("Group 1","Group 2","Group 3")
      
      par(mfrow=c(2,1))
      
      d$x=1   # change
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      bs <- (boxplot.stats(d$y,coef = rangez)$stats)[c(1,3,5)]
      bs1 <-  (boxplot.stats(d$y,coef = rangez)$stats)[c(1,3,5)]
      
      bs0 <- (boxplot.stats(d$y,coef = rangez)$stats)[c(2,4)]
      bs2 <-  (boxplot.stats(d$y,coef = rangez)$stats)[c(2,4)]
       
      boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,   horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4)[3] , range=rangez,  width=10,
              ylim=c(0,max(d$y)*1.2), 
              main=paste("Presenting the data with the boxplot statistics, top the raw untransformed scale, bottom log transforming the same data, with an antilog scale, N=", input$N,"\n"))
     # axis(1, at=1:3, labels=xlab.)
      axis(1,   las=1)
       grid(  NULL, NA, col="cornsilk2", lty=7)
     # abline(v=ticks, col="cornsilk2", lty=6)
      text(x = p2(bs), labels = p2(bs1), y = 1.48)
      text(x = p2(bs0), labels = p2(bs2), y =  .53)
      par(new=TRUE) #repeating so grid lines are ar back
      boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,   horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4)[3] , range=rangez,  width=10,
              ylim=c(0,max(d$y)*1.2), 
              main=paste("Presenting the data with the boxplot statistics, top the raw untransformed scale, bottom log transforming the same data, with an antilog scale, N=", input$N,"\n"))
      
      
      if (dp==1) {
        cols <-  c(    "purple")
         
        # Add data points
        mylevels <- 1
        levelProportions <- .01 #summary(d$x)/nrow(d)
        for(i in 1:length(mylevels)){
          
          thislevel <- mylevels[i]
          thisvalues <- d[d$x==thislevel, 'y']
          library(scales)
          myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]*30)
          points( thisvalues, myjitter, pch=1, col = alpha(cols, 0.8) )   
          
        }
       
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      if (min(d$y)<0.1) {
        
        low=0.01
        tickz <- unique(c( B,C,D,E,FF))
        
      } else {
        low=0.1
        tickz <- unique(c( C,D,E,FF))
      }
      
      if (max(d$y)>100) {up=1000}  else {up=100}
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # bs <- (boxplot.stats(d$logy)$stats)
      # bs1 <- exp(boxplot.stats(d$logy)$stats)
      
      bs <- (boxplot.stats(d$logy,coef = rangez)$stats)[c(1,3,5)]
      bs1 <-  exp(boxplot.stats(d$logy,coef = rangez)$stats)[c(1,3,5)]
      
      bs0 <- (boxplot.stats(d$logy,coef = rangez)$stats)[c(2,4)]
      bs2 <-  exp(boxplot.stats(d$logy,coef = rangez)$stats)[c(2,4)]
      
      boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4) [3], range=rangez, width=10,
              ylim=c(log(low),log(up))) #, main=paste("Presenting the same data, but logging the data and with an antilog scale and including the box plot stats, N=",input$N ) )
      #axis(1, at=1:3, labels=xlab.)
       axis(1, at=ticks, labels=labs, las=1)
      abline(v=(ticks), col="cornsilk2", lty=7)
      rug(x = log(tickz), ticksize = -0.02, side = 1)
     # text(x = p2(bs), labels = p2(bs1), y = 1.48)
      
      text(x = p2(bs), labels = p2(bs1), y = 1.48)
      text(x = p2(bs0), labels = p2(bs2), y =  .53)
      
      
      par(new=TRUE)
      
      boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4) [3], range=rangez, width=10,
              ylim=c(log(low),log(up))) #, main=paste("Presenting the same data, but logging the data and with an antilog scale and including the box plot stats, N=",input$N ) )
      
      
      
      
      if (dp==1) {
        cols <-  c(    "purple")

        # Add data points
        mylevels <- 1
        levelProportions <- summary(d$x)/nrow(d)
        for(i in 1:length(mylevels)){

          thislevel <- mylevels[i]
          thisvalues <- d[d$x==thislevel, 'logy']

          # take the x-axis indices and add a jitter, proportional to the N in each level
         # myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]*30)
          points( thisvalues, myjitter, pch=1, col = alpha(cols, 0.8) )   
          #
        }
        
      }
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
   #   par(mfrow=c(1,1))
    #  xx <- tapply(d$y, d$x, summary)
     # return(d=d)
      
    })
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # lsting of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$table1 <- renderPrint({
      
      return(     make.data2()$d)
      
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$table2 <- renderPrint({
      
      dd <-  make.data2()$d 
      
      rangez <-    input$Whisker 
      
      bs <- boxplot.stats(dd$y, coef = rangez, do.conf = FALSE, do.out = FALSE)$stats
      bs <- as.numeric(p2(as.vector(bs)))
      names(bs ) <- c("Lower whisker", "Lower ‘hinge’", "Median", "Upper ‘hinge’" ,"Upper whisker")
 
      return(  print(bs, row.names = FALSE)) 
      
       
    })
    
    output$table3 <- renderPrint({
      
      dd <-  make.data2()$d 
      
     
       f <- summary(dd$y)
       f <- as.matrix(f);
       f <-p2(f)
       f <- as.numeric(f)
       f<-(p2(f))
       f<-as.data.frame(t(f))
       names(f ) <- c("Minimum", "1st.Quartile", "Median", "Mean", "3rd.Quartile", "Maximum")
      
     return( print(f, row.names = FALSE)) 
      
      
    })
    
    
    output$table4 <- renderPrint({
      
      dd <-  make.data2()$d 
      
      rangez <-    input$Whisker 
      
      bs <- boxplot.stats(dd$logy, coef = rangez, do.conf = FALSE, do.out = FALSE)$stats
      bs <- as.numeric(p2(as.vector(exp(bs))))
      names(bs ) <- c("Lower whisker", "Lower ‘hinge’", "Median", "Upper ‘hinge’" ,"Upper whisker")
      
      return(  print(bs, row.names = FALSE)) 
      
      
    })
    
    output$table5 <- renderPrint({
      
      dd <-  make.data2()$d 
      
      
      f <- summary(dd$logy)
      f <- as.matrix(f);
      f <-p2(f)
      f <- exp(as.numeric(f))
      f<-(p2(f))
      f<-as.data.frame(t(f))
      names(f ) <- c("Minimum", "1st.Quartile", "Median", "Mean", "3rd.Quartile", "Maximum")
      
      return( print(f, row.names = FALSE)) 
      
      
    })
    
    
    
    # output$table<- renderTable({  
    # 
    # 
    #    print(make.data2()$d)
    #   
    #  # summary(d)
    #   
    # })
    
     
    
    # output$table <- renderTable({
    #   t1()$resid
    # })
    
    
 
    
    
    
    
    
    
    
    
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