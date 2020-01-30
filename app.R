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
    set.seed(12345) #reproducible


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"),
             
                shinyUI(pageWithSidebar(

#ui <-shinyUI(pageWithSidebar(
    
    headerPanel("Presenting boxplots on a transformed scale"),
    
    #sidebarLayout(  #new
      # Sidebar with a slider and selection inputs
   
    sidebarPanel( 
     
        div(p("This is a simple app to show the benefits of using a transformation on skewed data when plotting, specifically using boxplots [1].
        Imagine randomly allocating subjects to 3 groups, treating them differently and then comparing them with 
        respect to a measured laboratory response. Laboratory tests quantify the presence of analytes and 
        therefore we do not expect negative values. 
              Very often the data will be skewed, with a minority of very high values. 
              Therefore transforming the data often proves beneficial for visualisation.
              We fabricate data with the majority of values near zero and a few comparitively very high values.
              Here we use base R boxplots [2]. When we select 'Show me the data!' we present all the data and add random noise 
              to shift all the data points for better visualisation. ")),
        
        div(
       
       
          actionButton(inputId='ab1', label="R code",   icon = icon("th"), 
                       onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Boxplots/master/app.R', '_blank')"),   
          actionButton("resample", "Simulate a new sample"),
          br(), br(),
          
  
            div(strong("Select the parameters using the sliders below"),p(" ")),

            
            div(("Both the number of data points and the length of the whiskers can be varied. 
                 The default whisker length uses the common 1.5xIQR rule. Selecting zero will 
                 extend the whiskers to the extreme datapoints.
                 There is the option to show all the data on the plot 'Show me the data!'. 
                 Could you guess what the data would look like?
                 Unlikely, it is possible the same boxplot can be constructed from many different data sets [3].
                 Therefore if possible and feasible also plot all the data points. There is also the option
                 to 'Highlight 'outliers'', that is present the 'outliers'. Be careful as plotting the individual data points also will double up the 'outlying'
                 data points. Therefore if you are programming boxplots and showing the individual data, turn off the 
                 outlier option. This advice is also pertinent if generating boxplots using the ggplot2 package [4].
                 Another sample can be taken from the same data generating mechanism by clicking 'Simulate a new sample'.")),
            br(),
          
          sliderInput("N",
                      "Select the total number of data points",
                      min=3, max=500, step=1, value=100, ticks=FALSE),
          
          sliderInput("Whisker",
                      "Select the length of whiskers (multiples of the IQR)",
                      min=0, max=3, step=.5, value=1.5, ticks=TRUE),
          
          sliderInput("outliers",
                      "Highlight 'outliers'",
                      min=0, max=1, step=1, value=0, ticks=FALSE),
          
          sliderInput("dp",
                      "Show me the data!",
                      min=0, max=1, step=1, value=1, ticks=FALSE),
          
          div(p("References:")),  
          
          tags$a(href = "https://en.wikipedia.org/wiki/Exploratory_data_analysis", "[1] Tukey, J.W. 'Exploratory Data Analysis'"),
          div(p(" ")),
          tags$a(href = "https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/boxplot", "[2] R boxplot"),
          div(p(" ")),
          tags$a(href = "https://en.wikipedia.org/wiki/Anscombe%27s_quartet", "[3] Anscombe's quartet"),
          div(p(" ")),
          tags$a(href = "https://ggplot2.tidyverse.org/reference/geom_boxplot.html", "[4] Boxplots using ggplot2"),
          div(p(" ")),
          tags$a(href = "https://r.789695.n4.nabble.com/Whiskers-on-the-default-boxplot-graphics-td2195503.html", "[5] Whiskers on the default boxplot"),
          div(p(" ")),
          tags$a(href = "https://www.wiley.com/en-us/Understanding+Robust+and+Exploratory+Data+Analysis-p-9780471384915", "[6] Understanding Robust and Exploratory Data Analysis"),
          div(p(" ")),
          
          
          
          
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
        
        p(strong("Boxplots are simple graphical characterisations of continuous variables. 
        Boxplots were proposed by Tukey, J.W. in his 1977 book 'Exploratory Data Analysis' (only in 1977!).
                  In this example, we re-express the data using a natural logarithmic transformation.
                  Next, prior to presentation, perform the boxplot calculations. So first, perform the natural
                  log transformation on the data. Secondly create a boxplot, 
                  that is calculate the median, the two hinges and the whiskers. 
                  Then we present the boxplot, finally replace the axis log values with the antilog values.")) ,
                  
                   
                  p(strong("There are subtleties in the boxplot calculations.
                  Often you may hear it is said, 'the limits of the boxes represent Q1 and Q3 and the 
                  whiskers extend +/- 1.5 x Q3-Q1'. 
                  This is not necessarily true. There are many variations on the box plot, it is therefore better to explicitly state what is being presented.")) ,
        div(""),
        p(strong("I hope you agree the plot on the right gives a better understanding of the data distributions. 
                 In this programming exercise select 'Show me the data!' and deselect 'Highlight 'outliers''.")) ,

        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel("Wait...what? Aren't the whiskers different?", value=3, 
                 div(plotOutput("reg.plot2", width=fig.width, height=fig.height)),  
                 h3("Figure 2 Top panel untransformed data, bottom panel using a natural log transformation"),
                 
                 h4("Lets first look at the relationship between the hinges vrs. quartiles.") ,
                 p(strong("Boxplot stats for the untransformed data, as presented in the top panel:")) ,
                 div( verbatimTextOutput("table2")),
                 p(strong("Now summarise the same data. Q1 and Q3 may not match the hinges. Paraphrasing the 'box.plot.stats' help file...'The ‘hinges’ are versions of Q1 and Q3'. See what happens when n is odd. The hinges and the quartiles match.")) ,
                 div( verbatimTextOutput("table3")),
                 
                 p(strong("Now check out the boxplot stats based on the log transformed data, after exponentiating (bottom panel), check with the raw median:")) ,
                 div( verbatimTextOutput("table4")),
                 p(strong("In the same way summarise the log transformed data then exponentiating: ")) ,
                 div( verbatimTextOutput("table5")),
               
                 h4("Look again at how to calculate the boxplot statistics when transforming") ,
                 
                 p(strong("The length of the whiskers is typically set at 1.5xIQR, that is, the 
                 whiskers extend to the most extreme data point which is no more than 1.5 times the interquartile 
                 range from the box. A value of zero, using the R function, causes the whiskers to extend to the data extremes. 
                          So it may not be as simple as saying the whiskers extend 1.5xIQR.")),
                 
                 
                 div(""),
                 
                 p(strong("To summarise, apply the whisker calculation rule on the scale used to draw the boxplot. Therefore when using a transformation 
                 the calculations must 
                 be on the transformed scale. Do not mix scales. So, do not calculate the whiskers based on the 1.5xIQR rule
                 on the raw scale and then log transform 
                          to present the boxplot.  
                          The median and hinges will be the logs of the original median and hinges, they will match with odd sample sizes, but not generally with even. 
                          The step which determines the length of the whiskers will change. Hence the fences generally will differ. 
                          (The location of the end of the whiskers is referred to as the fences). ")) ,
        
                 
                 p(strong("")),
                 h3("Take home messages; when the sample size is even, the hinges may not necessarily equal Q1 and Q3 (raw and transformed) and the transformed median may be slightly different to the raw median.
                      The fences may not necessarily equal the fences on the raw scale for any sample size.
                    Explicitly state how you constructed your boxplots.")
                                  ) ,
  
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel("The data", value=3, 
                 div( verbatimTextOutput("table1")),
        ) ,
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        tabPanel("Another Example 1967 World Almanac data", 
                 div(plotOutput("reg.plot3", width=fig.width, height=fig.height)),  
                 
                 p(strong( "'The 1967 World Almanac lists 16 countries that have 10 or more large cities,
                 the dataset we use consist of the 10 largest cities. The dataset gives rise to many questions 
                 i) How do the median populations of major cities compare across countries? 
                 Are the smallest cities in China larger than the largest cities of some other countries? 
                 Do the countries having larger cities tend to show more variabilty in city populations? 
                 Which cities are outliers relative to other cities in their own coutries?")),
                 div(""),
p(strong("
We order the countries by the median city population for the 10 largest cities.  

The top left figure present the raw data. Due to the presentation according to median we see the spread tends to increase 
as the level does which does not support equal variability. When variabilty is equal 
across batches this often simplifies further analysis. To promote equality of spread and
reduce the dependency of spread on level, we will re-express (transform) the data.

Log transformations are monotonic for positive data values, the order statistics 
of the transformed data will equal the transformed original order statistics
(except for rounding and interporlation as we have seen). However after we 
recalculate the IQR and the outlier cut-offs, the cities that were originally 
high-side outliers may no longer be and some that were not outliers may now be 
low side outliers, though this is unlikely as we have the the ten highest.

The log transformation has boxes which are more equally spread, similar
                           in length and the remaining inequality does not seem to be much related to length. 
                           The outliers are pulled in also. Of the 19 outliers in the raw scale, 
                           eight cities are no longer outliers and the other have moved 
                           relatively closer to the upper outlier cutoffs. 
                           The transformation does improve the data in important ways.'
                 
                 This is Paraphrasing from 'Understanding Robust and Exploratory Data Analysis', 1983 [6].")),
div(""),
p(strong("The top left panel presents boxplots on the raw scale. The top right shows what happens when all 
you do is log the top left plot. This is not the preferred approach. The bottom left and bottom 
right are the preferred approach described above re-expressing the data using the natural log and log10 transformation.")),
),

        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                 tabPanel("Data listing 1967 World Almanac", value=3, 

                          # DT::dataTableOutput("table99"),
                          
                          
                          fluidRow(
                            column(
                             DT:: dataTableOutput(outputId = "table99"), width = 6)
                          )
                          
                          
                          
                 ) 
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        )
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        )
       #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
 
    )
  )
)

server <- shinyServer(function(input, output   ) {


    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({

    # Dummy line to trigger off button-press
     foo <- input$resample
     Whisker<-   isolate(input$Whisker )
     outliers <- isolate(input$outliers )
     n=(input$N )
     dp=isolate(input$dp )
     
     return(list( n=n ,  Whisker=Whisker , outliers=outliers, dp=dp )) 

    })

    
    make.data <- reactive({
             
     sample <- random.sample()
     n<-  sample$n  

    y <- rlnorm(n, .7, 1.5) 
    x <- factor(sample(3, length(y), repl = TRUE))
  
    d <- data.frame(x=x, y=y)
    d$logy <- log(d$y) # log the data
    
    return(list(d=d ))# rangez=rangez, outliers=outliers, n=n, dp=dp))
    
    })
    
    make.data2 <- reactive({
   
      sample <- random.sample()
      rangez <-    sample$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
      n<-          sample$n  

      y<- c(rbeta(n-4, 2,6)*35,  sample(50:150,4, replace=FALSE))
      x <- factor(sample(3, length(y), repl = TRUE))
      
      d <- data.frame(x=x, y=y)
      d$logy <- log(d$y) # log the data
      
      return(list(d=d , y=d$y, rangez=rangez))# 
      
    })
    # --------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    output$reg.plot <- renderPlot({         
        
      
       d <- make.data()$d
       
       rangez <-    input$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
       outliers <-  input$outliers   
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
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., 
                  outline=outliers,
                   col=terrain.colors(4) , range=rangez,
                   ylim=c(0,max(d$y)), main=paste("Presenting data on untransformed scale, N=", input$N) ) 
         axis(1, at=1:3, labels=xlab., tick=FALSE)
          axis(2,   las=2)
         # grid(NA, NULL, col="cornsilk2", lty=6)
          panel.first = 
            c(grid(NA, NULL, col="cornsilk2", lty=6))


          par(new=TRUE) #repeating
          
          boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,
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
       
          }
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
       
    })
    #---------------------------------------------------------------------------
    
    output$reg.plot2 <- renderPlot({         
      
      d <- make.data2()$d
      
     rangez <-    input$Whisker       # multiples of IQR length of whiskers, 0 means out to maximum
     # rangez <- make.data2()$rangez
      
      outliers <-  input$outliers   
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
 
      bs <- (boxplot.stats(d$logy,coef = rangez)$stats)[c(1,3,5)]
      bs1 <-  exp(boxplot.stats(d$logy,coef = rangez)$stats)[c(1,3,5)]
      
      bs0 <- (boxplot.stats(d$logy,coef = rangez)$stats)[c(2,4)]
      bs2 <-  exp(boxplot.stats(d$logy,coef = rangez)$stats)[c(2,4)]
      
      boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab., horizontal = TRUE, axes = FALSE, staplewex = 1,
              outline=outliers,
              col=terrain.colors(4) [3], range=rangez, width=10,
              ylim=c(log(low),log(up))) 
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
              ylim=c(log(low),log(up))) 
      
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
      
    })
 
    
    
    tukey.data <- reactive({
      
      country <- as.character( c("Sweden ", 
                                 "Holland ", "Canada ", "France ", "Mexico ", "Argentina ", "Spain ", "UK ",
                                 "W.Germ. ", 
                                 "Brazil ", "USSR ", "Japan ", 
                                 "US ", "India ", "China ", "Italy ") )
      
      Country <- rep(country, each=10)
      Population = c(7.87,   4.22, 2.49, 0.94, 0.89, 0.87, 0.81, 0.78, 0.71, 0.69, 8.68, 7.31, 
                     6.02, 2.64, 1.75, 1.72, 1.51, 1.42, 1.31, 1.29, 11.91, 6.72, 
                     3.84, 2.81, 2.73, 2.68, 2.65, 2.49, 1.71, 1.69, 28.11, 7.83, 
                     5.35, 3.3, 2.94, 2.54, 2.46, 2.33, 2.03, 1.99, 31.18, 10.12, 
                     8.06, 3.79, 3.46, 2.91, 2.71, 2.17, 2.06, 1.86, 29.66, 7.61, 
                     6.35, 4.1, 3.8, 2.75, 2.7, 2.69, 2.51, 2.44, 25.99, 16.96, 5.01, 
                     4.74, 3.57, 3.34, 3.12, 2.64, 2.14, 1.69, 79.86, 11.02, 7.22, 
                     6.38, 5.09, 4.88, 4.3, 3.3, 3.1, 2.99, 21.92, 18.56, 11.42, 8.27, 
                     7.28, 7.02, 6.94, 6.53, 5.84, 5.66, 49.81, 38.57, 9.68, 9.52, 
                     8.08, 8.03, 6.99, 5.02, 4.95, 2.78, 63.34, 36.36, 13.32, 11.37, 
                     10.9, 10.84, 10.7, 10.27, 9.5, 9.17, 110.21, 32.14, 18.88, 16.39, 
                     13.37, 11.95, 10.7, 7.89, 7.71, 7.04, 77.81, 35.5, 24.79, 20.02, 
                     16.7, 9.39, 9.38, 8.76, 7.63, 7.5, 45.37, 30.03, 22.98, 20.62, 
                     17.25, 16.11, 11.49, 9.47, 9.07, 7.21, 69, 40.1, 36.92, 32.2, 
                     24.11, 21.46, 21.21, 16.5, 15, 11.13,
                     23.59,15.8,11.82,11.14,7.84,5.9,4.54,4.44,3.62,3.36
      )
      
      d <- data.frame(Country=Country, Population=Population)
      
      return(list(d=d  )) 
      
    })
    
    
    
    
    
    
    output$reg.plot3 <- renderPlot({         
      
      d <- tukey.data()$d
   
      
      
      median.r <- rank(tapply(d$Population, d$Country, median))   # rank by median
      labels <- names(sort(median.r))                               # sort and get the names for labels
      d$Country <- factor(d$Country , levels=c(labels))             # relevel factor levels
      n <- length(d$Population)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      par(mfrow=c(2,2))
      
      A<- boxplot( data=d, Population~Country, horizontal=TRUE,  yaxt="n",ann=FALSE, #  ylab=ylab.,
                   outline=1,
                   col=rev(rainbow(16)) , range=1.5, 
                   main=paste("Presenting the data \nuntransformed; , N=",n ) ) 
      text(y=1:16, par("usr")[1] +.1, srt =0, adj = 1,
           labels = labels, xpd = TRUE)
      text(y=18, x=5, srt =0, adj = 0,
           labels = paste("Presenting the data untransformed, N=",n ), xpd = TRUE)
      text(y=-4, x=1, srt =0, adj = 0,
           labels = "Population in 100,000s", xpd = TRUE)
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      
      B <- boxplot(data=d, Population~Country,   log="x", horizontal=TRUE,  yaxt="n",ann=FALSE,
                   
                   outline=1,
                   col=rev(rainbow(16)) , range=1.5, #terrain.colors(1) 
                   main=paste("Presenting the  data \nusing log option, N=",n ) ) 
      text(y=1:16, par("usr")[1] +.82, srt =0, adj = 1,          #+.48
           labels = labels, xpd = TRUE)
      text(y=18, x=1, srt =0, adj = 0,
           labels = paste("Using the R log option - incorrect, N=",n ), xpd = TRUE)
      text(y=-4, x=1, srt =0, adj = 0,
           labels = "Population in 100,000s", xpd = TRUE)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ticks=log(c(1,2,5,10,20,50,100 ))
      labs <- exp(ticks)
      p <- (d$Population)
      Co <- d$Country
      C <-boxplot(log(p)~Co,      horizontal=TRUE,  xaxt="n",ann=FALSE,yaxt="n",
                  outline=1,
                  col=rev(rainbow(16)) , range=1.5, 
                  ylim=c( log(.8), log(100) ), 
                  main=paste("Presenting the same data\n natural log transf. the data antilog scale, N=",n ) )  
      text(y=1:16, par("usr")[1] +.01, srt =0, adj = 1,    ##1 -.89
           labels = labels, xpd = TRUE)
      text(y=18, x=0, srt =0, adj = 0,
           labels = paste("Presenting the data using nat log, N=",n ), xpd = TRUE)
      axis(1, at=ticks, labels=labs, las=1)
      text(y=-4, x=0, srt =0, adj = 0,
           labels = "Population in 100,000s", xpd = TRUE)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      ticks=log10(c( 1,2,5,10,20,50,100 ))
      labs <- 10^(ticks)
      
      D<-boxplot(log10(p)~Co,     horizontal=TRUE,  xaxt="n",ann=FALSE,yaxt="n",
                 outline=1,
                 col=rev(rainbow(16)), range=1.5, 
                 ylim=c( log10(.8), log10(100) ), 
                 main=paste("Presenting the same data\n log10 transf. the data antilog scale, N=",n ) )  
      text(y=1:16, par("usr")[1] +.00, srt =0, adj = 1,    #-.38 0
           labels = labels, xpd = TRUE)
      text(y=18, x=0, srt =0, adj = 0,
           labels = paste("Presenting the data using log10, N=",n ), xpd = TRUE)
      axis(1, at=ticks, labels=labs, las=1)
      
      text(y=-4, x=0, srt =0, adj = 0,
           labels = "Population in 100,000s", xpd = TRUE)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
      par(mfrow=c(1,1))
      
      
      
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$table99 <- DT::renderDataTable({
      
      foo<- tukey.data()$d
      
      names(foo) <- c("Country", "Population 100,000s")
      
      rownames(foo) <- NULL
      
      library(DT)
      
      datatable(foo,   
                
                rownames = TRUE,
                
                options = list(
                  searching = TRUE,
                  pageLength = input$V-1,
                  paging=FALSE,
                  lengthMenu = FALSE ,
                  lengthChange = FALSE,
                  autoWidth = TRUE
                  # colReorder = TRUE,
                  # deferRender = TRUE,
                  # scrollY = 200,
                  # scroller = T
                ))  %>%
        formatRound(
          columns= c("Country", "Population 100,000s"), digits=c(0,2)  )
    })
    
    
    
    
    
    
    
    
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    # listing of simulated data
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$table1 <- renderPrint({
      
      return(make.data2()$d)
      
    })
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$table2 <- renderPrint({
      
      dd <-  make.data2()$d 
      
      rangez <-    input$Whisker 
      
      bs <- boxplot.stats(dd$y, coef = rangez, do.conf = FALSE, do.out = FALSE)$stats
      bs <- as.numeric((as.vector(bs))) #p2
      names(bs ) <- c("Lower whisker", "Lower ‘hinge’", "Median", "Upper ‘hinge’" ,"Upper whisker")
 
      return(  print(bs, row.names = FALSE)) 

    })
    
    output$table3 <- renderPrint({
      
      dd <-  make.data2()$d 
      
       f <- summary(dd$y)[c(2,3,5)]
       f <- as.matrix(f);
       #f <-p2(f)   # here
       f <- as.numeric(f)
      # f<-(p2(f))
       f<-as.data.frame(t(f))
   #    names(f ) <- c("Minimum", "1st.Quartile", "Median", "Mean", "3rd.Quartile", "Maximum")
       names(f ) <- c( "1st.Quartile", "Median",  "3rd.Quartile")
       
     return( print(f, row.names = FALSE)) 
      
    })
    
    
    output$table4 <- renderPrint({
      
      dd <-  make.data2()$d 
      
      rangez <-    input$Whisker 
      
      bs <- boxplot.stats(dd$logy, coef = rangez, do.conf = FALSE, do.out = FALSE)$stats
      bs <- as.numeric((as.vector(exp(bs))))  #p2
      names(bs ) <- c("Lower whisker", "Lower ‘hinge’", "Median", "Upper ‘hinge’" ,"Upper whisker")
      
      return(  print(bs, row.names = FALSE)) 
      
    })
    
    output$table5 <- renderPrint({
      
      dd <-  make.data2()$d 
      
      f <- summary(dd$logy)[c(2,3,5)]
      f <- as.matrix(f);
    # f <-p2(f)
      f <- exp(as.numeric(f))
    #  f<-(p2(f))
      f<-as.data.frame(t(f))
     # names(f ) <- c("Minimum", "1st.Quartile", "Median", "Mean", "3rd.Quartile", "Maximum")
      names(f ) <- c( "1st.Quartile", "Median",  "3rd.Quartile")
      return( print(f, row.names = FALSE)) 

    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
})

# Run the application 
shinyApp(ui = ui, server = server)