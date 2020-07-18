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
    p1 <- function(x) {formatC(x, format="f", digits=1)}
    p2 <- function(x) {formatC(x, format="f", digits=2)}
    options(width=100)
    set.seed(12345) #reproducible


     n<-N<-100 
     rangez <- Whisker <-1.5
     outliers = "No"
     dp  = "Yes"
   
    y <- rlnorm(n, .7, 1.5) 
    x <- factor(sample(3, length(y), repl = TRUE))
  
    d <- data.frame(x=x, y=y)
    d$logy <- log(d$y) # log the data
    
    

      y<- c(rbeta(n-4, 2,6)*35,  sample(50:150,4, replace=FALSE))
      x <- factor(sample(3, length(y), repl = TRUE))
      
      d <- data.frame(x=x, y=y)
      d$logy <- log(d$y) # log the data
         
        
        outliers <- ifelse(outliers %in% "Yes",1,0)
       
    
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
                   ylim=c(0,max(d$y)), main=paste("Presenting data on untransformed scale, N=", N) ) 
         axis(1, at=1:3, labels=xlab., tick=FALSE)
          axis(2,   las=2)
         # grid(NA, NULL, col="cornsilk2", lty=6)
          panel.first = 
            c(grid(NA, NULL, col="cornsilk2", lty=6))


          par(new=TRUE) #repeating
          
          boxplot(d$y ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,
                  outline=outliers,
                  col=terrain.colors(4) , range=rangez,
                  ylim=c(0,max(d$y)), main=paste("Presenting data on untransformed scale, N=", N) ) 
          
          if (dp=="Yes") { 

          # Add data points
          mylevels <- levels(d$x)
          levelProportions <- summary(d$x)/nrow(d)
          for(i in 1:length(mylevels)){

            thislevel <- mylevels[i]
            thisvalues <- d[d$x==thislevel, 'y']

            myjitter <- jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
            points(myjitter, thisvalues, pch=20, col=rgb(0,0,0,.9))
 
          } }
 
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
                  ylim=c(log(low),log(up)), main=paste("Presenting the same data; log the data with antilog scale, N=",N) )
          axis(1, at=1:3, labels=xlab., tick=FALSE)
          axis(2, at=ticks, labels=labs, las=2)
          #abline(h=ticks, col="cornsilk2", lty=6)
          
          panel.first = 
            c( abline(h=ticks, col="cornsilk2", lty=6))
          
          par(new=TRUE) #repeating so grid lines are ar back
          boxplot(d$logy ~ d$x, xaxt="n", yaxt="n", xlab=xlabz, ylab=ylab.,
                  outline=outliers,
                  col=terrain.colors(4) , range=rangez,
                  ylim=c(log(low),log(up)), main=paste("Presenting the same data; log the data with antilog scale, N=",N) )
          
          
         # rug(x = 1:3, ticksize = 0.01, side = 1)  #ticks above line
          rug(x = log(tickz), ticksize = -0.01, side = 2)
          if (dp=="Yes") {
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
    #---------------------------------------------------------------------------
    
     # multiples of IQR length of whiskers, 0 means out to maximum
     # rangez <- make.data2()$rangez
      
   
      outliers <- ifelse(outliers %in% "Yes",1,0)
      

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
              main=paste("Presenting the data with the boxplot statistics, top the raw untransformed scale, bottom log transforming the same data, with an antilog scale, N=", N,"\n"))
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
              main=paste("Presenting the data with the boxplot statistics, top the raw untransformed scale, bottom log transforming the same data, with an antilog scale, N=", N,"\n"))
      
      if (dp=="Yes") {
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
      
      if (dp=="Yes") {
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
        
      
      foo<-  d
      
      names(foo) <- c("Country", "Population 100,000s")
      
      rownames(foo) <- NULL
      
      library(DT)
      
      datatable(foo,   
                
                rownames = TRUE,
                
                options = list(
                  searching = TRUE,
                  pageLength = 160,
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
          columns= c("Population 100,000s"), digits=c(2)  )
    
    
    
    
    
    
    
    
    
    
     