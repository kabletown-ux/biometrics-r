loadData <- function( fileName ) {
    
    rawData <- read.csv( paste( "/Users/rruiz/Work/biometrics/python-src/data/2015.06.16-output/", fileName, sep = '' ) )
    
    ## update all speaker.test.score fields to "NA" if their comment field != "trained"
    rawData$speaker.test.score[ rawData$training.comment != "trained" ] <- NA
    rawData$imposter.test.score[ rawData$training.comment != "trained" ] <- NA
    
    ## remove rows w/ NA.  
    rawData <<- rawData[ complete.cases( rawData ), ]
}

plotAll <- function( fileName ) {
    loadData( fileName )
    plotTestingFileLength()
    plotAverageTestScores()
    plotTestScores1()
    plotTestScores2()
    plotTestScores3()
    plotTestScores4()
}

plotTestingFileLength <- function() {
    hist( rawData[ , 3 ], main = "Testing Voice Sample Lengths", xlab = "File length in seconds", ylab = "Number of files" )
}

plotAverageTestScores <- function() {
    
    ## calculate *.test.score means, group by file.count
    msts <- aggregate( speaker.test.score~file.count, rawData, mean )
    mits <- aggregate( imposter.test.score~file.count, rawData, mean )
    
    # merge vertically
    #     names( msts )[ 2 ] <- "test.score"
    #     names( mits )[ 2 ] <- "test.score"
    #     bothSpeakers <- rbind( msts, mits )
    #     avgBoth <- aggregate( test.score~file.count, bothSpeakers, mean )
    #     
    
    ## set margins?
    
    ## plot speaker test score
    plotType <- "o"
    yRange <- c( -60, 80 )
    #yRange <- c( -75, 50 )
    plot( msts, col=3, pch=1, ylim=yRange, xlim=c(1, 30), type=plotType, main="Average Voice Biometric Test Scores\n for 8 Speakers, Trained & Tested in Groups of 1", xlab="Voice Samples Submitted for Training", ylab="Nuance Score")
    
    ## overlay imposter test score
    points( mits, col=2, pch=1, type=plotType )
    
    ## overlay avg of both test scores
    #points( avgBoth, col=1, pch=1, type=plotType )
    
    ## add a couple of horizontal lines
    hVals <- c( -40, -20, 0, 20, 40, 60 )
    #hVals <- c( -60, -40, -20, 0, 20, 40 )
    abline( h = hVals, lty = 2, col = "gray" )
    
    legend( 'topleft', legend=c( "Speaker", "Imposter" ), lty=1, col=c('green', 'red'), bty='y', cex=1 )
}
plotTestScores1 <- function() {
    
    library( ggplot2 )
    
    qplot( file.count, speaker.test.score, data = rawData, color = speaker ) +
        geom_line() +
        geom_point() +
        stat_summary( linetype = "dashed", size = 2, fun.y = mean, geom = "line", color = "black")
}
plotTestScores2 <- function() {
    
    library( ggplot2 )
    
    # ignore katrina
    sevenOfEight = subset( rawData, speaker != "katrina houlahan" )
    
    qplot( file.count, speaker.test.score, data = sevenOfEight, color = speaker ) +
        geom_line() +
        geom_point() +
        stat_summary( linetype = "dashed", size = 2, fun.y = mean, geom = "line", color = "black")
}
plotTestScores3 <- function() {
    
    library( ggplot2 )
    
    ## does a boxplot for each file iteration, all 8 speakers
    qplot( factor( file.count ), speaker.test.score, data = rawData, geom = "boxplot" ) #+ geom_jitter()
}
plotTestScores4 <- function() {
    
    library( ggplot2 )
    
    # ignore katrina
    sevenOfEight = subset( rawData, speaker != "katrina houlahan" )
    
    ## does a boxplot for each file iteration, all 8 speakers
    qplot( factor( file.count ), speaker.test.score, data = sevenOfEight, geom = "boxplot" ) #+ geom_jitter()
}
