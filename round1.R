loadData <- function( fileName ) {
    
    rawData <<- read.csv( paste( "/Users/rruiz/Work/biometrics/python-src/data/2015.06.16-output", fileName, sep = '' ) )
    
    ## update all speaker.test.score fields to "NA" if their comment field != "trained"
    rawData$speaker.test.score[ rawData$training.comment != "trained" ] <- NA
    rawData$imposter.test.score[ rawData$training.comment != "trained" ] <- NA
    
    ## remove rows w/ NA.  
    rawData <- rawData[ complete.cases( rawData ), ]
}

plotTestingFileLength <- function() {
    hist( rawData[ , 3 ], main = "Testing Voice Sample Lengths", xlab = "File length in seconds", ylab = "Number of files" )
}


plotAll <- function( fileName ) {
    loadData( fileName )
    plotTestingFileLength()
    plotAverageTestScores()
}

plotAverageTestScores <- function() {
    
    ## calculate *.test.score means, group by file.count
    msts <- aggregate( speaker.test.score~file.count, rawData, mean )
    mits <- aggregate( imposter.test.score~file.count, rawData, mean )
    
    # merge vertically
    names( msts )[ 2 ] <- "test.score"
    names( mits )[ 2 ] <- "test.score"
    bothSpeakers <- rbind( msts, mits )
    avgBoth <- aggregate( test.score~file.count, bothSpeakers, mean )
    
    ## plot speaker test score
    plotType <- "o"
    yRange <- c( -60, 80 )
    ##yRange <- c( -75, 50 )
    plot( msts, col=3, pch=1, ylim=yRange, xlim=c(1, 30), type=plotType, main="Average Voice Biometric Test Scores", xlab="Training Submissions", ylab="Voice Samples: Imposter vs. Speaker")
    
    ## overlay imposter test score
    points( mits, col=2, pch=1, type=plotType )
    
    ## overlay avg of both test scores
    points( avgBoth, col=1, pch=1, type=plotType )
    
    ## add a couple of horizontal lines
    hVals <- c( -40, -20, 0, 20, 40, 60 )
    ##hVals <- c( -60, -40, -20, 0, 20, 40 )
    abline( h = hVals, lty = 2, col = "gray" )
}
