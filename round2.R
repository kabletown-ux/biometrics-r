loadData <- function( fileName ) {
    
    rawData <<- read.csv( paste( "/Users/rruiz/Work/biometrics/python-src/data/2015.06.16-output/", fileName, sep = '' ) )
    
    ## update all speaker.test.score fields to "NA" if their comment field != "trained"
    #o <<- rawData$real.speaker[ rawData$real.speaker == "o", ]
    rawData$real.speaker[ rawData$real.speaker == "o" ] <- "o gabrielle borneman"
    #rawData$imposter.test.score[ rawData$training.comment != "trained" ] <- NA
    
    ## remove rows w/ NA.  
    #rawData <- rawData[ complete.cases( rawData ), ]
}