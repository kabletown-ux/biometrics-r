loadData <- function( fileName ) {
    
    rawData <- read.csv( paste( "data/", fileName, sep = '' ) )
    #print( str( rawData ) )
    
    # coerce all factors to character: http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
    i <- sapply( rawData, is.factor )
    rawData[ i ] <- lapply( rawData[ i ], as.character )
    
    #print( str( rawData ) )
    
    # update levels first, a *must* for factors
    levels( rawData$real.speaker ) <- c( "o gabrielle borneman", "errinn rebollar", "novela soto", "russel isabell", "robert ryan" )
    
    #print( levels( rawData$real.speaker ) )
    
    # update field names: real.speaker
    rawData$real.speaker[ rawData$real.speaker == "o" ] <- "o gabrielle borneman"
    rawData$real.speaker[ rawData$real.speaker == "errinn" ] <- "errinn rebollar"
    rawData$real.speaker[ rawData$real.speaker == "novella" ] <- "novella soto"
    rawData$real.speaker[ rawData$real.speaker == "russel" ] <- "russel isabell"
    rawData$real.speaker[ rawData$real.speaker == "robert" ] <- "robert ryan"
    # update field names: predicted.speaker
    rawData$predicted.speaker[ rawData$predicted.speaker == "o" ] <- "o gabrielle borneman"
    rawData$predicted.speaker[ rawData$predicted.speaker == "errinn" ] <- "errinn rebollar"
    rawData$predicted.speaker[ rawData$predicted.speaker == "novella" ] <- "novella soto"
    rawData$predicted.speaker[ rawData$predicted.speaker == "russel" ] <- "russel isabell"
    rawData$predicted.speaker[ rawData$predicted.speaker == "robert" ] <- "robert ryan"
    
    #rawData <<- rawData
}