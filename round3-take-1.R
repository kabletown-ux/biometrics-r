loadData3 <- function( fileName ) {
    
    rawData3 <- read.csv( paste( "data/baseline-supervised/", fileName, sep = '' ) )
    #print( str( rawData3 ) )
    
    # coerce all factors to character: http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
    i <- sapply( rawData3, is.factor )
    rawData3[ i ] <- lapply( rawData3[ i ], as.character )
    
    print( str( rawData3 ) )
    
    # update levels first, a *must* for factors
    levels( rawData3$real.speaker ) <- c( "o gabrielle borneman", "errinn rebollar", "novela soto", "russel isabell", "robert ryan" )
    
    print( levels( rawData3$real.speaker ) )
    
    # update field names: real.speaker
    rawData3$real.speaker[ rawData3$real.speaker == "o" ] <- "o gabrielle borneman"
    rawData3$real.speaker[ rawData3$real.speaker == "errinn" ] <- "errinn rebollar"
    rawData3$real.speaker[ rawData3$real.speaker == "novella" ] <- "novella soto"
    rawData3$real.speaker[ rawData3$real.speaker == "russel" ] <- "russel isabell"
    rawData3$real.speaker[ rawData3$real.speaker == "robert" ] <- "robert ryan"
    # update field names: predicted.speaker
    rawData3$predicted.speaker[ rawData3$predicted.speaker == "o" ] <- "o gabrielle borneman"
    rawData3$predicted.speaker[ rawData3$predicted.speaker == "errinn" ] <- "errinn rebollar"
    rawData3$predicted.speaker[ rawData3$predicted.speaker == "novella" ] <- "novella soto"
    rawData3$predicted.speaker[ rawData3$predicted.speaker == "russel" ] <- "russel isabell"
    rawData3$predicted.speaker[ rawData3$predicted.speaker == "robert" ] <- "robert ryan"
    
    # add column for aggregation calculation
    #rawData3$score <- 1
    
    #rawData3 <<- rawData3   
    #rawData3
    
    # this is waaaaaay easier!
    print( "##################################################################" )
    print( fileName )
    print( "" )
    print( "Nuance Baseline for Trios: " )
    confusion <- table( rawData3$real.speaker, rawData3$identified.as )
    print( confusion )
    print( "" )
    print( "##################################################################" )
    
}
doAnalysis3 <- function() {
    
    print( "" )
    print( "Nuance Baseline for Trios" )
    confusion <- table( rawData3$real.speaker, rawData3$identified.as )
    print( confusion )
    
}