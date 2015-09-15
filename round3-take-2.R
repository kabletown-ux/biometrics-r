debug <- TRUE
dataDir <- "data/baseline-supervised/"

confusionSum1 <- table( c( "Real: A", "Real: B", "Real: C" ), c( "A", "B", "C" ) )

processFiles3 <- function() {
    
    fileNames <- dir( dataDir, pattern = ".csv" )
    
    if ( debug ) print( paste( "fileNames.length()", length( fileNames ) ) )
    
    for ( i in 1:length( fileNames ) ) {
        
        processConfusionMatrix( fileNames[ i ] )
    }
    
    print( "====================================================================================" )
    print( "Supervised Baseline (Totals)" )
    print( confusionSum1 )
    print( "" );
    print( "Supervised Baseline accuracy" ) 
    print( calculateAccuracy( confusionSum1 ) )
    print( "" );
    print( "====================================================================================" )
}
processConfusionMatrix <- function( fileName ) {
    
    rawData <- read.csv( paste( dataDir, fileName, sep = '' ) )
    
    #print( str( rawData ) )
    # coerce all factors to character: http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
    i <- sapply( rawData, is.factor )
    rawData[ i ] <- lapply( rawData[ i ], as.character )
    
    #print( str( rawData ) )
    
    # update levels first, a *must* for factors
    levels( rawData$real.speaker ) <- c( "o gabrielle borneman", "errinn rebollar", "novela soto", "russel isabell", "robert ryan" )
    
    #print( levels( rawData$real.speaker ) )
    
    # KLUDGE (due to mislabled speaker names) update field names: real.speaker
    rawData$real.speaker[ rawData$real.speaker == "o" ] <- "o gabrielle borneman"
    rawData$real.speaker[ rawData$real.speaker == "errinn" ] <- "errinn rebollar"
    rawData$real.speaker[ rawData$real.speaker == "novella" ] <- "novella soto"
    rawData$real.speaker[ rawData$real.speaker == "russel" ] <- "russel isabell"
    rawData$real.speaker[ rawData$real.speaker == "robert" ] <- "robert ryan"
    # KLUDGE (due to mislabled speaker names) update field names: predicted.speaker
    rawData$predicted.speaker[ rawData$predicted.speaker == "o" ] <- "o gabrielle borneman"
    rawData$predicted.speaker[ rawData$predicted.speaker == "errinn" ] <- "errinn rebollar"
    rawData$predicted.speaker[ rawData$predicted.speaker == "novella" ] <- "novella soto"
    rawData$predicted.speaker[ rawData$predicted.speaker == "russel" ] <- "russel isabell"
    rawData$predicted.speaker[ rawData$predicted.speaker == "robert" ] <- "robert ryan"
    
    # create table: Rows 1st, columns 2nd
    confusion1 <- table( rawData$real.speaker, rawData$identified.as )
    
    # update global sums
    for ( i in 1:3 ) {
        
        for ( j in 1:3 ) {
            
            confusionSum1[ i, j ] <<- confusionSum1[ i, j ] + confusion1[ i, j ]
        }
    }
    
    # update row names to reflect which one is real speaker
    for ( i in 1:3 ) {
        
        rownames( confusion1 )[ i ] <- paste( "R:", rownames( confusion1 )[ i ] )
        colnames( confusion1 )[ i ] <- paste( "NP:", colnames( confusion1 )[ i ] )
    }
    
    # print it
    print( "##################################################################" )
    print( "Supervised baseline" )
    print( confusion1 )
    print( "" )
    print( "Supervised baseline accuracy" ) 
    print( calculateAccuracy( confusion1 ) )
    print( "" )
}
calculateAccuracy <- function( matrix ) {
    
    # calculate percentages TODO: Use something like rowSum to get total predictions, instead of doing this by hand!
    matrix[ 1, 1 ] <- ( matrix[ 1, 1 ] / ( matrix[ 1, 1 ] + matrix[ 1, 2 ] + matrix[ 1, 3 ] ) )
    matrix[ 2, 2 ] <- ( matrix[ 2, 2 ] / ( matrix[ 2, 1 ] + matrix[ 2, 2 ] + matrix[ 2, 3 ] ) )
    matrix[ 3, 3 ] <- ( matrix[ 3, 3 ] / ( matrix[ 3, 1 ] + matrix[ 3, 2 ] + matrix[ 3, 3 ] ) )
    
    # zero out the non intersection points
    for ( i in 1:3 ) {
        
        for ( j in 1:3 ) {
            
            if ( i != j ) {
                matrix[ i, j ] <- 0   
            }
        }
    }
    matrix
}