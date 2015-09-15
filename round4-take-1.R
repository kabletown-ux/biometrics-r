debug <- TRUE
dataDir <- "data/labeled-by-vamsi/"

confusionSum1 <- table( c( "Real: A", "Real: B", "Real: C" ), c( "A", "B", "C" ) )
confusionSum2 <- table( c( "Real: A", "Real: B", "Real: C" ), c( "A", "B", "C" ) )
confusionPercent <- table( c( "Real: A", "Real: B", "Real: C" ), c( "A", "B", "C" ) )

processFiles <- function() {
    
    fileNames <- dir( dataDir, pattern = ".csv" )
    
    if ( debug ) print( paste( "fileNames.length()", length( fileNames ) ) )
    
    for ( i in 1:length( fileNames ) ) {
        
        processConfusionMatrix( fileNames[ i ] )
    }
    # calculate global percentages
    confusionPercent[ 1, 1 ] <<- confusionSum1[ 1, 1 ] / confusionSum2[ 1, 1 ]
    confusionPercent[ 2, 2 ] <<- confusionSum1[ 2, 2 ] / confusionSum2[ 2, 2 ]
    confusionPercent[ 3, 3 ] <<- confusionSum1[ 3, 3 ] / confusionSum2[ 3, 3 ]
    
    print( "====================================================================================" )
    print( "Clustering Baseline (Totals)" )
    print( confusionSum2 )
    print( "" );
    print( "Clustering Baseline accuracy" ) 
    print( calculateAccuracy( confusionSum2 ) )
    
    print( "" );
    print( "Clustering => Nuance (Totals)" )
    print( confusionSum1 )
    print( "" );
    print( "Clustering => Nuance accuracy" ) 
    print( calculateAccuracy( confusionSum1 ) )
    
    print( "" );
    print( "Cluster => Nuance is X% As Accurate as Clustering" )
    print( confusionPercent )
    print( "" );
    print( "====================================================================================" )
}
processConfusionMatrix <- function( fileName ) {
    
    rawData <- read.csv( paste( dataDir, fileName, sep = '' ) )
    
    #print( str( rawData ) )
    
    # create table: Rows 1st, columns 2nd
    confusion1 <- table( rawData$real.speaker, rawData$identified.as )#rawData$predicted.speaker )
    confusion2 <- table( rawData$real.speaker, rawData$predicted.speaker )
    
    # update global sums
    for ( i in 1:3 ) {
        
        for ( j in 1:3 ) {
            
            confusionSum1[ i, j ] <<- confusionSum1[ i, j ] + confusion1[ i, j ]
            confusionSum2[ i, j ] <<- confusionSum2[ i, j ] + confusion2[ i, j ]
        }
    }
    
    # update row names to reflect which one is real speaker
    for ( i in 1:3 ) {
        
        rownames( confusion1 )[ i ] <- paste( "R:", rownames( confusion1 )[ i ] )
        colnames( confusion1 )[ i ] <- paste( "NP:", colnames( confusion1 )[ i ] )
        
        rownames( confusion2 )[ i ] <- paste( "R:", rownames( confusion2 )[ i ] )
        colnames( confusion2 )[ i ] <- paste( "CP:", colnames( confusion2 )[ i ] )
    }
    
    # print it
    print( "##################################################################" )
    print( "Clustering baseline" ) #c( fileName, "Clustering baseline" ) )
    print( confusion2 )
    print( "" )
    print( "Clustering baseline accuracy" ) 
    print( calculateAccuracy( confusion2 ) )
    print( "" )
    
    print( "Clustering => Nuance" )#c( fileName, "Clustering => Nuance" ) )
    print( confusion1 )
    print( "" )
    print( "Clustering => Nuance accuracy" ) 
    print( calculateAccuracy( confusion1 ) )
}
calculateAccuracy <- function( matrix ) {
    
    # calculate percentages
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
loadData4 <- function( fileName ) {
    
    rawData4 <- read.csv( paste( "data/labeled-by-vamsi/", fileName, sep = '' ) )
    print( str( rawData4 ) )

    # coerce all factors to character: http://stackoverflow.com/questions/2851015/convert-data-frame-columns-from-factors-to-characters/2853231#2853231
    i <- sapply( rawData4, is.factor )
    rawData4[ i ] <- lapply( rawData4[ i ], as.character )
    
    # add column (with value of 1) for aggregation calculation
    rawData4$score <- 1
    
    rawData4 <<- rawData4   
    fileName <<- fileName
}
doAnalysis4 <- function() {
    
#     matches <- subset( rawData4, predicted.speaker == identified.as ) 
#     misses <- subset( rawData4, predicted.speaker != identified.as )
#     
#     missesAggregate <- aggregate( score ~ predicted.speaker + identified.as, misses, sum )
#     matchesAggregate <- aggregate( score ~ predicted.speaker + identified.as, matches, sum )
#     
#     bothAggregates <- rbind( missesAggregate, matchesAggregate )
#     sortedAggregate <- bothAggregates[ order( bothAggregates$predicted.speaker, -bothAggregates$score ), ]
#     
#     print( "Clustering => Nuance" )
#     print( sortedAggregate )
# 
#     matches <- subset( rawData4, predicted.speaker == real.speaker ) 
#     misses <- subset( rawData4, predicted.speaker != real.speaker )
#     
#     missesAggregate <- aggregate( score ~ predicted.speaker + real.speaker, misses, sum )
#     matchesAggregate <- aggregate( score ~ predicted.speaker + real.speaker, matches, sum )
#     
#     bothAggregates <- rbind( missesAggregate, matchesAggregate )
#     sortedAggregate <- bothAggregates[ order( bothAggregates$predicted.speaker, -bothAggregates$score ), ]
#     
#     print( "Clustering baseline for trios" )
#     print( sortedAggregate )
#     
    # table is muuuuuuch easier
    print( "##################################################################" )
    confusion <- table( rawData4$predicted.speaker, rawData4$identified.as )
    print( fileName )
    print( "" )
    print( "Clustering => Nuance" )
    print( "" )
    print( confusion )
    print( "" )
    print( "##################################################################" )
    confusion <- table( rawData4$predicted.speaker, rawData4$real.speaker )
    print( fileName )
    print( "" )
    print( "Clustering baseline for trios" )
    print( "" )
    print( confusion )
    print( "" )
    print( "##################################################################" )
}