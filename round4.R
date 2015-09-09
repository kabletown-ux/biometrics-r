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
#     
#     # table is muuuuuuch easier
#     print( "##################################################################" )
#     confusion <- table( rawData4$predicted.speaker, rawData4$identified.as )
#     print( fileName )
#     print( "" )
#     print( "Clustering => Nuance" )
#     print( "" )
#     print( confusion )
#     print( "" )
#     print( "##################################################################" )
#     confusion <- table( rawData4$predicted.speaker, rawData4$real.speaker )
#     print( fileName )
#     print( "" )
#     print( "Clustering baseline for trios" )
#     print( "" )
#     print( confusion )
#     print( "" )
#     print( "##################################################################" )
    
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