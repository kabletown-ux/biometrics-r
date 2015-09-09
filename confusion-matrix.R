library( ggplot2 )

gen1 <- function() {
    
    #generate random data 
    #data = data.frame( sample(LETTERS[0:20], 100, replace=T),sample(LETTERS[0:20], 100, replace=T) )
    data = data.frame( sample(LETTERS[0:3], 100, replace=T),sample(LETTERS[0:3], 100, replace=T) )
    names(data) = c("Actual", "Predicted") 
    
    #compute frequency of actual categories
    actual = as.data.frame(table(data$Actual))
    names(actual) = c("Actual","ActualFreq")
    
    #build confusion matrix
    confusion = as.data.frame(table(data$Actual, data$Predicted))
    names(confusion) = c("Actual","Predicted","Freq")
    
    #calculate percentage of test cases based on actual frequency
    confusion = merge(confusion, actual, by=c("Actual"))
    confusion$Percent = confusion$Freq/confusion$ActualFreq*100
    
    #render plot
    # we use three different layers
    # first we draw tiles and fill color based on percentage of test cases
    tile <- ggplot() +
        geom_tile(aes(x=Actual, y=Predicted,fill=Percent),data=confusion, color="black",size=0.1) +
        labs(x="Actual",y="Predicted")
    tile = tile + 
        geom_text(aes(x=Actual,y=Predicted, label=sprintf("%.1f", Percent)),data=confusion, size=3, colour="black") +
        scale_fill_gradient(low="white",high="red")
    
    # lastly we draw diagonal tiles. We use alpha = 0 so as not to hide previous layers but use size=0.3 to highlight border
    tile = tile + 
        geom_tile(aes(x=Actual,y=Predicted),data=subset(confusion, as.character(Actual)==as.character(Predicted)), color="black",size=0.6, fill="black", alpha=0) 
    
    #render
    tile
}
gen2 <- function() {
    
    z = c(10,0,0,
          0,8,0,
          0,0,5)
    
    z = matrix( z, ncol=3 )
    colnames(z) = c("a","b","c" )#,"d","e","f","g","h","i", "j")
    rownames(z) = c("a","b","c" )#,"d","e","f","g","h","i", "j")
    
#     heatmap(t(z)[ncol(z):1,], Rowv=NA, Colv=NA, col = heat.colors(256))
    
    ##To get the correct image plot rotation we need to flip the plot
    image( z[,ncol(z):1], axes=FALSE )
    #image( z[,1:ncol(z)], axes=FALSE )
    #image( z[,], axes=FALSE )
    
    ##Add in the y-axis labels. Similar idea for x-axis.
    axis(2, at = seq( 0, 1, length=length( colnames( z ))), labels=colnames(z) ) #left
    axis(3, at = seq( 0, 1, length=length( rownames( z ))), labels=rownames(z) ) #top
}
gen3 <- function() {
    
    classes=c("Underweight", "Normal", "Overweight")
    # Confusion matrix
    Observations <- bmi_classification(cross.m$bmi)
    Predicted <- bmi_classification(cross.m$cvpred)
    
    conf <- table(Predicted, Observations)
    
    library(caret) 
    f.conf <- confusionMatrix(conf)
    print(f.conf)
}