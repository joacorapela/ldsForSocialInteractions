
getPlotPercentageExplainedVar <- function(percExpVar, labels=NA, xlab="Neuron Index", ylab="Percentage of Explained Variance") {
    sortRes <- sort(percExpVar, index.return=TRUE)
    sortedPercExpVar <- sortRes$x
    if(is.na(labels)) {
        labels <- sprintf("%d", 1:nrow(percExpVar))
    }
    sortedLabels <- factor(labels[sortRes$ix], levels=labels[sortRes$ix])
    df <- data.frame(labels=sortedLabels, percExpVar=sortedPercExpVar)
    fig <- plot_ly(df, x=~labels, y=~percExpVar, type="bar")
    fig <- fig%>%layout(yaxis=list(title=ylab), xaxis=list(title=xlab))
    return(fig)
}
