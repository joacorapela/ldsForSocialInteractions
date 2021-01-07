
require(plotly)

getPlotLogLik <- function(logLik, xlab="Iteration Number", ylab="Log Likelihood", title="") {
    data <- data.frame(iterNo=1:length(logLik), logLik=logLik)
    fig <- plot_ly(data=data, type='scatter', mode='lines+markers', x=~iterNo, y=~logLik)
    fig <- fig %>% layout(title=title, xaxis=list(title=xlab), yaxis=list(title=ylab))
    return(fig)
}

