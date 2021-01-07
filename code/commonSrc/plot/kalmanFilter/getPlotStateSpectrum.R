getPlotStateSpectrum <- function(B, tilte="", xlab="Real", ylab="Imaginary", circleLineColor="gray") {
    eigvals <- eigen(x=B, symmetric=FALSE, only.values=TRUE)$values
    dfEigvals <- data.frame(x=Re(eigvals), y=Im(eigvals))
    fig <- plot_ly(data=dfEigvals, type='scatter', mode='markers')
    fig <- fig%>%add_trace(x=~x, y=~y, showlegend=FALSE)
    fig <- fig%>%layout(title=title, 
                        xaxis=list(title=xlab), 
                        yaxis=list(title=ylab),
                        shapes=list(
                                # unfilled circle
                                list(type="circle",
                                     xref="x",
                                     yref="y", 
                                     x0=-1,
                                     x1=1,
                                     y0=-1,
                                     y1=1, 
                                     line_color=circleLineColor)))
    return(fig)
}
