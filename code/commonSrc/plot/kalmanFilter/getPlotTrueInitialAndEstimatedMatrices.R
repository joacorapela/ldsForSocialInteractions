
require(plotly)
require(ggplot2)
require(RColorBrewer)

getPlotTrueInitialAndEstimatedMatrices <- function(x=NA, true=NA, initial=NA, estimated=NA, title="", xlab="Row Index", ylab="Value", trueLegendLabelPattern="true[%d,]", initialLegendLabelPattern="initial[%d,]", estimatedLegendLabelPattern="estimated[%d,]", trueLinetype="solid", initialLinetype="dot", estimatedLinetype="dash") {
    if(is.na(true[1]) && is.na(initial[1]) && is.na(estimated[1])) {
        error("At least one of true, initial or estimated must not be NA")
    }

    allData <- c()
    if(!is.na(true[1])) {
        trueRownames <- sprintf("true%d", 1:nrow(true))
        trueColnames <- sprintf("%d", 1:ncol(true))
        rownames(true) <- trueRownames
        colnames(true) <- trueColnames
        allData <- rbind(allData, true)
    }

    if(!is.na(initial[1])) {
        initialRownames <- sprintf("initial%d", 1:nrow(initial))
        initialColnames <- sprintf("%d", 1:ncol(initial))
        rownames(initial) <- initialRownames
        colnames(initial) <- initialColnames
        allData <- rbind(allData, initial)
    }

    if(!is.na(estimated[1])) {
        estimatedRownames <- sprintf("estimated%d", 1:nrow(estimated))
        estimatedColnames <- sprintf("%d", 1:ncol(estimated))
        rownames(estimated) <- estimatedRownames
        colnames(estimated) <- estimatedColnames
        allData <- rbind(allData, estimated)
    }
    allData <- data.frame(allData)
    if(is.na(x[1])) {
        x <- 1:ncol(allData)
    }

    # fig <- plot_ly(data=allMelted, x=~row, y=~value, linetype=~col, colour=~type, type='scatter', mode='markers')
    fig <- plot_ly(type='scatter', mode='lines+markers')
    if(!is.na(true[1])) {
        cols <- colorRampPalette(brewer.pal(9, "Set1"))(max(3, nrow(true)))
        for(j in 1:nrow(true)) {
            fig <- fig%>%add_trace(x=x, y=as.numeric(allData[sprintf("true%d", j),]), name=sprintf(trueLegendLabelPattern, j), line=list(color=cols[j], dash=trueLinetype), marker=list(color=cols[j]), type="scatter", mode="lines+markers")
        }
    }
    if(!is.na(initial[1])) {
        cols <- colorRampPalette(brewer.pal(9, "Set1"))(max(3, nrow(initial)))
        for(j in 1:nrow(initial)) {
            fig <- fig%>%add_trace(x=x, y=as.numeric(allData[sprintf("initial%d", j),]), name=sprintf(initialLegendLabelPattern, j), line=list(color=cols[j], dash=initialLinetype), marker=list(color=cols[j]), type="scatter", mode="lines+markers")
        }
    }
    if(!is.na(estimated[1])) {
        cols <- colorRampPalette(brewer.pal(9, "Set1"))(max(3, nrow(estimated)))
        for(j in 1:nrow(estimated)) {
            fig <- fig%>%add_trace(x=x, y=as.numeric(allData[sprintf("estimated%d", j),]), name=sprintf(estimatedLegendLabelPattern, j), line=list(color=cols[j], dash=estimatedLinetype), marker=list(color=cols[j]), type="scatter", mode="lines+markers")
        }
    }
    # fig <- fig %>% layout(title=title, xaxis=list(title=xlab, tickvals=1:nrow(allData)), yaxis=list(title=ylab))
    fig <- fig %>% layout(title=title, xaxis=list(title=xlab), yaxis=list(title=ylab))
    return(fig)
}

