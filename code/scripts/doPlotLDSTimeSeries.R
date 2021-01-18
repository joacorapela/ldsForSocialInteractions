require(plotly)

processAll <- function() {
    neuronToPlot <- 59
    sRate <- 1000
    timeSeriesFilename <- "../../results/firstMouse/binnedTimSeries_interaction01.RData"
    figFilenamePattern <- "../../figures/firstMouse/binnedTimSeries_interaction01_neuron%03d.%s"

    timeSeries <- get(load(timeSeriesFilename))
    behavioralTimeSeries <- timeSeries$behavioralTimeSeries
    times <- timeSeries$times

    figStim <- plot_ly(x=times, y=behavioralTimeSeries[1,], type="scatter", mode="lines", name=rownames(behavioralTimeSeries)[1], showlegend=TRUE)
    for(i in 2:nrow(behavioralTimeSeries)) {
        figStim <- figStim%>%add_trace(y=behavioralTimeSeries[i,], name=rownames(behavioralTimeSeries)[i], showlegend=TRUE)
    }
    figStim <- figStim%>%layout(xaxis=list(title="Time (sec)"), yaxis=list(title="Behavior On/Off"))

    spikeCounts <- timeSeries$spikeCounts
    figSpikeCounts <- plot_ly(x=times, y=spikeCounts[neuronToPlot, ], name="spikes", type="scatter", mode="markers")
    figSpikeCounts <- figSpikeCounts%>%layout(xaxis=list(title="Time (sec)"), yaxis=list(title="Spike Count"))

    allFigs <- list(figStim, figSpikeCounts)
    figFinal <- subplot(allFigs, nrows=length(allFigs), shareX=TRUE)

    pngFigFilename <- sprintf(figFilenamePattern, neuronToPlot, "png")
    htmlFigFilename <- sprintf(figFilenamePattern, neuronToPlot, "html")
    orca(p=figFinal, file=pngFigFilename)
    htmlwidgets::saveWidget(as_widget(figFinal), file.path(normalizePath(dirname(htmlFigFilename)), basename(htmlFigFilename)))
    print(figFinal)

    browser()
}

processAll()
rm(processAll)
