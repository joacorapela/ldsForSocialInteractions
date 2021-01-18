require(plotly)
require(reticulate)
require(yaml)
require(tools)
source("../commonSrc/plot/getBehaviorsRecs.R")
np <- import("numpy")

processAll <- function() {
    behaviorsNames <- c("nonsocial", "approach", "following", "headhead", "headtail", "conspecific")
    behaviorsColors <- c("black", "red", "green", "blue", "cyan", "orange")
    nNeuronsToPlot <- 131
    sRate <- 1000
    interaction <- 1
    markerColor <- "rgb(128,128,128)"
    markerSize <- 3
    behaviorsRecsOpacity <- 0.3
    parametersFilename <- "../../../data/120120/Parameters_13042019.yml"
    spikesSamplesPath <- "../../../data/120120/Neurons_BLA"
    spikesSamplesPattern <- "*.npy"
    boutTimesPath <- "../../../data/120120/Behavior"
    boutTimesPatternPattern <- "^.*_int%d_bouttimes.npz"
    figFilenamePattern <- "../figures/rasterPlotAndBehavior_interaction%d.%s"

    sortSpikesSamplesFilenames <- function(filenames) {
        N <- length(filenames)
        filenamesNumbers <- rep(NA, times=N)
        filenamesLetters <- rep(NA, times=N)
        for(i in 1:N) {
            filename <- filenames[i]
            filenameName <- file_path_sans_ext(filename)
            filenamesNumbers[i] <- as.numeric(substring(filenameName, 1, nchar(filenameName)-1))
            filenamesLetters[i] <- substring(filenameName, nchar(filenameName)-1, nchar(filenameName))
        }
        sortIndices <- order(filenamesNumbers, filenamesLetters)
        sortedFilenames <- filenames[sortIndices]
        return(sortedFilenames)
    }

    parameters <- read_yaml(file=parametersFilename)
    interactionStartTime <- parameters[["InteractionStart"]][interaction]
    interactionStopTime <- parameters[["InteractionStop"]][interaction]

    boutTimesPattern <- sprintf(boutTimesPatternPattern, interaction)
    boutTimesFilenames <- list.files(path=boutTimesPath, pattern=boutTimesPattern)

    spikesSamplesFilenames <- list.files(path=spikesSamplesPath, pattern=spikesSamplesPattern)
    sortedSpikesSamplesFilenames <- sortSpikesSamplesFilenames(filenames=spikesSamplesFilenames)
    fig <- plot_ly(type="scatter", mode="markers")
    # for(n in 1:length(sortedSpikesSamplesFilenames)) {
    for(n in 1:nNeuronsToPlot) {
        spikesSamplesFilename <- sortedSpikesSamplesFilenames[[n]]
        spikesSamplesFullFilename <- file.path(spikesSamplesPath, spikesSamplesFilename)
        spikesSamples <- np$load(spikesSamplesFullFilename)
        spikesSamplesToPlot <- spikesSamples[interactionStartTime*sRate<=spikesSamples & spikesSamples<=interactionStopTime*sRate]
        traceName <- file_path_sans_ext(spikesSamplesFilename)
        fig <- fig%>%add_trace(x=spikesSamplesToPlot/sRate, y=rep(n, times=length(spikesSamplesToPlot)), marker=list(color=markerColor, size=markerSize), name=traceName, showlegend=TRUE)
    }
    behaviorsRecs <- getBehaviorsRecs(boutTimesFilenames=boutTimesFilenames, behaviorsNames=behaviorsNames, behaviorsColorsa=behaviorsColors, behaviorsRecsOpacity=behaviorsRecsOpacity)
    fig <- fig%>%layout(shapes=behaviorRecs, xaxis=list(title="Time (sec)"), yaxis=list(title="Neuron Index"))

    pngFigFilename <- sprintf(figFilenamePattern, interaction, "png")
    htmlFigFilename <- sprintf(figFilenamePattern, interaction, "html")
    orca(p=fig, file=pngFigFilename)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFigFilename)), basename(htmlFigFilename)))
    print(fig)

    browser()
}

processAll()

rm(processAll)

