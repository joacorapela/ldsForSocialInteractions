
require(MARSS)
require(ini)
require(optparse)
require(plotly)
source("../commonSrc/stats/utils/computePercentageExplainedVar.R")
source("../commonSrc/stats/kalmanFilter/filterLDS_SS_withOffsetsAndInputs.R")
source("../commonSrc/stats/kalmanFilter/smoothLDS_SS.R")
source("../commonSrc/stats/kalmanFilter/computeOneStepAheadObsPredStats.R")
source("../commonSrc/plot/kalmanFilter/getPlotTrueInitialAndEstimatedMatrices.R")
source("../commonSrc/plot/kalmanFilter/getPlotTrueInitialAndEstimatedVectors.R")
source("../commonSrc/plot/kalmanFilter/getPlotStateSpectrum.R")
source("../commonSrc/plot/kalmanFilter/getPlotOneStepAheadForecasts.R")
source("../commonSrc/plot/kalmanFilter/getPlotSmoothedStates.R")
source("../commonSrc/plot/kalmanFilter/getPlotPercentageExplainedVar.R")
source("../commonSrc/plot/kalmanFilter/getPlotLogLik.R")
source("../commonSrc/plot/getRecsForOnsetAndOffsetTimes.R")
source("../projectSrc/plot/getBehaviorsRecs.R")

plotAllRFsAllNeurons <- function(Z, B, C, D, stateInputMemorySamples, obsInputMemorySamples, stateInputMemoryToPlotSamples, sRate, mouseName, figFilenamePattern, estNumber, xlab="Delay (sec)", ylab="Value") {
    if(stateInputMemorySamples!=0) {
        stop(sprintf("At the moment only stateInputMemorySamples=0 is supported, but you provided stateInputMemorySamples=%d", stateInputMemorySamples))
    }
    if(is.nan(obsInputMemorySamples)) {
        stop("obsInputMemorySamples should be greater or equal than zero")
    }
    Cvg <- C[,1]
    Cvn <- C[,2]
    Clg <- C[,3]
    Cln <- C[,4]
    ZBCvg <- matrix(NA, nrow=nrow(Z), ncol=stateInputMemoryToPlotSamples+1)
    ZBCvn <- matrix(NA, nrow=nrow(Z), ncol=stateInputMemoryToPlotSamples+1)
    ZBClg <- matrix(NA, nrow=nrow(Z), ncol=stateInputMemoryToPlotSamples+1)
    ZBCln <- matrix(NA, nrow=nrow(Z), ncol=stateInputMemoryToPlotSamples+1)
    Bpow <- diag(rep(1, nrow(B)))
    for(i in 1:(stateInputMemoryToPlotSamples+1)) {
        ZBCvg[,i] <- Z%*%Bpow%*%Cvg
        ZBCvn[,i] <- Z%*%Bpow%*%Cvn
        ZBClg[,i] <- Z%*%Bpow%*%Clg
        ZBCln[,i] <- Z%*%Bpow%*%Cln
        Bpow <- Bpow%*%B
    }
    DblockSize <- 1+obsInputMemorySamples
    Doffset <- 0
    #
    Dvg <- matrix(D[,Doffset+(1:DblockSize)], ncol=DblockSize)
    Doffset <- Doffset + DblockSize
    Dvn <- matrix(D[,Doffset+(1:DblockSize)], ncol=DblockSize)
    Doffset <- Doffset + DblockSize
    Dlg <- matrix(D[,Doffset+(1:DblockSize)], ncol=DblockSize)
    Doffset <- Doffset+DblockSize
    Dln <- matrix(D[,Doffset+(1:DblockSize)], ncol=DblockSize)

    timeStateInputs <- (0:stateInputMemoryToPlotSamples)/sRate
    timeObsInputs <- (0:obsInputMemorySamples)/sRate
    nNeurons <- nrow(Z)
    for(n in 1:nNeurons) {
        show(sprintf("Plotting RFs for neuron %d", n))
        fig <- plot_ly(type='scatter', mode='lines+markers')
        fig <- fig%>%add_trace(x=timeStateInputs, y=ZBCvg[n,], name="population_vg", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeStateInputs, y=ZBCvn[n,], name="population_vn", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeStateInputs, y=ZBClg[n,], name="population_lg", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeStateInputs, y=ZBCln[n,], name="population_ln", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeObsInputs, y=Dvg[n,], name="neuron_vg", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeObsInputs, y=Dvn[n,], name="neuron_vn", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeObsInputs, y=Dlg[n,], name="neuron_lg", type="scatter", mode="lines+markers")
        fig <- fig%>%add_trace(x=timeObsInputs, y=Dln[n,], name="neuron_ln", type="scatter", mode="lines+markers")
        fig <- fig %>% layout(xaxis=list(title=xlab), yaxis=list(title=ylab))
        pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, sprintf("allRFsNeuron%d", n), "png")
        htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, sprintf("allRFsNeuron%d", n), "html")
        htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
        orca(p=fig, file=pngFilename)
        # print(fig)
        # browser()
    }
}

processAll <- function() {
    # DEBUG <- FALSE
    DEBUG <- TRUE
    if(!DEBUG) {
        option_list <- list(
            make_option(c("-m", "--estMetaDataFilenamePattern"), type="character", default="../../results/%s/%08d_estimation.ini", help="Estimation metadata filename pattern"),
            make_option(c("-m", "--timeSeriesFilenamePattern"), type="character", default="../../data/%s/binLDStimeSeries.ini", help="Time series filename pattern"),
            make_option(c("-f", "--figFilenamePattern"), type="character", default="../../figures/%s/%08d_%s.%s", help="Figure filename pattern")
        )
        parser <- OptionParser(usage = "%prog [options] mouseName estNumber", option_list=option_list)
        parseRes <- parse_args(parser, positional_arguments=2)
        options <- parseRes$options
        arguments <- parseRes$args

        mouseName <- arguments[1]
        estNumber <- as.numeric(arguments[2])
        estMetaDataFilenamePattern <- options$estMetaDataFilenamePattern
        timeSeriesFilenamePattern <- options$timeSeriesFilenamePattern
        figFilenamePattern <- options$figFilenamePattern
    } else {
        mouseName <- "exampleMouse"
        estNumber <- 53386362
        behaviorsToPlot <- c("nonsocial", "approach", "following", "headhead", "headtail", "conspecific")
        behaviorsColors <- c("black", "red", "green", "blue", "cyan", "orange")
        estMetaDataFilenamePattern <- "../../results/%s/%08d_estimation.ini"
        timeSeriesFilenamePattern <- "../../data/%s/binLDStimeSeries.ini"
        figFilenamePattern <- "../../figures/%s/%08d_%s.%s"
    }
    estMetaDataFilename <- sprintf(estMetaDataFilenamePattern, mouseName, estNumber)
    estMetaData <- read.ini(filepath=estMetaDataFilename)
    estConfigFilename <- estMetaData$estimation_config_info$estConfigFilename
    estConfig <- read.ini(estConfigFilename)
    timeSeriesFilename <- sprintf(timeSeriesFilenamePattern, mouseName)
    timeSeriesConfig <- read.ini(timeSeriesFilename)
    dataFilenamePattern <- timeSeriesConfig$filenames$binnedTimeSeriesFilenamePattern
    boutTimesPath <- timeSeriesConfig$filenames$boutTimesPath
    boutTimesPatternPattern <- timeSeriesConfig$filenames$boutTimesPatternPattern
    interactionNumber <- as.numeric(timeSeriesConfig$behaviors$interactionNumber)
    estResFilenamePattern <- estConfig$filenames$estResFilenamePattern

    dataFilename <- sprintf(dataFilenamePattern, interactionNumber)
    estResFilename <- sprintf(estResFilenamePattern, estNumber)
    estRes <- get(load(estResFilename))
    stateDim <- estRes$stateDim
    obsInputMemorySecs <- estRes$obsInputMemorySecs
    stateInputMemorySecs <- estRes$stateInputMemorySecs

    dsSSM <- estRes$dsSSM
    obsInputs <- estRes$obsInputs
    stateInputs <- estRes$stateInputs
    sRate <- estRes$sRate
    stateInputMemorySamples <- stateInputMemorySecs*sRate
    obsInputMemorySamples <- obsInputMemorySecs*sRate

    boutTimesPattern <- sprintf(boutTimesPatternPattern, interactionNumber)
    boutTimesFilenames <- list.files(path=boutTimesPath, pattern=boutTimesPattern)
    kfRes <- filterLDS_SS_withOffsetsAndInputs(y=estRes$trainSqrtSpikeCounts, B=dsSSM$B, u=dsSSM$u, C=dsSSM$C, c=estRes$stateInputs, Q=dsSSM$Q, m0=dsSSM$m0, V0=dsSSM$V0, Z=dsSSM$Z, a=dsSSM$a, D=dsSSM$D, d=estRes$obsInputs, R=dsSSM$R)
    ksRes <- smoothLDS_SS(B=dsSSM$B, xnn=kfRes$xnn, Vnn=kfRes$Vnn, xnn1=kfRes$xnn1, Vnn1=kfRes$Vnn1, m0=dsSSM$m0, V0=dsSSM$V0)

    trainSqrtSpikeCounts <- estRes$trainSqrtSpikeCounts
    time <- estRes$startTime+(1:ncol(estRes$trainSqrtSpikeCounts))/estRes$sRate
    dimObs <- nrow(trainSqrtSpikeCounts)
    nObs <- ncol(trainSqrtSpikeCounts)

    data <- get(load(dataFilename))
    behavioralLabels <- rownames(data$behavioralTimeSeries)

if(FALSE) {
    goStimOn <- data$goStimOnSecs
    goStimOff <- data$goStimOffSecs
    toKeepIndices <- which(minTime<=goStimOn & goStimOff<=maxTime)
    goStimOn <- goStimOn[toKeepIndices]
    goStimOff <- goStimOff[toKeepIndices]

    nogoStimOn <- data$nogoStimOnSecs
    nogoStimOff <- data$nogoStimOffSecs
    toKeepIndices <- which(minTime<=nogoStimOn & nogoStimOff<=maxTime)
    nogoStimOn <- nogoStimOn[toKeepIndices]
    nogoStimOff <- nogoStimOff[toKeepIndices]

    laserStimOn <- data$laserStimOnSecs
    laserStimOff <- data$laserStimOffSecs
    toKeepIndices <- which(minTime<=laserStimOn & laserStimOff<=maxTime)
    laserStimOn <- laserStimOn[toKeepIndices]
    laserStimOff <- laserStimOff[toKeepIndices]
}

if(TRUE) {
    show("Plotting logLik")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "logLik", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "logLik", "html")
    fig <- getPlotLogLik(logLik=dsSSM$logLik)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting B")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "B", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "B", "html")
    fig <- getPlotTrueInitialAndEstimatedMatrices(estimated=dsSSM$B)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting stateSpectrum")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "stateSpectrum", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "stateSpectrum", "html")
    fig <- getPlotStateSpectrum(B=dsSSM$B)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting U")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "U", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "U", "html")
    fig <- getPlotTrueInitialAndEstimatedVectors(estimated=dsSSM$u)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    if(!is.nan(stateInputMemorySecs)) {
        show("Plotting C")
        x <- (0:stateInputMemorySamples)/sRate
        CblockSize <- 1+stateInputMemorySamples
        Coffset <- 0
        #
        for(i in 1:length(behavioralLabels)) {
            descriptor <- sprintf("C%s", behavioralLabels[i]) 
            COneStim <- matrix(data=dsSSM$C[,Coffset+(1:CblockSize)], ncol=CblockSize)
            pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, descriptor, "png")
            htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, descriptor, "html")
            fig <- getPlotTrueInitialAndEstimatedMatrices(x=x, estimated=COneStim, estimatedLegendLabelPattern="state %d", xlab="Delay (sec)")
            htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
            orca(p=fig, file=pngFilename)
            # print(fig)
            Coffset <- Coffset + CblockSize
        }
    }

    show("Plotting Q")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "Q", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "Q", "html")
    fig <- getPlotTrueInitialAndEstimatedVectors(estimated=diag(dsSSM$Q), xlab="State Index")
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting m0")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "m0", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "m0", "html")
    fig <- getPlotTrueInitialAndEstimatedVectors(estimated=dsSSM$m0, xlab="State Index")
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting V0")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "V0", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "V0", "html")
    fig <- getPlotTrueInitialAndEstimatedVectors(estimated=diag(dsSSM$V0), xlab="State Index")
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting Z")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "Z", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "Z", "html")
    fig <- getPlotTrueInitialAndEstimatedMatrices(estimated=dsSSM$Z)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    show("Plotting a")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "a", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "a", "html")
    fig <- getPlotTrueInitialAndEstimatedVectors(estimated=dsSSM$a, xlab="Observation Index")
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

    if(!is.nan(obsInputMemorySecs)) {
        show("Plotting D")
        x <- (0:obsInputMemorySamples)/sRate
        DblockSize <- 1+obsInputMemorySamples
        Doffset <- 0
        #
        for(i in 1:length(behavioralLabels)) {
            descriptor <- sprintf("D%s", behavioralLabels[i])
            DOneStim <- matrix(dsSSM$D[,Doffset+(1:DblockSize)], ncol=DblockSize)
            pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, descriptor, "png")
            htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, descriptor, "html")
            fig <- getPlotTrueInitialAndEstimatedMatrices(x=x, estimated=DOneStim, estimatedLegendLabelPattern="neuron %d", xlab="Delay (sec)")
            htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
            orca(p=fig, file=pngFilename)
            # print(fig)
            Doffset <- Doffset + DblockSize
        }
    }

    show("Plotting R")
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "R", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "R", "html")
    fig <- getPlotTrueInitialAndEstimatedVectors(estimated=diag(dsSSM$R), xlab="Observation Index")
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)
}

    predStats <- computeOneStepAheadObsPredStats(xtt1=kfRes$xnn1[,1,], Vtt1=kfRes$Vnn1, Z=dsSSM$Z, a=as.vector(dsSSM$a), D=dsSSM$D, R=dsSSM$R, obsInputs=estRes$obsInputs[,1,])

if(TRUE) {
    show("Plotting percExpVar")
    percExpVar <- computePercentageExplainedVar(observations=trainSqrtSpikeCounts, predictions=predStats$ytt1)
    pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "percExpVar", "png")
    htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "percExpVar", "html")
    fig <- getPlotPercentageExplainedVar(percExpVar=percExpVar)
    htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
    orca(p=fig, file=pngFilename)
    # print(fig)

#     show("Plotting oneStepAheadForecasts")
#     pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "oneStepAheadForecasts", "png")
#     htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "oneStepAheadForecasts", "html")
#     fig <- getPlotOneStepAheadForecasts(time=time, obs=trainSqrtSpikeCounts, ytt1=predStats$ytt1, Wtt1=predStats$Wtt1, goStimOn=goStimOn, goStimOff=goStimOff, nogoStimOn=nogoStimOn, nogoStimOff=nogoStimOff, laserStimOn=laserStimOn, laserStimOff=laserStimOff)
#     htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
#     orca(p=fig, file=pngFilename)
    # print(fig)

}
    neuronsToPlot <- c(8, 40, 22, 28)
    # for(i in 1:nrow(predStats$ytt1)) {
    for(i in neuronsToPlot) {
        show(sprintf("Plotting oneStepAheadForecast for neuron %d", i))
        pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, sprintf("oneStepAheadForecastsNeuron%d", i), "png")
        htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, sprintf("oneStepAheadForecastsNeuron%d", i), "html")
        fig <- getPlotOneStepAheadForecasts(time=time, obs=trainSqrtSpikeCounts, ytt1=predStats$ytt1, Wtt1=predStats$Wtt1, boutTimesPath=boutTimesPath, boutTimesFilenames=boutTimesFilenames, behaviorsToPlot=behaviorsToPlot, behaviorsColors=behaviorsColors, obsToPlot=c(i))
        htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
        orca(p=fig, file=pngFilename)
        # print(fig)
    }

#     show("Plotting smoothedStates")
#     pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "smoothedStates", "png")
#     htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, "smoothedStates", "html")
#     fig <- getPlotSmoothedStates(time=time, xtT=ksRes$xnN[,1,], VtT=ksRes$VnN, goStimOn=goStimOn, goStimOff=goStimOff, nogoStimOn=nogoStimOn, nogoStimOff=nogoStimOff, laserStimOn=laserStimOn, laserStimOff=laserStimOff)
#     htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
#     orca(p=fig, file=pngFilename)
    # print(fig)

    for(i in 1:nrow(ksRes$xnN)) {
        show(sprintf("Plotting smoothedState %d", i))
        pngFilename <- sprintf(figFilenamePattern, mouseName, estNumber, sprintf("smoothedState%d", i), "png")
        htmlFilename <- sprintf(figFilenamePattern, mouseName, estNumber, sprintf("smoothedState%d", i), "html")
        # fig <- getPlotSmoothedStates(time=time, xtT=ksRes$xnN[,1,], VtT=ksRes$VnN, goStimOn=goStimOn, goStimOff=goStimOff, nogoStimOn=nogoStimOn, nogoStimOff=nogoStimOff, laserStimOn=laserStimOn, laserStimOff=laserStimOff, statesToPlot=c(i))
        fig <- getPlotSmoothedStates(time=time, xtT=ksRes$xnN[,1,], VtT=ksRes$VnN, boutTimesPath=boutTimesPath, boutTimesFilenames=boutTimesFilenames, behaviorsToPlot=behaviorsToPlot, behaviorsColors=behaviorsColors)
        htmlwidgets::saveWidget(as_widget(fig), file.path(normalizePath(dirname(htmlFilename)), basename(htmlFilename)))
        orca(p=fig, file=pngFilename)
        # print(fig)
    }
    plotAllRFsAllNeurons(Z=dsSSM$Z, B=dsSSM$B, C=dsSSM$C, D=dsSSM$D, stateInputMemorySamples=stateInputMemorySamples, obsInputMemorySamples=obsInputMemorySamples, stateInputMemoryToPlotSamples=obsInputMemorySamples, sRate=sRate, figFilenamePattern=figFilenamePattern, mouseName=mouseName, estNumber=estNumber)

}

processAll()
