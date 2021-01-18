require(ini)
require(optparse)
require(reticulate)
require(yaml)
require(tools)
np <- import("numpy")

getSpikesSamples <- function(spikesSamplesPath, spikesSamplesPattern, startSample, stopSample, sRate) {
    spikesSamplesFilenames <- list.files(path=spikesSamplesPath, pattern=spikesSamplesPattern)
    sortedSpikesSamplesFilenames <- sortSpikesSamplesFilenames(filenames=spikesSamplesFilenames)
    allSpikesSamples <- list()
    for(i in 1:length(sortedSpikesSamplesFilenames)) {
        spikesSamplesFilename <- sortedSpikesSamplesFilenames[[i]]
        neuronName <- file_path_sans_ext(spikesSamplesFilename)
        spikesSamplesFullFilename <- file.path(spikesSamplesPath, spikesSamplesFilename)
        neuronSpikesSamples <- np$load(spikesSamplesFullFilename)
        neuronSpikesSamplesInRange <- neuronSpikesSamples[startSample<=neuronSpikesSamples & neuronSpikesSamples<=stopSample]
        allSpikesSamples[[neuronName]] <- neuronSpikesSamplesInRange
    }
    return(allSpikesSamples)
}

getRangeSpikesSamples <- function(spikesSamples) {
    minSpikeSample <- Inf
    maxSpikeSample <- -Inf
    for(i in 1:length(spikesSamples)) {
        neuronSpikesSamples <- spikesSamples[[i]]
        rangeNeuronSpikesSamples <- range(neuronSpikesSamples)
        if(rangeNeuronSpikesSamples[1]<minSpikeSample) {
            minSpikeSample <- rangeNeuronSpikesSamples[1]
        }
        if(rangeNeuronSpikesSamples[2]>maxSpikeSample) {
            maxSpikeSample <- rangeNeuronSpikesSamples[2]
        }
    }
    range <- c(minSpikeSample, maxSpikeSample)
    return(range)
}

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

getSpikeCounts <- function(spikesSamples, breaks) {
    nUnits <- length(spikesSamples)
    spikeCounts <- matrix(NA, nrow=nUnits, ncol=length(breaks)-1)
    for(i in 1:nUnits) {
        spikeCounts[i,] <- hist(spikesSamples[[i]], breaks=breaks, plot=FALSE)$counts
    }
    return(spikeCounts)
}

getBinnedStimulus <- function(stimOnSamples, stimOffSamples, breaks) {
    binnedStimulus <- rep(0, times=length(breaks)-1)
    for(i in 1:length(stimOnSamples)) {
        stimulatedBins <- which(stimOnSamples[i]<=breaks & breaks<stimOffSamples[i])
        binnedStimulus[stimulatedBins] <- 1.0
    }
    return(binnedStimulus)
}

getOnsetsAndOffsetsOfSocialBehaviorsByInteractionCategory <- function(interactionCategories, nInteractions, boutTimesPath, boutTimesPatternPattern) {
    answer <- list()
    for(interactionCategory in interactionCategories) {
        answer[[interactionCategory]] <- c()
    }
    for(i in 1:nInteractions) {
        boutTimesPattern <- sprintf(boutTimesPatternPattern, i)
        boutTimesFilenames <- list.files(path=boutTimesPath, pattern=boutTimesPattern)
        stopifnot(length(boutTimesFilenames)==1)
        boutTimesFullFilename <- file.path(boutTimesPath, boutTimesFilenames[1])
        behaviorsOnsetsAndOffsets <- np$load(boutTimesFullFilename)
        if(interactionCategories[i]=="Food") {
            behaviorOnsetsAndOffsets <- rbind(behaviorsOnsetsAndOffsets[["rice1"]], behaviorsOnsetsAndOffsets[["rice2"]])
        } else {
            behaviorOnsetsAndOffsets <- behaviorsOnsetsAndOffsets[["social"]]
        }
        answer[[interactionCategories[i]]] <- rbind(answer[[interactionCategories[i]]], behaviorOnsetsAndOffsets)
    }
    return(answer)
}

getBehaviorsOnsetsAndOffsets <- function(behaviorsToUse, boutTimesPath, boutTimesFilenames) {
    answer <- list()
    for(behaviorToUse in behaviorsToUse) {
        answer[[behaviorToUse]] <- c()
    }
    for(boutTimesFilename in boutTimesFilenames) {
        boutTimesFullFilename <- file.path(boutTimesPath, boutTimesFilename)
        behaviorsOnsetsAndOffsets <- np$load(boutTimesFullFilename)
        for(behaviorToUse in behaviorsToUse) {
            behaviorOnsetsAndOffsets <- behaviorsOnsetsAndOffsets[[behaviorToUse]]
            answer[[behaviorToUse]] <- rbind(answer[[behaviorToUse]], behaviorOnsetsAndOffsets)
        }
    }
    return(answer)
}

getBehavioralTimeSeries <- function(behaviorsOnsetsAndOffsets, breaks, sRate) {
    behavioralTimesSeries <- c()
    for(i in 1:length(behaviorsOnsetsAndOffsets)) {
        behaviorTimeSeries <- getBinnedStimulus(stimOnSamples=behaviorsOnsetsAndOffsets[[i]][,1]*sRate, stimOffSamples=behaviorsOnsetsAndOffsets[[i]][,2]*sRate, breaks=breaks)
        behavioralTimesSeries <- rbind(behavioralTimesSeries, behaviorTimeSeries)
    }
    rownames(behavioralTimesSeries) <- names(behaviorsOnsetsAndOffsets)
    return(behavioralTimesSeries)
}

processAll <- function() {
    DEBUG <- TRUE
    if(!DEBUG) {
        parser <- OptionParser(usage = "%prog configFilename")
        parseRes <- parse_args(parser, positional_arguments=1)
        arguments <- parseRes$args
        configFilename <- arguments[[1]]
    } else {
        configFilename <- "../../data/firstMouse/binLDStimeSeries.ini"
    }

    config <- read.ini(configFilename)

    sRate <- as.numeric(config$config_params$sRate)
    binSizeSecs <- as.numeric(config$config_params$binSizeSecs)
    behaviorsToUse <- eval(parse(text=config$behaviors$behaviorsToUse))
    interactionNumber <- as.numeric(config$behaviors$interactionNumber)
    spikesSamplesPath <- config$filenames$spikesSamplesPath
    spikesSamplesPattern <- config$filenames$spikesSamplesPattern
    boutTimesPath <- config$filenames$boutTimesPath
    boutTimesPatternPattern <- config$filenames$boutTimesPatternPattern
    parametersFilename <- config$filenames$parametersFilename
    binnedTimeSeriesFilenamePattern <- config$filenames$binnedTimeSeriesFilenamePattern

    parameters <- read_yaml(file=parametersFilename)
    interactionFolderName <- parameters[["folderName"]][interactionNumber]
    videoNumber <- as.numeric(substr(interactionFolderName, 1, 1))
    startTime <- parameters[["videoStart"]][videoNumber]
    stopTime <- parameters[["videoStop"]][videoNumber]

    startSample <- startTime*sRate
    stopSample <- stopTime*sRate
    spikesSamples <- getSpikesSamples(spikesSamplesPath=spikesSamplesPath, spikesSamplesPattern=spikesSamplesPattern, startSample=startSample, stopSample=stopSample, sRate=sRate)
    # rangeSpikesSamples <- getRangeSpikesSamples(spikesSamples=spikesSamples)
    # startTime <- rangeSpikesSamples[1]/sRate-binSizeSecs
    # stopTime <- rangeSpikesSamples[2]/sRate+binSizeSecs

    binSizeSamples <- round(binSizeSecs*sRate)
    breaks <- seq(from=startSample-binSizeSamples, to=stopSample+binSizeSamples, by=binSizeSamples)
    spikeCounts <- getSpikeCounts(spikesSamples=spikesSamples, breaks=breaks)

    boutTimesPattern <- sprintf(boutTimesPatternPattern, interactionNumber)
    boutTimesFilenames <- list.files(path=boutTimesPath, pattern=boutTimesPattern)
    behaviorsOnsetsAndOffsets <- getBehaviorsOnsetsAndOffsets(behaviorsToUse=behaviorsToUse, boutTimesPath=boutTimesPath, boutTimesFilenames=boutTimesFilenames)
    # interactionCategories <- parameters[["interactionCategory"]]
    # nInteractions <- parameters[["interaction#"]]
    # behaviorsOnsetsAndOffsets <- getOnsetsAndOffsetsOfSocialBehaviorsByInteractionCategory(interactionCategories=interactionCategories, nInteractions=nInteractions, boutTimesPath=boutTimesPath, boutTimesPatternPattern=boutTimesPatternPattern)

    behavioralTimeSeries <- getBehavioralTimeSeries(behaviorsOnsetsAndOffsets=behaviorsOnsetsAndOffsets, breaks=breaks, sRate=sRate)

    timeSeries <- list(sRate=1.0/binSizeSecs, startTime=startTime, spikeCounts=spikeCounts, behavioralTimeSeries=behavioralTimeSeries)

    binnedTimeSeriesFilename <- sprintf(binnedTimeSeriesFilenamePattern, interactionNumber)
    save(timeSeries, file=binnedTimeSeriesFilename)

    browser()
}

processAll()
# rm(processAll)
