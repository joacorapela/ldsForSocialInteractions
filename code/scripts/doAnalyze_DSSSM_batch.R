
require(MASS)
require(ini)
require(optparse)
source("../projectSrc/utils/kalmanFilter/buildBehavioralInputs.R")
source("../projectSrc/utils/kalmanFilter/getInitialConds.R")
source("../commonSrc/stats/kalmanFilter/emEstimationKF_SS_withOffsetsAndInputs.R")
source("../commonSrc/stats/kalmanFilter/filterLDS_SS_withOffsetsAndInputs.R")
source("../commonSrc/stats/kalmanFilter/smoothLDS_SS.R")
source("../commonSrc/stats/kalmanFilter/computeAIC.R")
source("../commonSrc/stats/kalmanFilter/estimateKFInitialCondFA.R")
source("../commonSrc/stats/kalmanFilter/estimateKFInitialCondPPCA.R")

processAll <- function() {
DEBUG <- TRUE
# DEBUG <- FALSE
if(!DEBUG) {
    option_list <- list(
        make_option(c("-a", "--analysisStartTimeSecs"), type="double", default=0, help="Analysis start time (sec)"),
        make_option(c("-t", "--trainDurSecs"), type="double", default=180, help="Duration of the data train segment (sec)"),
        make_option(c("-d", "--stateDim"), type="integer", default=3, help="State dimensionalty"),
        make_option(c("-m", "--stateInputMemorySecs"), type="double", default=0.6, help="State input memory (sec)"),
        make_option(c("-o", "--obsInputMemorySecs"), type="double", default=0.6, help="Observations input memory (sec)"),
        make_option(c("-i", "--initialCondMethod"), type="character", default="FA", help="Initial conditions method (FA: factor analysis; PPCA: probabilisitc PCA"),
        make_option(c("-n", "--nStartFA"), type="integer", default=5, help="Number of start values for factor analysis"),
        make_option(c("-c", "--checkModelsLogFilename"), action="store_true", default=FALSE, help="Check models log filename and do not estimate a model if it has been previously estimated")
    )
    parser <- OptionParser(usage = "%prog [options] binConfigFilename estConfigFilename modelsLogFilename", option_list=option_list)
    parseRes <- parse_args(parser, positional_arguments=3)
    arguments <- parseRes$args
    options <- parseRes$options

    analysisStartTimeSecs <- options$analysisStartTimeSecs
    trainDurSecs <- options$trainDurSecs

    stateDim <- options$stateDim
    stateInputMemorySecs <- options$stateInputMemorySecs
    if(stateInputMemorySecs<0) {
        stateInputMemorySecs <- NaN
    }
    obsInputMemorySecs <- options$obsInputMemorySecs
    if(obsInputMemorySecs<0) {
        obsInputMemorySecs <- NaN
    }
    initialCondMethod <- options$initialCondMethod
    nStartFA <- options$nStartFA
    checkModelsLogFilename <- options$checkModelsLogFilename

    binConfigFilename <- arguments[[1]]
    estConfigFilename <- arguments[[2]]
    modelsLogFilename <- arguments[[3]]

} else {
    # 46390734, 180.000000, 180.000000, 60.000000, 5, 0.000000, 0.400000, PPCA, -10923.367918, 23110.735836, -3692.318680, 141.450000
    analysisStartTimeSecs <- 341
    trainDurSecs <- 401
    stateDim <- 6
    stateInputMemorySecs <- 0.0
    obsInputMemorySecs <- 0.4
    initialCondMethod <- "PPCA"
    binConfigFilename <- "../../data/firstMouse/binLDStimeSeries.ini"
    estConfigFilename <- "../../data/firstMouse/estimation_DSSSM.ini"
    modelsLogFilename <- "../../log/firstMouse/log_DSSSM.csv"
    nStartFA <- 5
    checkModelsLogFilename <- TRUE
}
    binConfig <- read.ini(binConfigFilename)
    binnedTimeSeriesFilenamePattern <- binConfig$filenames$binnedTimeSeriesFilenamePattern
    interactionNumber <- as.numeric(binConfig$behaviors$interactionNumber)
    dataFilenamePattern <-  binConfig$filenames$saveFilename
    timeSeriesFilename <- sprintf(binnedTimeSeriesFilenamePattern, interactionNumber)


    estConfig <- read.ini(estConfigFilename)

    stateCovType <- estConfig$covariance_type$states
    initialStateCovType <- estConfig$covariance_type$initialStates
    obsCovType <- estConfig$covariance_type$observations
    covsConstraints <- list(V0=initialStateCovType, Q=stateCovType, R=obsCovType)

    tol <- as.double(estConfig$EM$tol)
    maxIter <- as.numeric(estConfig$EM$maxIter)
    maxIter <- maxIter+1

    estMetaDataFilenamePattern <- estConfig$filenames$estMetaDataFilenamePattern
    estResFilenamePattern <- estConfig$filenames$estResFilenamePattern

    timeSeries <- get(load(timeSeriesFilename))
    sRate <- timeSeries$sRate
    times <- seq(from=timeSeries$startTime, by=1/timeSeries$sRate, length.out=ncol(timeSeries$spikeCounts))
    selectedSamples <- which(analysisStartTimeSecs<=times & times<analysisStartTimeSecs+trainDurSecs)
    trainSpikeCounts <- timeSeries$spikeCounts[,selectedSamples]
    trainSpikeCounts <- trainSpikeCounts[1:40,]
    browser()
    trainSqrtSpikeCounts <- sqrt(trainSpikeCounts)
    behavioralTimeSeries <- timeSeries$behavioralTimeSeries[,selectedSamples]
    numberOfInputs <- nrow(behavioralTimeSeries)

    obsDim <- nrow(trainSqrtSpikeCounts)
    initialConds <- getInitialConds(initialValues=estConfig$initial_values, stateDim=stateDim, obsDim=obsDim, numberOfInputs=numberOfInputs, stateInputMemorySamples=stateInputMemorySecs*sRate, obsInputMemorySamples=obsInputMemorySecs*sRate)

    show(sprintf("Processing number of latents=%d, state input memory=%f sec, observation input memory=%f sec", stateDim, stateInputMemorySecs, obsInputMemorySecs))

    exit <- FALSE
    while(!exit) {
        estNumber <- sample(1e8, 1)
        estMetaDataFilename <- sprintf(estMetaDataFilenamePattern, estNumber)
        if(!file.exists(estMetaDataFilename)) {
            exit <- TRUE
        }
    }

    metaData <- list()
    metaData[["estimation_config_info"]] <- list(estConfigFilename=estConfigFilename)
    metaData[["model_info"]] <- list(stateDim=stateDim, stateInputMem=stateInputMemorySecs, obsInputMem=obsInputMemorySecs)
    write.ini(x=metaData, filepath=estMetaDataFilename)
    show(sprintf("Saved estimation meta data to: %s", estMetaDataFilename))

    if(!is.nan(stateInputMemorySecs)) {
        trainStateInputs <- buildBehavioralInputs(behavioralTimeSeries=behavioralTimeSeries, inputMemory=as.integer(stateInputMemorySecs*sRate))
        dim(trainStateInputs) <- c(nrow(trainStateInputs), 1, ncol(trainStateInputs))
    } else {
        trainStateInputs <- NA
    }
    if(!is.nan(obsInputMemorySecs)) {
        trainObsInputs <- buildBehavioralInputs(behavioralTimeSeries=behavioralTimeSeries, inputMemory=as.integer(obsInputMemorySecs*sRate))
        dim(trainObsInputs) <- c(nrow(trainObsInputs), 1, ncol(trainObsInputs))
    } else {
        trainObsInputs <- NA
    }

    dataForEstInitialCond <- t(as.matrix(trainSqrtSpikeCounts))
    startTime <- proc.time()[3]
    if(initialCondMethod=="FA") {
        controlFA <- list(trace=TRUE, nstart=nStartFA)
        estRes <- estimateKFInitialCondFA(z=dataForEstInitialCond, nFactors=stateDim, control=controlFA)
        initialConds <- c(list(B=estRes$B, Z=estRes$Z, R=diag(estRes$RDiag)), initialConds)
    } else {
        if(initialCondMethod=="PPCA") {
            estRes <- estimateKFInitialCondPPCA(z=dataForEstInitialCond, nFactors=stateDim)
            initialConds <- c(list(B=estRes$B, Z=estRes$Z), initialConds)
        } else {
            stop(sprintf("Invalid initialCondMethod=%s", initialCondMethod))
        }
    }
    dsSSM <- emEstimationKF_SS_withOffsetsAndInputs(y=trainSqrtSpikeCounts, c=trainStateInputs, d=trainObsInputs, B0=initialConds$B, u0=initialConds$u, C0=initialConds$C, Q0=initialConds$Q, Z0=initialConds$Z, a0=initialConds$a, D0=initialConds$D, R0=initialConds$R, m0=initialConds$m0, V0=initialConds$V0, maxIter=maxIter, tol=tol, varsToEstimate=list(m0=TRUE, V0=TRUE, B=TRUE, u=TRUE, C=TRUE, Q=TRUE, Z=TRUE, a=TRUE, D=TRUE, R=TRUE), covsConstraints=covsConstraints)
    elapsedTime <- proc.time()[3]-startTime
    elapsedTime <- unname(elapsedTime)

    AIC <- computeAIC(dsSSM=dsSSM)
    logMessage <- sprintf("%d, %f, %f, %d, %f, %f, %s, %f, %f, %f\n", estNumber, analysisStartTimeSecs, trainDurSecs, stateDim, stateInputMemorySecs, obsInputMemorySecs, initialCondMethod, dsSSM$logLik[length(dsSSM$logLik)], AIC, elapsedTime)
    show(logMessage)
    cat(logMessage, file=modelsLogFilename, append=TRUE)
    metaData[["estimation_summary"]] <- list(logLik=dsSSM$logLik[length(dsSSM$logLik)], AIC=AIC, elapsedTime=elapsedTime)
    write.ini(x=metaData, filepath=estMetaDataFilename)
    #
    estResFilename <- sprintf(estResFilenamePattern, estNumber)
    estRes <- list(dsSSM=dsSSM, AIC=AIC, initialConds=initialConds, stateDim=stateDim, stateInputMemorySecs=stateInputMemorySecs, obsInputMemorySecs=obsInputMemorySecs, trainSqrtSpikeCounts=trainSqrtSpikeCounts, stateInputs=trainStateInputs, obsInputs=trainObsInputs, sRate=sRate, startTime=analysisStartTimeSecs)
    save(estRes, file=estResFilename)
}

null <- processAll()
