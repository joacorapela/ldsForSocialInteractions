#!/bin/csh

cd ..
set mouseName = exampleMouse

set time = 02:00:00
set mem = 4G

setenv modelSelectionFile ../../data/{$mouseName}/modelSelection.txt
setenv estConfigFilename ../../data/{$mouseName}/estimation_DSSSM.ini
setenv modelsLogFilename ../../log/{$mouseName}/log_DSSSM.csv

set arrayOpt=1-`wc -l < $modelSelectionFile`

sbatch \
--job-name=dsSSM \
--output=../../slurmOutputs/{$mouseName}/doAnalyze_DSSSM_%A_%a.out \
--error=../../slurmOutputs/{$mouseName}/doAnalyze_DSSSM_%A_%a.err \
--time=$time \
--mem=$mem \
--array=$arrayOpt \
./doAnalyze_DSSSM.sbatch 
cd -

