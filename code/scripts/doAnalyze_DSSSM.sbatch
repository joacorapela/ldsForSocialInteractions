#!/bin/csh

# Print the task id.
echo "Starting task " $SLURM_ARRAY_TASK_ID " on " $SLURMD_NODENAME

set analysisStartTimeSecs=(`awk -v myNR=$SLURM_ARRAY_TASK_ID 'NR==myNR {print $1}' $modelSelectionFile`)
set trainDurSecs=(`awk -v myNR=$SLURM_ARRAY_TASK_ID 'NR==myNR {print $2}' $modelSelectionFile`)
set stateDim=(`awk -v myNR=$SLURM_ARRAY_TASK_ID 'NR==myNR {print $3}' $modelSelectionFile`)
set stateInputMemorySecs=(`awk -v myNR=$SLURM_ARRAY_TASK_ID 'NR==myNR {print $4}' $modelSelectionFile`)
set obsInputMemorySecs=(`awk -v myNR=$SLURM_ARRAY_TASK_ID 'NR==myNR {print $5}' $modelSelectionFile`)
set initialCondMethod=(`awk -v myNR=$SLURM_ARRAY_TASK_ID 'NR==myNR {print $6}' $modelSelectionFile`)

Rscript doAnalyze_DSSSM_batch.R --analysisStartTimeSecs=$analysisStartTimeSecs --trainDurSecs=$trainDurSecs --stateDim=$stateDim --stateInputMemorySecs=$stateInputMemorySecs --obsInputMemorySecs=$obsInputMemorySecs --initialCondMethod=$initialCondMethod --checkModelsLogFilename $binConfigFilename $estConfigFilename $modelsLogFilename

echo "Done with task " $SLURM_ARRAY_TASK_ID " on " $SLURMD_NODENAME

