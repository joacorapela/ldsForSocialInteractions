#!/bin/csh

set filename='../../../data/exampleMouse/modelSelection_tmp.txt'
set analysisStartTimeSecs=(341)
set trainDurSecs=(401)
set stateDims=(2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19)
set initMethods=(PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA PPCA)
set stateMemories=(0.0)
set obsMemories=(0.0)

foreach analysisStartTime ($analysisStartTimeSecs)
    foreach trainDur ($trainDurSecs)
        set i = 1
        while ($i <= ${#stateDims})
            foreach stateMemory ($stateMemories)
                foreach obsMemory ($obsMemories)
                    echo $analysisStartTime $trainDur $stateDims[$i] $stateMemory $obsMemory $initMethods[$i] >> $filename
                end
            end
            @ i++
        end
    end
end
