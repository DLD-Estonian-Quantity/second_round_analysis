# Summary
This repository is providing code and explanation for the study on the understanding of the Estonian quantity systems in a population with developmental language disorder (DLD). For this purpose a longitudinal EEG study was performed. In this repository you may find the code used. 
The code is available under the GPLv3 open source license and might be used in accordance with this license. We used some code of other talented researchers. We are including the information in accordance with the license provided by them. If you feel like we are not following the best practice feel free to contact us and we will try the best to provide a good solution.
# Glossary
**Group** - referring to the participants with two level typical and clinical developing children 

**Stimulus** - Referring to the word that makes up the odd-ball task with three options: *sada*, *vere* and *sadam* (sadaManipulated) 

**Condition** - with in the optimum-1 oddball paradigm there are multiple oddballs per stimulus these are the deviants in quantity 1 and quantity 3 of the *stimulus* word as well as quantity 2 which also always took the place of the standard. The conditions are encoded as follow: deviant:  quantity 1: condition 32, quantity 2: condition 16, quantity 3: condition 64; standard, quantity 2: condition 128
# Requirements

We are including here the complete list of packages and extensions installed on the computer on the time of analysis. Even if not all packages are used by our processing and analysis we want to give the full list to disclose any possible interactions of packages which might occur in an unforeseen way.

## Matlab
This code was tested using the following packages
MATLAB Version: 9.14.0.2239454 (R2023a) Update 1
Operating System: Microsoft Windows Server 2022 Standard Version 10.0 (Build 20348)
 Java Version: Java 1.8.0_202-b08 with Oracle Corporation Java HotSpot(TM) 64-Bit Server VM mixed mode

| Package                                 | Version | Revision |
| --------------------------------------- | ------- | -------- |
| MATLAB                                  | 9.14    | R2023a   |
| Simulink                                | 10.7    | R2023a   |
| EEGLAB Toolbox to process EEG data      | 2023.0  |          |
| FieldTrip                               | unknown |          |
| Image Processing Toolbox                | 11.7    | R2023a   |
| MATLAB Compiler                         | 8.6     | R2023a   |
| Optimization Toolbox                    | 9.5     | R2023a   |
| Parallel Computing Toolbox              | 7.8     | R2023a   |
| Signal Processing Toolbox               | 9.2     | R2023a   |
| Simulink Compiler                       | 1.6     | R2023a   |
| Statistical Parametric Mapping          | 6479    | R2023a   |
| Statistics and Machine Learning Toolbox | 12.5    | R2023a   |
| Wavelet Toolbox                         | 6.3     | R2023a   |
## EEGLAB

| Extension     | Version   |
| ------------- | --------- |
| Adjust        | v1.1.1    |
| Biosig        | v3.8.3    |
| Cleanline     | v2.00     |
| FASTER        | v1.2.4    |
| Fileio        | v20231113 |
| ICLabel       | v1.4      |
| MFFMatlabIO   | v4.1      |
| clean_rawdata | v2.91     |
| dipfit        | v5.3      |
| erplab        | v10.04    |
| firfilt       | v.2.7.1   |

# Structure

## Preprocessing

The code is divide into different files which should be used accordingly.
The first section is the preprocessing of the data. Here we were utilizing the MADE pipeline. The code of the MADE pipeline was extended to accommodate the additional step of classifying the stimuli before the potential removal of epochs during the course of the MADE pipeline. 

After using the Made pipeline due to some in consistencies in it was necessary to utility the custom function `fix_odd_files.m` 

After all files are fixed `choose_stimuli.m`splits up the preprocessed data file per participant in to data files per stimulus and condition and within this creating separate files, during this step it is made sure that for each condition there is the same number of deviant and standard. Standards are taken at random from the the pool of all standards of this recording, through this method one standard from on participant might be used for multiple conditions but only with in one stimulus.

## Analysis

The analysis is split up into multiple parts. The main parts being the analysis of the obligatory response. As well as the cluster-based permutation test of the event-related fields. The cluster-based permutation test on the time-frequency power was taken as an additional measure based on the results of the analysis of the event-related fields.

### Event-related fields
`e2f_ungrouped.m` converts from EEGLAB to Fieldtrip format, run before the analysis.

`ERF_calc_ungr_epochShort_woT7T8_groupCol_stimSep_condSep.m` The group analysis file.
- `ERF` -> Event-related field
- `calc` -> calculation
- `ungr` -> ungrouped
- `epochShort` -> take the epoch only since onset of change in stimulus
- `woT7T8` -> without electrode side T7 and T8 since they are too noisy
- `groupCol_stimSep_condSep` -> group,stimulus,condition Collapsed/Separate


### Time-frequency power analysis
`e2f_tf_ungrouped.m` converts from EEGLAB to Fieldtrip format, run before the analysis.

`TF_calc_ungrouped_groupSep_stimSep_condSep_TW1.m` The group analysis file, based on the ERF results
- `TF` -> time-frequency
- `calc` -> calculation
- `ungrouped` -> ungrouped
- `groupCol_stimSep_condSep` -> group,stimulus,condition Collapsed/Separate
- `TW1` -> time window 1,2,...
