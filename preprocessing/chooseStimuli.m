% this script splits up participant files into condition based files and
% balances the deviants with the standards accordingly

clear; eeglab nogui;
rng('default'); % For reproducibility

%% init the dirs
indir = 'L:\eeg\lastekatse2021\Erik\data\working\preprocessing\MADE_out\task_tf_noBL\processed_data';
outdir = 'L:\eeg\lastekatse2021\Erik\data\working\preprocessing\relevantStimuli_update_tf_noBL\';

%% Read files to analyses
datafile_names=dir(indir);
datafile_names=datafile_names(~ismember({datafile_names.name},{'.', '..', '.DS_Store','.fdt'}));
datafile_names={datafile_names.name};
[filepath,name,ext] = fileparts(char(datafile_names{1}));

report.names = [];
report.dev16 = [];
report.dev32 = [];
report.dev64 = [];

%% Create output folders to save data

stimulus = ["sadam","sada", "vere", ];
condition = ["64", "32", "16"];
groupCode = ["EK"; "KP"];







counter = 1
for subject=1:length(datafile_names)

    [filepath,name,ext] = fileparts(char(datafile_names{subject}));
    if ~strcmp(ext,'.set') | contains(name,'no_usable_data_all_bad_epochs')
        continue
    end
    
    containsBadSbjct = false;
    bad_subjects = ["08" ; "38"; "44"];
    for i = 1:height(bad_subjects)
        if contains(name,bad_subjects(i))
            containsBadSbjct = true
            break
        end
    end
    if containsBadSbjct == true
        continue
    end

    EEG=[];


    fprintf('\n\n\n*** Processing subject %d (%s) ***\n\n\n', subject, datafile_names{subject});

    %% load data set
    try
        EEG = pop_loadset([indir filesep datafile_names{subject}]);
    catch
    end


    %% set the right wokring director
    % if not existend create
    % if found or created break
    % check sadam before sada to not have sadam in sada
    for group = 1:2
        if contains(name,groupCode(group))
            for stim = 1:3
                if contains(lower(name),stimulus(stim))
                    outdir_current = char(strcat(outdir, filesep,'splitByStimCond',filesep, groupCode(group),filesep,stimulus(stim)));
                    if exist(outdir_current, 'dir') == 0
                        mkdir(outdir_current)
                    end
                    break
                end
            end
        end
    end

    condLongString = ["condition 16"; "condition 32"; "condition 64"];
    condString = ["16"; "32"; "64"];

    devNum = [];

    for i = 1:3

        tempEEG = EEG;
        % if exist(char(strcat(outdir, filesep, condString(i))), 'dir') == 0
        %     mkdir(char(strcat(outdir, filesep, condString(i))));
        % end
        if exist(char(strcat(outdir_current,filesep, condString(i),filesep,'deviant')), 'dir') == 0
            mkdir(char(strcat(outdir_current,filesep, condString(i),filesep,'deviant')));
        end
        if exist(char(strcat(outdir_current,filesep, condString(i),filesep,'standard')), 'dir') == 0
            mkdir(char(strcat(outdir_current,filesep, condString(i),filesep,'standard')));
        end

        tempEEG.relevantStimuli.original = length([tempEEG.event.edftype] );
        maskDev = strcmp({tempEEG.event.type}, condLongString(i));

        tempEEG2 = pop_selectevent(tempEEG,'event',find([tempEEG.event.edftype] == 128 & [tempEEG.event.urevent] > 14 ));
        maskStds = ismember([tempEEG.event.urevent],randsample([tempEEG2.event.urevent],sum(maskDev)));
        maskCond = maskDev | maskStds;

        tempEEG.relevantStimuli.deviant = sum(maskDev);
        tempEEG.relevantStimuli.standard = sum(maskStds);
        tempEEG.relevantStimuli.combined = sum(maskCond);
        devNum(i) = sum(maskDev);


        tempEEGDev = pop_selectevent(tempEEG,'event',find(maskDev));
        tempEEGStd = pop_selectevent(tempEEG,'event',find(maskStds));
        tempEEG = pop_selectevent(tempEEG,'event',find(maskCond));

        tempEEGDev.setname=char(strcat(name, condString(i), '_deviant' ));
        tempEEGStd.setname=char(strcat(name, condString(i), '_standard' ));
        tempEEG.setname=char(strcat(name, condString(i)));

        tempEEGDev = pop_saveset( tempEEGDev, 'filename',[tempEEGDev.setname  '.set'],'filepath',char(strcat(outdir_current,filesep, condString(i),filesep,'deviant')));
        tempEEGStd = pop_saveset( tempEEGStd, 'filename',[tempEEGStd.setname  '.set'],'filepath',char(strcat(outdir_current,filesep, condString(i),filesep,'standard')));
        %tempEEG = pop_saveset( tempEEG, 'filename',[tempEEG.setname  '.set'],'filepath',char(strcat(outdir,filesep,condString(i))));

        % EEG = pop_saveset(EEG, 'filename', strrep(datafile_names{subject}, ext, 'relevantStimuli.set'),'filepath', [outdir filesep condString(i) filesep ]);

    end

    report.names{counter} = datafile_names(subject);
    report.dev16{counter} = devNum(1)
    report.dev32{counter} = devNum(2)
    report.dev64{counter} = devNum(3)
    counter = counter+1;

end

report_table = table(report.names', report.dev16', report.dev32', report.dev64');
report_table.Properties.VariableNames={'names','dev16','dev32','dev64'};
writetable(report_table, [outdir, filesep ,'deviant_numbers_report_', datestr(now,'dd-mm-yyyy'),'.csv']);

