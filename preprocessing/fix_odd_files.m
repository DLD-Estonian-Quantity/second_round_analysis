% this script fixes reproducably the files which did not follow the naming
% convention, based on the external data

clear
eeglab nogui;

indir = 'L:\eeg\lastekatse2021\Erik\data\working\preprocessing\MADE_out\task_tf_noBL\'


%% 01
movefile(char(strcat(indir,'processed_data\01P1KPsada_vere - SADA_processed_data.set')) , char(strcat(indir,'01P1KPsada_vere - SADA_processed_data.set')))
movefile( char(strcat(indir,'processed_data\01P1KPsada_vere - SADA_processed_data.fdt')), char(strcat(indir,'01P1KPsada_vere - SADA_processed_data.fdt')))
EEG = pop_loadset(char(strcat(indir,'01P1KPsada_vere - SADA_processed_data.set')));
tmpEEG_sada = pop_selectevent(EEG,'event',find([EEG.event.urevent] <= 734))
tmpEEG_sada = pop_saveset( tmpEEG_sada,'filename', '01P1KPsada_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))
tmpEEG_vere = pop_selectevent(EEG,'event',find([EEG.event.urevent] > 734))
tmpEEG_vere = pop_saveset( tmpEEG_vere,'filename', '01P1KPvere_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))

%% 16
EEG = pop_loadset(char(strcat(indir,'processed_data\16T1sada_processed_data.set')));
EEG = pop_saveset( EEG,'filename', '16T1EKsada_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))
EEG = pop_loadset(char(strcat(indir,'processed_data\16T1sadaM_processed_data.set')));
EEG = pop_saveset( EEG,'filename', '16T1EKsadaM_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))
EEG = pop_loadset(char(strcat(indir,'processed_data\16T1vere_processed_data.set')));
EEG = pop_saveset( EEG,'filename', '16T1EKvere_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))

%% 17
EEG = pop_loadset(char(strcat(indir,'processed_data\17T1AKsada_processed_data.set')));
EEG = pop_saveset( EEG,'filename', '17T1Kpsada_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))
EEG = pop_loadset(char(strcat(indir,'processed_data\17T1AKsadam_processed_data.set')));
EEG = pop_saveset( EEG,'filename', '17T1KPsadaM_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))
EEG = pop_loadset(char(strcat(indir,'processed_data\17T1AKvere_processed_data.set')));
EEG = pop_saveset( EEG,'filename', '17T1KPvere_processed_data.set' , 'filepath' ,char(strcat(indir,'processed_data')))