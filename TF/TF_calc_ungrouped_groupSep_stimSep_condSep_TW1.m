% this script is calculating the time-frequency power cluster-based 
% permutation test, the code is based on the tutorial for time-frequency power
% field permutation test of fieldtrip and a script published by Marlene
% Meyer and Robert Oostenveld under GPLv3
% (https://github.com/Donders-Institute/infant-cluster-effectsize/blob/main/do_group_analysis.m)

% This script is for the first time window, determined  by the ERF analysis

% to look for cluster inspect the "stat_standard_deviant_clusstats" variale

clear; eeglab nogui; close all;
addpath('C:\Users\eriksam1\Documents\MATLAB\fieldtrip-master');
ft_defaults;

%% init the dirs
indir = 'L:\eeg\lastekatse2021\Erik\data\working\preprocessing\ungrouped-tf-noBL-powspctrm-width3-freq3_01_15-timepre100_700ms';
outdir = 'L:\eeg\lastekatse2021\Erik\data\working\analysis\TF_permTest_ungrouped_groupSep_stimSep_condSep_TW1';

%% Read files to analyses


mini = [];
maxi = [];
% KP -> clinical
% EK -> typically developing


%% Create output folders to save data


stimulus = ["sada" ];
condition = ["64"];
groupCode = ["EK","KP"];
groupName = ["typical";"clinical"];

for group = 1:length(groupCode)
    for stim = 1:length(stimulus)
        for cond = 1:length(condition)


            subjects_total = 0


            indir_current = char(strcat(indir ,filesep,groupName(group),filesep,stimulus(stim), filesep, condition(cond)));
            output_dir = char(strcat(outdir,filesep,groupName(group),filesep,stimulus(stim), filesep, condition(cond)));

            if exist(output_dir, 'dir') == 0
                mkdir(output_dir);
            end


            subjects=dir(indir_current);
            subjects=subjects(~ismember({subjects.name},{'.', '..', '.DS_Store'}));
            subjects={subjects.name};





            for ii = 1:length(subjects)

                folder                  = [indir_current filesep subjects{ii}];

                load(char(strcat(folder,filesep,'ft_data_std.mat')));
                load(char(strcat(folder,filesep,'ft_data_dev.mat')));


                cfg                     = [];
                cfg.baseline            = [ -100 0];
                cfg.baselinetype        = 'db';
                cfg.parameter           = 'powspctrm';
                standard_all(ii+subjects_total) = { ft_freqbaseline(cfg,ft_data_std) };
                deviant_all(ii+subjects_total) = { ft_freqbaseline(cfg,ft_data_dev) };

                % standard_all(ii+subjects_total) = { ft_data_std };
                % deviant_all(ii+subjects_total) = { ft_data_dev };


            end
            subjects_total = subjects_total + length(subjects)

            

       end
    end
end


            %%
            % Calculate grand average for both conditions (standard, deviant) separately
            cfg                      = [];
            cfg.toilim                 = [0.240 0.340];
            cfg.foilim                  = [3 15];
            cfg.channel                 = ft_channelselection('EEG',{'AF3', 'F3', 'FC5', 'F7', 'FZ', 'FC1', 'C3', 'FC6', 'F8'})
            grandavg_standard        = ft_freqgrandaverage(cfg, standard_all{:});
            grandavg_deviant         = ft_freqgrandaverage(cfg, deviant_all{:});

            % grandavg_standard        = ft_freqdescriptives(cfg, standard_all{:});
            % grandavg_deviant         = ft_freqdescriptives(cfg, deviant_all{:});

            grandavg_diff = grandavg_standard
            grandavg_diff.powspctrm = grandavg_deviant.powspctrm - grandavg_standard.powspctrm
            %%

            % cfg           = [];
            % cfg.operation = 'x2-x1';
            % cfg.parameter = 'avg';
            %
            % difference_wave = ft_math(cfg, grandsavg_standard, grandavg_deviant);


            % Plot the resultsf



            cfg                      = [];
            cfg.layout               = 'biosemi32.lay';
            cfg.interactive          = 'yes';
            cfg.showoutline          = 'yes';
            cfg.showlabels           = 'yes';
            cfg.xlim                 = [.240 .340];
            %cfg.zlim                 = [-0.2 0.2];
            cfg.colorbar             = 'yes';
            cfg.colorbartext         = 'yes';
            cfg.colormap             = 'jet';
           % cfg.channel                 = ft_channelselection('EEG',{'AF3', 'F3', 'FC5', 'F7', 'FZ', 'FC1', 'C3', 'FC6', 'F8'})

            figure('NumberTitle', 'off','WindowState', 'maximized');
            ft_multiplotTFR(cfg, grandavg_standard);
            set(gcf, 'Name', char(strcat('grandavg_standard-',stimulus(stim), '_',condition(cond),'_',groupName(group))))
            figure('NumberTitle', 'off','WindowState', 'maximized');
            ft_multiplotTFR(cfg,  grandavg_deviant);
            set(gcf, 'Name', char(strcat('grandavg_deviant-',stimulus(stim), '_',condition(cond),'_',groupName(group))))
            figure('NumberTitle', 'off','WindowState', 'maximized');
            ft_multiplotTFR(cfg, grandavg_diff);
            set(gcf, 'Name', char(strcat('grandavg_difference-',groupName(group),'_',stimulus(stim), '_',condition(cond))))


            % Save the data
            save(fullfile(output_dir, 'grandaverage_standard.mat'), 'grandavg_standard');
            save(fullfile(output_dir, 'grandaverage_deviant.mat'), 'grandavg_deviant');
            savefig(gcf, fullfile(output_dir, 'topoplot_grandaverage_standard_deviant'));

            %% 3.3 Perform cluster-based permutation statistics correcting for multiple comparisons

            %% 3.3.1 Perform cluster-based test
            cfg_neighb        = [];
            cfg_neighb.method = 'triangulation';
            cfg.layout = 'biosemi32.lay';
            neighbours = ft_prepare_neighbours(cfg_neighb, ft_data_std);

            cfg                       = [];
            cfg.channel                 = ft_channelselection('EEG',{'AF3', 'F3', 'FC5', 'F7', 'FZ', 'FC1', 'C3', 'FC6', 'F8'})
            cfg.latency               = [.240 .340]; %TODO
            cfg.frequency             = [3 15];
            % required for a selected sample to be included
            % in the clustering algorithm (default=0).
            cfg.neighbours            = neighbours;  % as defined for this channel layout
            %cfg.parameter            = 'powspctrm';
            cfg.method                = 'montecarlo';
            cfg.statistic             = 'ft_statfun_depsamplesT';
            cfg.alpha                 = 0.05;
            cfg.clusteralpha          = 0.05;
            cfg.clusterstatistic      = 'maxsum';
            cfg.correctm              = 'cluster';
            cfg.correcttail           = 'prob';
            cfg.numrandomization      = 1000;

            Nsub                      = length(standard_all);
            cfg.design(1,1:2*Nsub)    = [ones(1,Nsub) 2*ones(1,Nsub)];
            cfg.design(2,1:2*Nsub)    = [1:Nsub 1:Nsub];
            cfg.ivar                  = 1; % the 1st row in cfg.design contains the independent variable
            cfg.uvar                  = 2; % the 2nd row in cfg.design contains the subject number (uni variable)

            stat_standard_deviant_clusstats  = ft_freqstatistics(cfg, standard_all{:}, deviant_all{:});

            % Save the data
            save(fullfile(output_dir, 'stat_standard_deviant_clusstats.mat'), 'stat_standard_deviant_clusstats');


            %%
            %             cfg = [];
            % cfg.alpha  = 0.025;
            % cfg.parameter = 'stat';
            % cfg.zlim   = [-4 4];
            % cfg.layout = 'biosemi32.lay';
            % ft_clusterplot(cfg, stat_standard_deviant_clusstats);



            %% 3.3.2 Plot the results of the cluster-based permutation test

            %load(fullfile(output_dir, 'stat_standard_deviant_clusstats.mat'), 'stat_standard_deviant_clusstats');


            pos_cluster_pvals       = [stat_standard_deviant_clusstats.posclusters(:).prob];
            pos_clust               = find(pos_cluster_pvals < 0.025);
            pos                     = ismember(stat_standard_deviant_clusstats.posclusterslabelmat, pos_clust);

            neg_cluster_pvals       = [stat_standard_deviant_clusstats.negclusters(:).prob];
            neg_clust               = find(neg_cluster_pvals < 0.025);
            neg                     = ismember(stat_standard_deviant_clusstats.negclusterslabelmat, neg_clust);



            cfg = [];
            cfg.alpha  = 0.025;
            cfg.parameter = 'stat'; 
            cfg.zlim   = [-4 4];
            cfg.xlim   = [0 0.7];
            cfg.layout = 'biosemi32.lay';


            cfg                      = [];
            cfg.layout               = 'biosemi32.lay';
            cfg.interactive          = 'yes';
            cfg.showoutline          = 'yes';
            cfg.showlabels           = 'yes';
            cfg.zlim   = [0 1];
            cfg.xlim   = [0 0.7];
            cfg.xlim                 = [.240 .340];
            % cfg.colormap             = 'jet';

            grandavg_diff_temp = grandavg_diff
            grandavg_diff_temp.powspctrm = grandavg_diff_temp.powspctrm .* neg;
            ft_multiplotTFR(cfg, grandavg_diff_temp);

            grandavg_diff_temp = grandavg_diff
            grandavg_diff_temp.powspctrm = grandavg_diff_temp.powspctrm .* pos;
            ft_multiplotTFR(cfg, grandavg_diff_temp);

            % % Alternatively, plot only the first positive/negative cluster
            % pos = stat_expected_unexpected_clusstats.posclusterslabelmat ==1;
            % neg = stat_expected_unexpected_clusstats.negclusterslabelmat ==1;

            % Set plotting specifications
            timestep                = 0.05; % plot every 0.05 sec intervals
            sampling_rate           = 500; % set sampling frequency
            sample_count            = length(stat_standard_deviant_clusstats.time);
            j                       = [stat_standard_deviant_clusstats.time(1):timestep:stat_standard_deviant_clusstats.time(end)]; % start of each interval for plotting in seconds
            m                       = [1:timestep*sampling_rate:sample_count]; % start of each interval for plotting in sample points

            [i1,i2] = match_str(grandavg_diff.label, stat_standard_deviant_clusstats.label); % matching channel labels

            mini = [mini min(grandavg_diff.powspctrm,[],"all")];
            maxi = [maxi max(grandavg_diff.powspctrm,[],"all")];
            zlim = max([3.6 ceil(max([abs(min(grandavg_diff.powspctrm,[],"all")) max(grandavg_diff.powspctrm,[],"all")])) ]);


            figure('NumberTitle', 'off', 'Name', char(strcat(stimulus(stim), '_',condition(cond),'_',groupName(group))),'WindowState', 'maximized');

            %% end clean up
            close all
            clearvars -except indirA  indirP indir outdir stimulus stim groupName groupCode group condition cond mini maxi
 
