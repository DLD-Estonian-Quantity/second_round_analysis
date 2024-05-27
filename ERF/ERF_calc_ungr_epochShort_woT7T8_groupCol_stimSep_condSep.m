% this script is calculating the event-related field cluster-based 
% permutation test, the code is based on the tutorial for event-related
% field permutation test of fieldtrip and a script published by Marlene
% Meyer and Robert Oostenveld under GPLv3
% (https://github.com/Donders-Institute/infant-cluster-effectsize/blob/main/do_group_analysis.m)


clear; eeglab nogui; close all;
addpath('C:\Users\eriksam1\Documents\MATLAB\fieldtrip-master');
ft_defaults;

%% init the dirs
indir = 'L:\eeg\lastekatse2021\Erik\data\working\preprocessing\ungrouped';
outdir = ['L:\eeg\lastekatse2021\Erik\data\working\analysis\ERF_permTest_ungrouped_meyer_epochShort_woT7T8_groupCol_stimSep_condSep_01'];

%% Read files to analyses


mini = [];
maxi = [];
% KP -> clinical
% EK -> typically developing


%% Create output folders to save data

stimulus = ["sada", "vere"];
devTime = [0.090, 0.086];
condition = ["64", "32", "16"];
groupCode = ["EK", "KP"];
groupName = ["typical", "clinical"];

%%
output_dir = outdir;
if exist(output_dir, 'dir') == 0
    mkdir(output_dir);
end


for cond = 1:length(condition)


    for stim = 1:length(stimulus)
        subjects_total = 0
        for group = 1:length(groupCode)
            indir_current = char(strcat(indir ,filesep,groupName(group),filesep,stimulus(stim), filesep, condition(cond)));
            output_dir = char(strcat(outdir,filesep,stimulus(stim), filesep, condition(cond)));
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

                cfg =[];
                cfg.channel   = ft_channelselection({'all','-T7','-T8'},ft_data_std)
                ft_data_std = ft_selectdata(cfg,ft_data_std)
                
                cfg =[];
                cfg.channel   = ft_channelselection({'all','-T7','-T8'},ft_data_dev)
                ft_data_dev  = ft_selectdata(cfg,ft_data_dev)

                cfg = [];
                cfg.toilim = [devTime(stim) 0.698];
                ft_data_std = ft_redefinetrial(cfg, ft_data_std);
                ft_data_dev = ft_redefinetrial(cfg, ft_data_dev);

                standard_all(ii+subjects_total) = { ft_data_std };
                deviant_all(ii+subjects_total) = { ft_data_dev };

            end
            subjects_total = subjects_total + length(subjects)


        end


        % Calculate grand average for both conditions (standard, deviant) separately
        cfg                      = [];
        cfg.channel              = 'all';
        cfg.latency              = 'all';
        cfg.parameter            = 'avg';
        grandavg_standard        = ft_timelockgrandaverage(cfg, standard_all{:});
        grandavg_deviant         = ft_timelockgrandaverage(cfg, deviant_all{:});
        %%
        cfg           = [];
        cfg.operation = 'x2-x1';
        cfg.parameter = 'avg';

        difference_wave = ft_math(cfg, grandavg_standard, grandavg_deviant);


        % Plot the results
        figure('NumberTitle', 'off','WindowState', 'maximized');
        cfg                      = [];
        cfg.layout               = 'biosemi32.lay';
        % cfg.xlim             =[devTime(stim) 0.698];
        cfg.interactive          = 'yes';
        cfg.showoutline          = 'yes';
        cfg.showlabels           = 'yes';
        ft_multiplotER(cfg,difference_wave, grandavg_standard, grandavg_deviant);
        set(gcf, 'Name', char(strcat('grandavg_difference-',stimulus(stim), '_',condition(cond),'_',groupName(group))))

        % Save the data
        save(fullfile(output_dir, 'grandaverage_standard.mat'), 'grandavg_standard');
        save(fullfile(output_dir, 'grandaverage_deviant.mat'), 'grandavg_deviant');
        savefig(gcf, fullfile(output_dir, 'topoplot_grandaverage_standard_deviant'));

        %% 3.3 Perform cluster-based permutation statistics correcting for multiple comparisons

        % 3.3.1 Perform cluster-based test
        cfg_neighb        = [];
        cfg_neighb.method = 'triangulation';
        neighbours = ft_prepare_neighbours(cfg_neighb, ft_data_std);

        cfg                       = [];
        cfg.channel               = 'EEG';
        % required for a selected sample to be included
        % in the clustering algorithm (default=0).
        % cfg.latency               = [devTime(stim) 0.698]
        cfg.neighbours     = neighbours;  % as defined for this channel layout
        cfg.parameter             = 'avg';
        cfg.method                = 'montecarlo';
        cfg.statistic             = 'ft_statfun_depsamplesT';
        cfg.alpha                 = 0.05;
        cfg.correctm              = 'cluster';
        cfg.correcttail           = 'prob';
        cfg.numrandomization      = 10000;

        Nsub                      = subjects_total;
        cfg.design(1,1:2*Nsub)    = [ones(1,Nsub) 2*ones(1,Nsub)];
        cfg.design(2,1:2*Nsub)    = [1:Nsub 1:Nsub];
        cfg.ivar                  = 1; % the 1st row in cfg.design contains the independent variable
        cfg.uvar                  = 2; % the 2nd row in cfg.design contains the subject number

        stat_standard_deviant_clusstats  = ft_timelockstatistics(cfg, standard_all{:}, deviant_all{:});

        % Save the data
        save(fullfile(output_dir, 'stat_standard_deviant_clusstats.mat'), 'stat_standard_deviant_clusstats');

        %% 3.3.2 Plot the results of the cluster-based permutation test

        load(fullfile(output_dir, 'stat_standard_deviant_clusstats.mat'), 'stat_standard_deviant_clusstats');

        % Plot displaying t- and p-value distribution across channels and time
        plot_clus = zeros(size(stat_standard_deviant_clusstats.prob));
        plot_clus(stat_standard_deviant_clusstats.negclusterslabelmat==1) = -1; % negative cluster
        plot_clus(stat_standard_deviant_clusstats.posclusterslabelmat==1) =  1; % positive cluster

        figure('NumberTitle', 'off','WindowState', 'maximized','Name',char(strcat('T_and_Pvalues_stat_standard_deviant_clusstats-',stimulus(stim), '_',condition(cond),'_',groupName(group))))
        subplot(2,1,1)
        imagesc(stat_standard_deviant_clusstats.time, 1:size(stat_standard_deviant_clusstats.label,1),plot_clus)
        colormap(jet)
        colorbar

        title('Largest positive and negative cluster');
        subplot(2,1,2)
        imagesc(stat_standard_deviant_clusstats.time, 1:size(stat_standard_deviant_clusstats.label,1),  stat_standard_deviant_clusstats.stat)
        colorbar
        title('T-values per channel x time');
        savefig(gcf, fullfile(output_dir, 'T_and_Pvalues_stat_standard_deviant_clusstats'));

        % Plot displaying topographic maps across time bins highlighting channel/time as part of clusters

        % For this purpose, calculate the difference between conditions
        cfg                       = [];
        cfg.operation             = 'x2-x1';
        cfg.parameter             = 'avg';
        grandavg_diff_standard_deviant = ft_math(cfg, grandavg_standard, grandavg_deviant);

        % Find clusters with a 5% two-sided cutoff based on the cluster p-values
        pos_cluster_pvals       = [stat_standard_deviant_clusstats.posclusters(:).prob];
        pos_clust               = find(pos_cluster_pvals < 0.025);
        pos                     = ismember(stat_standard_deviant_clusstats.posclusterslabelmat, pos_clust);

        neg_cluster_pvals       = [stat_standard_deviant_clusstats.negclusters(:).prob];
        neg_clust               = find(neg_cluster_pvals < 0.025);
        neg                     = ismember(stat_standard_deviant_clusstats.negclusterslabelmat, neg_clust);

        % % Alternatively, plot only the first positive/negative cluster
        % pos = stat_expected_unexpected_clusstats.posclusterslabelmat ==1;
        % neg = stat_expected_unexpected_clusstats.negclusterslabelmat ==1;

        % Set plotting specifications
        timestep                = 0.05; % plot every 0.05 sec intervals
        sampling_rate           = 500; % set sampling frequency
        sample_count            = length(stat_standard_deviant_clusstats.time);
        j                       = [stat_standard_deviant_clusstats.time(1):timestep:stat_standard_deviant_clusstats.time(end)]; % start of each interval for plotting in seconds
        m                       = [1:timestep*sampling_rate:sample_count]; % start of each interval for plotting in sample points

        [i1,i2] = match_str(grandavg_diff_standard_deviant.label, stat_standard_deviant_clusstats.label); % matching channel labels

        mini = [mini min(grandavg_diff_standard_deviant.avg,[],"all")];
        maxi = [maxi max(grandavg_diff_standard_deviant.avg,[],"all")];
        zlim = max([3.6 ceil(max([abs(min(grandavg_diff_standard_deviant.avg,[],"all")) max(grandavg_diff_standard_deviant.avg,[],"all")])) ]);


        figure('NumberTitle', 'off', 'Name', char(strcat(stimulus(stim), '_',condition(cond),'_',groupName(group))),'WindowState', 'maximized');
        for k = 1:(length(j)-1)

            cfg                  = [];
            cfg.figure           = subplot(3,5,k);
            cfg.xlim             = [j(k) j(k+1)]; % current interval
            cfg.zlim             = [-zlim zlim]; % set minimum and maximum z-axis

            pos_int              = zeros(numel(grandavg_diff_standard_deviant.label),1);
            neg_int              = zeros(numel(grandavg_diff_standard_deviant.label),1);
            pos_int(i1)          = all(pos(i2, m(k):m(k+1)),2); % determine which channels are in a cluster throughout the current time interval (pos cluster)
            neg_int(i1)          = all(neg(i2, m(k):m(k+1)),2); % determine which channels are in a cluster throughout the current time interval (neg cluster)

            cfg.highlight        = 'on';
            cfg.highlightchannel = find(pos_int | neg_int); % highlight channels belonging to a cluster
            cfg.highlightcolor   = [1 1 1]; % highlight marker color (default = [0 0 0] (black))
            cfg.comment          = 'xlim';
            cfg.commentpos       = 'title';
            cfg.layout           =  'biosemi32.lay';
            cfg.interactive      = 'no';
            cfg.figure      = 'gca';
            ft_topoplotER(cfg, grandavg_diff_standard_deviant)
            colorbar
            colormap(jet)
        end

        % Save the figure
        savefig(gcf, fullfile(output_dir, 'topoplot_stat_standard_deviant_clusstats'));

        %% 3.4 Determine effect size
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % 3.4.1 Option 1: Calculate Cohen's d for the average difference in the respective cluster
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        % First for the positive cluster
        effect_window_pos = stat_standard_deviant_clusstats.posclusterslabelmat==1;

        % Calculate pairwise difference between conditions for each participant
        for ii = 1:size(subjects,1)
            folder                  = [indir_current filesep subjects{ii}];
            load(char(strcat(folder,filesep,'ft_data_std.mat')));
            load(char(strcat(folder,filesep,'ft_data_dev.mat')));


            a = ft_data_std.avg(effect_window_pos);
            b = ft_data_dev.avg(effect_window_pos);
            c = a-b;
            Pos.ERP_Diff_alltimechan(ii,:) =c;
            Pos.ERP_Diff(ii) = nanmean([a - b]);
            clear expected unexpected a b c

        end

        % Calculate Cohen's d
        Pos.stdev_ERP_diff      = std(Pos.ERP_Diff);
        Pos.mean_ERP_diff       = mean(Pos.ERP_Diff);
        Pos.cohensd_ERP_diff    = Pos.mean_ERP_diff/Pos.stdev_ERP_diff;

        % Then for the negative clustesr
        effect_window_neg = stat_standard_deviant_clusstats.negclusterslabelmat==1;

        % Calculate pairwise difference between conditions for each participant
        for ii = 1:size(subjects,1)
            folder                  = [indir_current filesep subjects{ii}];
            load([folder filesep 'ft_data_std.mat']);
            load([folder filesep 'ft_data_dev.mat']);

            a = ft_data_std.avg(effect_window_neg);
            b = ft_data_dev.avg(effect_window_neg);
            c = a-b;
            Neg.ERP_Diff_alltimechan(ii,:) =c;
            Neg.ERP_Diff(ii) = nanmean([a - b]);
            clear expected unexpected a b c

        end

        % Calculate Cohen's d
        Neg.stdev_ERP_diff      = std(Neg.ERP_Diff);
        Neg.mean_ERP_diff       = mean(Neg.ERP_Diff);
        Neg.cohensd_ERP_diff    = Neg.mean_ERP_diff/Neg.stdev_ERP_diff;

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % 3.4.2 Option 2: Determine at maximum effect size and at which channel/time it
        % is maximal (upper bound)
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

        % Determine maximum effect size and at which channel and time point Cohen's d is maximal
        for t = 1:size(Pos.ERP_Diff_alltimechan,2)
            Pos.cohensd_ERP_Diff_alltimechan(t) =(nanmean(Pos.ERP_Diff_alltimechan(:,t)))/(std(Pos.ERP_Diff_alltimechan(:,t)));
        end

        [Pos.cohensd_ERP_Diff_Max, Pos.idx]     = max(Pos.cohensd_ERP_Diff_alltimechan);
        [Pos.row,Pos.col]                       = find(stat_standard_deviant_clusstats.posclusterslabelmat==1);

        % Determine maximum effect size and at which channel and time point Cohen's d is maximal

        for t = 1:size(Neg.ERP_Diff_alltimechan,2)
            Neg.cohensd_ERP_Diff_alltimechan(t) =(nanmean(Neg.ERP_Diff_alltimechan(:,t)))/(std(Neg.ERP_Diff_alltimechan(:,t)));
        end

        [Neg.cohensd_ERP_Diff_Max, Neg.idx]     = min(Neg.cohensd_ERP_Diff_alltimechan);
        [Neg.row,Neg.col]                       = find(stat_standard_deviant_clusstats.negclusterslabelmat==1);

        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % 3.4.3 Option 3: % Calculate effect size on rectangle around cluster results (lower bound)
        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        % Calculate grand average, keeping information about individual
        % participants
        cfg                      = [];
        cfg.channel              = 'all';
        cfg.latency              = 'all';
        cfg.parameter            = 'avg';
        cfg.keepindividual       = 'yes';
        grandavg_standard_all    = ft_timelockgrandaverage(cfg, standard_all{:});
        grandavg_deviant_all     = ft_timelockgrandaverage(cfg, deviant_all{:});

        % First for the largest positive cluster

        % Determine time and channels of the largest positive cluster
        [Pos.row,Pos.col] = find(stat_standard_deviant_clusstats.posclusterslabelmat==1); % row = channel; col = time
        idx_time_min = min(Pos.col);
        idx_time_max = max(Pos.col);

        % Estimate rectangular window around this cluster
        Pos.rect_t_min = stat_standard_deviant_clusstats.time(idx_time_min);
        Pos.rect_t_max = stat_standard_deviant_clusstats.time(idx_time_max);
        Pos.rect_chan = stat_standard_deviant_clusstats.label(any(stat_standard_deviant_clusstats.mask(:,idx_time_min:idx_time_max),2));
        %%
        % Calculate effect size (Cohen's d) withing this rectengular window
        cfg                     = [];
        cfg.channel             = Pos.rect_chan;
        cfg.latency             = [Pos.rect_t_min Pos.rect_t_max];
        cfg.avgoverchan         = 'yes';
        cfg.avgovertime         = 'yes';
        cfg.method              = 'analytic';
        cfg.statistic           = 'cohensd';
        cfg.ivar                = 1;
        cfg.uvar                = 2;
        Nsub                    = subjects_total;
        cfg.design(1,1:2*Nsub)  = [ones(1,Nsub) 2*ones(1,Nsub)];
        cfg.design(2,1:2*Nsub)  = [1:Nsub 1:Nsub];

        effect_rectangle_pos = ft_timelockstatistics(cfg, grandavg_standard_all, grandavg_deviant_all);
        Pos.effect_rectangle = effect_rectangle_pos;

        % Determine time and channels of the largest negative cluster
        [Neg.row,Neg.col] = find(stat_standard_deviant_clusstats.negclusterslabelmat==1);
        idx_time_min = min(Neg.col);
        idx_time_max = max(Neg.col);

        % Estimate rectangular window around this cluster
        Neg.rect_t_min = stat_standard_deviant_clusstats.time(idx_time_min);
        Neg.rect_t_max = stat_standard_deviant_clusstats.time(idx_time_max);
        Neg.rect_chan = stat_standard_deviant_clusstats.label(any(stat_standard_deviant_clusstats.mask(:,idx_time_min:idx_time_max),2));

        % Calculate effect size (Cohen's d) withing this rectengular window
        cfg                     = [];
        cfg.channel             = Neg.rect_chan;
        cfg.latency             = [Neg.rect_t_min Neg.rect_t_max];
        cfg.avgoverchan         = 'yes';
        cfg.avgovertime         = 'yes';
        cfg.method              = 'analytic';
        cfg.statistic           = 'cohensd';
        cfg.ivar                = 1;
        cfg.uvar                = 2;
        Nsub                    = subjects_total;
        cfg.design(1,1:2*Nsub)  = [ones(1,Nsub) 2*ones(1,Nsub)];
        cfg.design(2,1:2*Nsub)  = [1:Nsub 1:Nsub];

        effect_rectangle_neg = ft_timelockstatistics(cfg, grandavg_standard_all, grandavg_deviant_all);
        Neg.effect_rectangle = effect_rectangle_neg;

        % Display results
        fprintf('\n')
        disp('~~~~~')
        disp(['Contrast: Standard vs. deviant: ']);
        disp(['Effect size Cohens d of average positive cluster is ' num2str(Pos.cohensd_ERP_diff)])
        disp(['Maximum effect size is ' num2str(Pos.cohensd_ERP_Diff_Max) ' at channel ' ft_data_std.label{Pos.row(Pos.idx)} ' and at time ' num2str(ft_data_std.time(Pos.col(Pos.idx))) ' sec']);
        disp(['Effect size Cohens d of average on rectangle around positive cluster is ' num2str(effect_rectangle_pos.cohensd)])
        disp('~~~~~')
        fprintf('\n')

        fprintf('\n')
        disp('~~~~~')
        disp(['Contrast: Standard vs. deviant: '])
        disp(['Effect size Cohens d of average negative cluster is ' num2str(Neg.cohensd_ERP_diff)])
        disp(['Maximum effect size is ' num2str(Neg.cohensd_ERP_Diff_Max) ' at channel ' ft_data_std.label{Neg.row(Neg.idx)} ' and at time ' num2str(ft_data_std.time(Neg.col(Neg.idx))) ' sec']);
        disp(['Effect size Cohens d of average on rectangle around negative cluster is ' num2str(effect_rectangle_neg.cohensd)])
        disp('~~~~~')
        fprintf('\n')

        % Save the data
        save(fullfile(output_dir, 'EffectSize.mat'), 'Neg', 'Pos');

        %% 3.4.4 Plot ERP timecourse of the channels with the maximal effect size

        % Determine variability between participants
        se_grandavg_standard = squeeze(nanstd(grandavg_standard_all.individual/sqrt(length(subjects))));
        se_grandavg_deviant = squeeze(nanstd(grandavg_deviant_all.individual/sqrt(length(subjects))));

        % Set color specifications
        colour_code = {'b','r', 'k'};
        shaded_area = {[0, 0, 1], [1 0 0], [0, 0, 0]};

        % Plot the ERP of the channel with maximum effect size of positive Cluster
        figure('NumberTitle', 'off','WindowState', 'maximized','Name',char(strcat('ERP_maxEffect_pos-',stimulus(stim), '_',condition(cond),'_',groupName(group))))


        % Condition 1
        subplot(1,2,1)
        plot(grandavg_standard.time,grandavg_standard.avg(Pos.row(Pos.idx),:),colour_code{1}, 'LineWidth', 1.5)
        mean_standard = grandavg_standard.avg(Pos.row(Pos.idx),:);
        se_standard = se_grandavg_standard(Pos.row(Pos.idx),:);
        patch([grandavg_standard.time, fliplr(grandavg_standard.time)], [mean_standard-se_standard, fliplr(mean_standard+se_standard)],  shaded_area{1}, 'edgecolor', 'none', 'FaceAlpha', .3);

        hold all

        % Condition 2
        plot(grandavg_deviant.time,grandavg_deviant.avg(Pos.row(Pos.idx),:),colour_code{2}, 'LineWidth', 1.5)
        mean_deviant = grandavg_deviant.avg(Pos.row(Pos.idx),:);
        se_deviant = se_grandavg_deviant(Pos.row(Pos.idx),:);
        patch([grandavg_standard.time, fliplr(grandavg_standard.time)], [mean_deviant-se_deviant, fliplr(mean_deviant+se_deviant)],  shaded_area{2}, 'edgecolor', 'none', 'FaceAlpha', .3);

        % Shaded area to indicate positive cluster
        idx_pos_time = find(stat_standard_deviant_clusstats.posclusterslabelmat(Pos.row(Pos.idx),:)==1);
        hold all
        patch([grandavg_standard.time(idx_pos_time),fliplr(grandavg_standard.time(idx_pos_time))], [(ones(size(grandavg_standard.time(idx_pos_time),2),1)*-15)', fliplr((ones(size(grandavg_standard.time(idx_pos_time),2),1)*15)')], shaded_area{3}, 'edgecolor', 'none', 'FaceAlpha', .1)

        xlabel('Time [s]');
        ylabel('Amplitude [mV]');
        ylim([-6 6])
        line([-.1 0.7],[ 0 0], 'Color', [0 0 0],'LineStyle', ':')
        title(['Maximum effect of positive cluster at channel ' grandavg_standard.label(Pos.row(Pos.idx))])

        % Plot the ERP of the channel with maximum effect size of negative Cluster

        % Condition 1
        subplot(1,2,2)
        plot(grandavg_standard.time,grandavg_standard.avg(Neg.row(Neg.idx),:),colour_code{1}, 'LineWidth', 1.5)
        mean_standard = grandavg_standard.avg(Neg.row(Neg.idx),:);
        se_standard = se_grandavg_standard(Neg.row(Neg.idx),:);
        patch([grandavg_standard.time, fliplr(grandavg_standard.time)], [mean_standard-se_standard, fliplr(mean_standard+se_standard)],  shaded_area{1}, 'edgecolor', 'none', 'FaceAlpha', .3);

        hold all

        % Condition 2
        plot(grandavg_deviant.time,grandavg_deviant.avg(Neg.row(Neg.idx),:),colour_code{2}, 'LineWidth', 1.5)
        mean_deviant = grandavg_deviant.avg(Neg.row(Neg.idx),:);
        se_deviant = se_grandavg_deviant(Neg.row(Neg.idx),:);
        patch([grandavg_standard.time, fliplr(grandavg_standard.time)], [mean_deviant-se_deviant, fliplr(mean_deviant+se_deviant)],  shaded_area{2}, 'edgecolor', 'none', 'FaceAlpha', .3);

        % Shaded area to indicate negative cluster
        idx_neg_time = find(stat_standard_deviant_clusstats.negclusterslabelmat(Neg.row(Neg.idx),:)==1);
        hold all
        patch([grandavg_standard.time(idx_neg_time),fliplr(grandavg_standard.time(idx_neg_time))], [(ones(size(grandavg_standard.time(idx_neg_time),2),1)*-15)', fliplr((ones(size(grandavg_standard.time(idx_neg_time),2),1)*15)')], shaded_area{3}, 'edgecolor', 'none', 'FaceAlpha', .1)

        xlabel('Time [s]');
        ylabel('Amplitude [mV]');
        ylim([-6 6])
        line([-.1 0.7],[ 0 0], 'Color', [0 0 0],'LineStyle', ':')
        title(['Maximum effect of negative cluster at channel ' grandavg_standard.label(Neg.row(Neg.idx))])

        savefig(gcf, fullfile(output_dir, 'ERP_maxEffect_pos_neg'));

        %% 3.4.5 Plot effect size topography highlighting cluster-based permutation test results

        % Determine effect size for each channel x time pair
        cfg           = [];
        cfg.parameter = 'individual';
        cfg.method    = 'analytic';
        cfg.statistic = 'cohensd';
        cfg.ivar      = 1;
        cfg.uvar      = 2;
        num_sub       = length(standard_all);
        cfg.design    = [
            1*ones(1,num_sub) 2*ones(1,num_sub)
            1:num_sub         1:num_sub
            ];

        effect_all_with_mask = ft_timelockstatistics(cfg, grandavg_standard_all, grandavg_deviant_all);

        % Create mask to indicate clusters
        effect_all_with_mask.mask = stat_standard_deviant_clusstats.mask;
        figure('NumberTitle', 'off','WindowState', 'maximized','Name',char(strcat('effect_all_with_mask-',stimulus(stim), '_',condition(cond),'_',groupName(group))))

        cfg               = [];
        cfg.layout        = 'biosemi32.lay';
        cfg.parameter     = 'cohensd';
        cfg.maskparameter = 'mask';
        cfg.linecolor     = [0 0 0];
        ft_multiplotER(cfg,effect_all_with_mask)
        set(gcf, 'Name', char(strcat('effect_all_with_mask-',stimulus(stim), '_',condition(cond),'_',groupName(group))))
        savefig(gcf, fullfile(output_dir, 'effect_all_with_mask'));
        close all
        clearvars -except indirA  indirP indir outdir stimulus devTime stim groupName groupCode group condition cond mini maxi
    end
end