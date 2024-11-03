%% Processing of Model Outputs for Joint Search over the Life Cycle
% Annika Bacher, Philipp Gruebener, Lukas Nord

clear; clc; close all;

plot_path = '';     % set path where figures are stored
path_base = '';     % set path where base model solution is stored
path_cf1 = '';      % set path where counterfactual with phi_us = 1/12 solution is stored
path_cf2 = '';      % set path where counterfactual with phi_us = 1 solution is stored
addpath('');        % set path where LS_targets input is stored

%% load relevant input files

cd(path_base)

% dimensions
dim = dlmread('dimensions.txt');
AA = dim(1);    % number of asset grid points
TT = dim(2);    % number of working age time periods
HH = dim(3);    % number of human capital grid points
NN = dim(4);    % number of HH in simulation

% Parameters and targets
parameters = dlmread('parameters.txt');
parameters = parameters';

targets = dlmread('targets.txt');
targets = targets';

delta = dlmread('delta.txt');
delta = delta';

assgrid = dlmread('assgrid.txt');
assgrid = assgrid';

hgrid = dlmread('hgrid.txt');
hgrid = hgrid';

wage = dlmread('wage.txt');
wage = wage';

% simulation output
sim_asset = dlmread('sim_asset.txt');
sim_asset = sim_asset';
sim_asset = sim_asset(1:NN*TT);
sim_asset = reshape(sim_asset,NN,TT);

sim_LS = dlmread('sim_LS.txt');
sim_LS = sim_LS';
sim_LS = sim_LS(1:NN*TT);
sim_LS = reshape(sim_LS,NN,TT);

sim_LS_h = dlmread('sim_LS_h.txt');
sim_LS_h = sim_LS_h';
sim_LS_h = sim_LS_h(1:NN*TT);
sim_LS_h = reshape(sim_LS_h,NN,TT);

sim_LS_sp = dlmread('sim_LS_sp.txt');
sim_LS_sp = sim_LS_sp';
sim_LS_sp = sim_LS_sp(1:NN*TT);
sim_LS_sp = reshape(sim_LS_sp,NN,TT);

sim_hh = dlmread('sim_hh.txt');
sim_hh = sim_hh';
sim_hh = sim_hh(1:NN*TT);
sim_hh = reshape(sim_hh,NN,TT);

sim_hsp = dlmread('sim_hsp.txt');
sim_hsp = sim_hsp';
sim_hsp = sim_hsp(1:NN*TT);
sim_hsp = reshape(sim_hsp,NN,TT);

%% Assets and Income by Group

% assets
a_med_all = median(assgrid(sim_asset),'all');
a_med_groups(1) = median(assgrid(sim_asset(:,1:120)),'all');
a_med_groups(2) = median(assgrid(sim_asset(:,121:240)),'all');
a_med_groups(3) = median(assgrid(sim_asset(:,241:360)),'all');
a_med_groups(4) = median(assgrid(sim_asset(:,361:480)),'all');

% income
inc_h = wage(sim_hh).*(sim_LS_h==1);
inc_sp = wage(sim_hsp).*(sim_LS_sp==1);
inc= [inc_h;inc_sp];

inc_mean_all = mean(inc,'all')/0.72*(NN*TT*2/sum(inc(:)>0));
inc_mean_groups(1) = mean(inc(:,1:120),'all')/0.72*(NN*TT/4*2/sum(sum(inc(:,1:120)>0)));
inc_mean_groups(2) = mean(inc(:,121:240),'all')/0.72*(NN*TT/4*2/sum(sum(inc(:,121:240)>0)));
inc_mean_groups(3) = mean(inc(:,241:360),'all')/0.72*(NN*TT/4*2/sum(sum(inc(:,241:360)>0)));
inc_mean_groups(4) = mean(inc(:,361:480),'all')/0.72*(NN*TT/4*2/sum(sum(inc(:,361:480)>0)));

% earnings losses
inc_loss = targets(55:58);

%% Labor market states
  
LS_targets=dlmread('LS_Targets.txt');
LS_targets = LS_targets(1:30);

LS_model = targets(1:30);

set(groot,'defaultAxesTickLabelInterpreter','latex');  

fig=figure;
hold on
subplot(2,2,1)
b=bar([LS_model(7:12), LS_targets(7:12)],'FaceColor', 'flat');
b(1).FaceColor = [0.35 0.35 0.35];
b(2).FaceColor = [1 1 1];
b(1).EdgeColor = [0.35 0.35 0.35];
b(2).EdgeColor = [0.70 0.00 0.00];
b(1).LineWidth = 1.5;
b(2).LineWidth = 1.5;
set(gca,'LineWidth',1.5)
set(gca,'XTick',[])
ylim([0, 0.85])
yticks([0 0.2 0.4 0.6 0.8])
title('Age 26-35','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12)
subplot(2,2,2)
b=bar([LS_model(13:18) LS_targets(13:18)],'FaceColor', 'flat');
b(1).FaceColor = [0.35 0.35 0.35];
b(2).FaceColor = [1 1 1];
b(1).EdgeColor = [0.35 0.35 0.35];
b(2).EdgeColor = [0.70 0.00 0.00];
b(1).LineWidth = 1.5;
b(2).LineWidth = 1.5;
set(gca,'LineWidth',1.5)
set(gca,'XTick',[])
ylim([0, 0.85])
yticks([0 0.2 0.4 0.6 0.8])
title('Age 36-45','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12)
subplot(2,2,3)
b=bar([LS_model(19:24) LS_targets(19:24)],'FaceColor', 'flat');
b(1).FaceColor = [0.35 0.35 0.35];
b(2).FaceColor = [1 1 1];
b(1).EdgeColor = [0.35 0.35 0.35];
b(2).EdgeColor = [0.70 0.00 0.00];
b(1).LineWidth = 1.5;
b(2).LineWidth = 1.5;
set(gca,'LineWidth',1.5)
xticklabels({'EE', 'EU', 'EN', 'UU', 'UN', 'NN'})
ylim([0, 0.85])
yticks([0 0.2 0.4 0.6 0.8])
title('Age 46-55','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12)
subplot(2,2,4)
b=bar([LS_model(25:30) LS_targets(25:30)],'FaceColor', 'flat');
b(1).FaceColor = [0.35 0.35 0.35];
b(2).FaceColor = [1 1 1];
b(1).EdgeColor = [0.35 0.35 0.35];
b(2).EdgeColor = [0.70 0.00 0.00];
b(1).LineWidth = 1.5;
b(2).LineWidth = 1.5;
set(gca,'LineWidth',1.5)
xticklabels({'EE', 'EU', 'EN', 'UU', 'UN', 'NN'})
ylim([0, 0.85])
yticks([0 0.2 0.4 0.6 0.8])
title('Age 56-65','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12)
set(gca,'TickLabelInterpreter','LaTex')
legend({'Model', 'Data'},'Interpreter','LaTex','Fontsize',12);
legend boxoff
saveas(gcf,[plot_path 'LS_fit.pdf'])
hold off

%% individual transition rates

sim_LS_h_3 = sim_LS_h;
sim_LS_h_3(sim_LS_h_3==4)=2;
sim_LS_sp_3 = sim_LS_sp;
sim_LS_sp_3(sim_LS_sp_3==4)=2;
trans_h = filltrans(sim_LS_h_3);
trans_sp = filltrans(sim_LS_sp_3);
trans_all = filltrans([sim_LS_h_3;sim_LS_sp_3]);
trans_age(:,:,1) = filltrans([sim_LS_h_3(:,1:120);sim_LS_sp_3(:,1:120)]); 
trans_age(:,:,2) = filltrans([sim_LS_h_3(:,120:240);sim_LS_sp_3(:,120:240)]); % start where previous group finishes to include all transitions
trans_age(:,:,3) = filltrans([sim_LS_h_3(:,240:360);sim_LS_sp_3(:,240:360)]);
trans_age(:,:,4) = filltrans([sim_LS_h_3(:,360:480);sim_LS_sp_3(:,360:480)]);

%% Assets EN couples
dist_ass_LS=zeros(16,AA);
dist_ass_LS_T=zeros(16,AA,TT);

for t=1:TT
    for n=1:NN
        dist_ass_LS(sim_LS(n,t),sim_asset(n,t))=dist_ass_LS(sim_LS(n,t),sim_asset(n,t))+1;
        dist_ass_LS_T(sim_LS(n,t),sim_asset(n,t),t)=dist_ass_LS_T(sim_LS(n,t),sim_asset(n,t),t)+1;
    end
end

dist_ass_LS=dist_ass_LS/(NN*TT);
dist_ass_LS_T=dist_ass_LS_T/NN;

% EN couples by age group
dist_ass_EN_y=sum(sum(dist_ass_LS_T(4:5,:,1:119),3),1)/sum(sum(sum(dist_ass_LS_T(4:5,:,1:119))));
dist_ass_EN_o=sum(sum(dist_ass_LS_T(4:5,:,361:479),3),1)/sum(sum(sum(dist_ass_LS_T(4:5,:,361:479))));

fig=figure;
hold on
plot(assgrid,dist_ass_EN_y,'Linewidth',3.25,'Color','[0.70 0.0 0.0]','LineStyle','-')
plot(assgrid,dist_ass_EN_o,'Linewidth',3.25,'Color','[0.70 0.0 0.0]','LineStyle',':')
xline(dist_ass_EN_y*assgrid,'Linewidth',3.25,'Color','[0.70 0.0 0.0]','LineStyle','-')
xline(dist_ass_EN_o*assgrid,'Linewidth',3.25,'Color','[0.70 0.0 0.0]','LineStyle',':')
xlim([0 45])
ylim([0 0.30])
xticks(10:10:40)
legend({'young EN (25-35)','old EN (56-65)'},'Interpreter','LaTex','Fontsize',12)
legend boxoff
xlabel('assets $a$ (in \$10k)','Interpreter','LaTex','Fontsize',12)
ylabel('cond.\ distribution','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12) 
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'dist_ass_EN.pdf'])
hold off

%% distribution of HC for old and young EN
 
% EN/NE: Out of the labor force spouse
N_EN_y = sum(sum(sim_LS(:,1:119)==4));
N_NE_y = sum(sum(sim_LS(:,1:119)==5));
dist_hout_y_1 = zeros(N_EN_y,1);
dist_hout_y_2 = zeros(N_NE_y,1);
iEN = 1;
iNE = 1;
for i = 1:160000
  for t = 1:119
      if (sim_LS(i,t) == 4) 
          dist_hout_y_1(iEN) = sim_hsp(i,t);
          iEN = iEN+1;
      end
      if (sim_LS(i,t) == 5) 
          dist_hout_y_2(iNE) = sim_hh(i,t);
          iNE = iNE+1;
      end
  end
end
dist_hout_y = [dist_hout_y_1;dist_hout_y_2];
  
N_EN_o = sum(sum(sim_LS(:,361:479)==4));
N_NE_o = sum(sum(sim_LS(:,361:479)==5));
dist_hout_o_1 = zeros(N_EN_o,1);
dist_hout_o_2 = zeros(N_NE_o,1);
iEN = 1;
iNE = 1;
for i = 1:160000
  for t = 361:479
      if (sim_LS(i,t) == 4) 
          dist_hout_o_1(iEN) = sim_hsp(i,t);
          iEN = iEN+1;
      end
      if (sim_LS(i,t) == 5) 
          dist_hout_o_2(iNE) = sim_hh(i,t);
          iNE = iNE+1;
      end
  end
end
dist_hout_o = [dist_hout_o_1;dist_hout_o_2];

fig = figure;
subplot(1,2,1)
histogram(dist_hout_y, 'Normalization','probability','FaceColor',[0.7 0.00 0.00])
xlim([0 12])
ylim([0 0.8])
xticks([2 4 6 8 10 12])
xtickangle(0)
xlabel('Human capital state','Interpreter','LaTex','Fontsize',12)
ylabel('Fraction among young','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12) 
set(gca,'TickLabelInterpreter','LaTex')
subplot(1,2,2)
histogram(dist_hout_o, 'Normalization','probability','FaceColor',[0.7 0.00 0.00])
xlim([0 12])
ylim([0 0.8])
xticks([2 4 6 8 10 12])
xtickangle(0)
xlabel('Human capital state','Interpreter','LaTex','Fontsize',12)
ylabel('Fraction among old','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12) 
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'dist_hout.pdf'])
    
% EN/NE: Employed spouse
N_EN_y = sum(sum(sim_LS(:,1:119)==4));
N_NE_y = sum(sum(sim_LS(:,1:119)==5));
dist_hemp_y_1 = zeros(N_EN_y,1);
dist_hemp_y_2 = zeros(N_NE_y,1);
iEN = 1;
iNE = 1;
for i = 1:160000
  for t = 1:119
      if (sim_LS(i,t) == 4) 
          dist_hemp_y_1(iEN) = sim_hh(i,t);
          iEN = iEN+1;
      end
      if (sim_LS(i,t) == 5) 
          dist_hemp_y_2(iNE) = sim_hsp(i,t);
          iNE = iNE+1;
      end
  end
end
dist_hemp_y = [dist_hemp_y_1;dist_hemp_y_2];

N_EN_o = sum(sum(sim_LS(:,361:479)==4));
N_NE_o = sum(sum(sim_LS(:,361:479)==5));
dist_hemp_o_1 = zeros(N_EN_o,1);
dist_hemp_o_2 = zeros(N_NE_o,1);
iEN = 1;
iNE = 1;
for i = 1:160000
  for t = 361:479
      if (sim_LS(i,t) == 4) 
          dist_hemp_o_1(iEN) = sim_hh(i,t);
          iEN = iEN+1;
      end
      if (sim_LS(i,t) == 5) 
          dist_hemp_o_2(iNE) = sim_hsp(i,t);
          iNE = iNE+1;
      end
  end
end
dist_hemp_o = [dist_hemp_o_1;dist_hemp_o_2];

fig=figure;
subplot(1,2,1)
histogram(dist_hemp_y, 'Normalization','probability','FaceColor',[0.7 0.00 0.00])
xlim([0 12])
ylim([0 0.4])
xticks([2 4 6 8 10 12])
xtickangle(0)
xlabel('Human capital state','Interpreter','LaTex','Fontsize',12)
ylabel('Fraction among young','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12) 
set(gca,'TickLabelInterpreter','LaTex')
subplot(1,2,2)
histogram(dist_hemp_o, 'Normalization','probability','FaceColor',[0.7 0.00 0.00])
xlim([0 12])
ylim([0 0.4])
xticks([2 4 6 8 10 12])
xtickangle(0)
xlabel('Human capital state','Interpreter','LaTex','Fontsize',12)
ylabel('Fraction among old','Interpreter','LaTex','Fontsize',12)
set(gca,'XGrid','off','YGrid','on','Fontsize',12) 
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'dist_hemp.pdf'])

%% arrival rate EN

lambda_NE_X = dlmread('lambda_NE_X.txt');
lambda_NE_X = lambda_NE_X';
lambda_NE_X = lambda_NE_X(1:AA*TT*HH*HH);
lambda_NE_X = reshape(lambda_NE_X,AA,HH,HH,TT);

sim_lambda_ne_x = zeros(NN,TT);
mean_lambda_ne_x_cf = zeros(1,TT);
sim_lambda_ne_x_count = zeros(NN,TT);

ass_y = 4; %get median from dist_ass_EN_y
ass_o = 7; %get median from dist_ass_EN_o

for i = 1:NN
    for t = 1:TT
        if (sim_LS(i,t) == 4) % EN
            sim_lambda_ne_x_count(i,t) = 1;
            sim_lambda_ne_x(i,t) = lambda_NE_X(sim_asset(i,t),sim_hsp(i,t),sim_hh(i,t),t);
            mean_lambda_ne_x_cf = mean_lambda_ne_x_cf + squeeze(lambda_NE_X(sim_asset(i,t),sim_hsp(i,t),sim_hh(i,t),:))';

        elseif (sim_LS(i,t) == 5) % NE
            sim_lambda_ne_x_count(i,t) = 1;
            sim_lambda_ne_x(i,t) = lambda_NE_X(sim_asset(i,t),sim_hh(i,t),sim_hsp(i,t),t);
            mean_lambda_ne_x_cf = mean_lambda_ne_x_cf + squeeze(lambda_NE_X(sim_asset(i,t),sim_hh(i,t),sim_hsp(i,t),:))';

        end
    end
end        

mean_lambda_ne_x = sum(sim_lambda_ne_x)./sum(sim_lambda_ne_x_count);
mean_lambda_ne_x_cf = mean_lambda_ne_x_cf./sum(sum(sim_lambda_ne_x_count));

fig = figure;
hold on
plot((1:480)/12+25,mean_lambda_ne_x,'Linewidth',2,'Color',[0.7 0 0])
plot((1:480)/12+25,mean_lambda_ne_x_cf,'Linewidth',2,'Color',[0.7 0 0],'LineStyle','--')
xlabel('Age','Interpreter','LaTex','Fontsize',12)
ylabel('Average arrival rate (in \%)','Interpreter','LaTex','Fontsize',12)
legend({'baseline','fix dist. $(a,h_E,h_N)$'},'Interpreter','LaTex','Fontsize',12)
legend boxoff
set(gca,'XGrid','off','YGrid','on','Fontsize',12) 
xlim([1/12+25 480/12+25])
ylim([0 0.20])
saveas(gcf,[plot_path 'lambda_ne_avg.pdf'])
hold off

%% Exercises

% Exercises starting from young 

% load exercises (new)
AWE_o_base = dlmread('AWE_o_base.txt');
AWE_o_base = AWE_o_base';
AWE_o_base = AWE_o_base(1:16);
AWE_o_base = reshape(AWE_o_base,4,4);

AWE_US_o_base=[AWE_o_base(1,:); AWE_o_base(2,:)+AWE_o_base(4,:); AWE_o_base(3,:)];
AWE_US_o_base=[AWE_US_o_base(:,1), AWE_US_o_base(:,2)+AWE_US_o_base(:,4), AWE_US_o_base(:,3)];

AWE_y_base = dlmread('AWE_y_base.txt');
AWE_y_base = AWE_y_base';
AWE_y_base = AWE_y_base(1:16);
AWE_y_base = reshape(AWE_y_base,4,4);

AWE_US_y_base=[AWE_y_base(1,:); AWE_y_base(2,:)+AWE_y_base(4,:); AWE_y_base(3,:)];
AWE_US_y_base=[AWE_US_y_base(:,1), AWE_US_y_base(:,2)+AWE_US_y_base(:,4), AWE_US_y_base(:,3)];

AWE_y_age = dlmread('AWE_y_age.txt');
AWE_y_age = AWE_y_age';
AWE_y_age = AWE_y_age(1:16);
AWE_y_age = reshape(AWE_y_age,4,4);

AWE_US_y_age=[AWE_y_age(1,:); AWE_y_age(2,:)+AWE_y_age(4,:); AWE_y_age(3,:)];
AWE_US_y_age=[AWE_US_y_age(:,1), AWE_US_y_age(:,2)+AWE_US_y_age(:,4), AWE_US_y_age(:,3)];

AWE_y_age_lambda = dlmread('AWE_y_age_lambda.txt');
AWE_y_age_lambda = AWE_y_age_lambda';
AWE_y_age_lambda = AWE_y_age_lambda(1:16);
AWE_y_age_lambda = reshape(AWE_y_age_lambda,4,4);

AWE_US_y_age_lambda=[AWE_y_age_lambda(1,:); AWE_y_age_lambda(2,:)+AWE_y_age_lambda(4,:); AWE_y_age_lambda(3,:)];
AWE_US_y_age_lambda=[AWE_US_y_age_lambda(:,1), AWE_US_y_age_lambda(:,2)+AWE_US_y_age_lambda(:,4), AWE_US_y_age_lambda(:,3)];

AWE_y_all = dlmread('AWE_y_all.txt');
AWE_y_all = AWE_y_all';
AWE_y_all = AWE_y_all(1:16);
AWE_y_all = reshape(AWE_y_all,4,4);

AWE_US_y_all=[AWE_y_all(1,:); AWE_y_all(2,:)+AWE_y_all(4,:); AWE_y_all(3,:)];
AWE_US_y_all=[AWE_US_y_all(:,1), AWE_US_y_all(:,2)+AWE_US_y_all(:,4), AWE_US_y_all(:,3)];

AWE_y_all_lambda = dlmread('AWE_y_all_lambda.txt');
AWE_y_all_lambda = AWE_y_all_lambda';
AWE_y_all_lambda = AWE_y_all_lambda(1:16);
AWE_y_all_lambda = reshape(AWE_y_all_lambda,4,4);

AWE_US_y_all_lambda=[AWE_y_all_lambda(1,:); AWE_y_all_lambda(2,:)+AWE_y_all_lambda(4,:); AWE_y_all_lambda(3,:)];
AWE_US_y_all_lambda=[AWE_US_y_all_lambda(:,1), AWE_US_y_all_lambda(:,2)+AWE_US_y_all_lambda(:,4), AWE_US_y_all_lambda(:,3)];


AWE_y_lambda = dlmread('AWE_y_lambda.txt');
AWE_y_lambda = AWE_y_lambda';
AWE_y_lambda = AWE_y_lambda(1:16);
AWE_y_lambda = reshape(AWE_y_lambda,4,4);

AWE_US_y_lambda=[AWE_y_lambda(1,:); AWE_y_lambda(2,:)+AWE_y_lambda(4,:); AWE_y_lambda(3,:)];
AWE_US_y_lambda=[AWE_US_y_lambda(:,1), AWE_US_y_lambda(:,2)+AWE_US_y_lambda(:,4), AWE_US_y_lambda(:,3)];

AWE_y_hemp = dlmread('AWE_y_hemp.txt');
AWE_y_hemp = AWE_y_hemp';
AWE_y_hemp = AWE_y_hemp(1:16);
AWE_y_hemp = reshape(AWE_y_hemp,4,4);

AWE_US_y_hemp=[AWE_y_hemp(1,:); AWE_y_hemp(2,:)+AWE_y_hemp(4,:); AWE_y_hemp(3,:)];
AWE_US_y_hemp=[AWE_US_y_hemp(:,1), AWE_US_y_hemp(:,2)+AWE_US_y_hemp(:,4), AWE_US_y_hemp(:,3)];

AWE_y_hemp_lambda = dlmread('AWE_y_hemp_lambda.txt');
AWE_y_hemp_lambda = AWE_y_hemp_lambda';
AWE_y_hemp_lambda = AWE_y_hemp_lambda(1:16);
AWE_y_hemp_lambda = reshape(AWE_y_hemp_lambda,4,4);

AWE_US_y_hemp_lambda=[AWE_y_hemp_lambda(1,:); AWE_y_hemp_lambda(2,:)+AWE_y_hemp_lambda(4,:); AWE_y_hemp_lambda(3,:)];
AWE_US_y_hemp_lambda=[AWE_US_y_hemp_lambda(:,1), AWE_US_y_hemp_lambda(:,2)+AWE_US_y_hemp_lambda(:,4), AWE_US_y_hemp_lambda(:,3)];

AWE_y_hout = dlmread('AWE_y_hout.txt');
AWE_y_hout = AWE_y_hout';
AWE_y_hout = AWE_y_hout(1:16);
AWE_y_hout = reshape(AWE_y_hout,4,4);

AWE_US_y_hout=[AWE_y_hout(1,:); AWE_y_hout(2,:)+AWE_y_hout(4,:); AWE_y_hout(3,:)];
AWE_US_y_hout=[AWE_US_y_hout(:,1), AWE_US_y_hout(:,2)+AWE_US_y_hout(:,4), AWE_US_y_hout(:,3)];

AWE_y_hout_lambda = dlmread('AWE_y_hout_lambda.txt');
AWE_y_hout_lambda = AWE_y_hout_lambda';
AWE_y_hout_lambda = AWE_y_hout_lambda(1:16);
AWE_y_hout_lambda = reshape(AWE_y_hout_lambda,4,4);

AWE_US_y_hout_lambda=[AWE_y_hout_lambda(1,:); AWE_y_hout_lambda(2,:)+AWE_y_hout_lambda(4,:); AWE_y_hout_lambda(3,:)];
AWE_US_y_hout_lambda=[AWE_US_y_hout_lambda(:,1), AWE_US_y_hout_lambda(:,2)+AWE_US_y_hout_lambda(:,4), AWE_US_y_hout_lambda(:,3)];


AWE_y_ass = dlmread('AWE_y_ass.txt');
AWE_y_ass = AWE_y_ass';
AWE_y_ass = AWE_y_ass(1:16);
AWE_y_ass = reshape(AWE_y_ass,4,4);

AWE_US_y_ass=[AWE_y_ass(1,:); AWE_y_ass(2,:)+AWE_y_ass(4,:); AWE_y_ass(3,:)];
AWE_US_y_ass=[AWE_US_y_ass(:,1), AWE_US_y_ass(:,2)+AWE_US_y_ass(:,4), AWE_US_y_ass(:,3)];

AWE_y_ass_lambda = dlmread('AWE_y_ass_lambda.txt');
AWE_y_ass_lambda = AWE_y_ass_lambda';
AWE_y_ass_lambda = AWE_y_ass_lambda(1:16);
AWE_y_ass_lambda = reshape(AWE_y_ass_lambda,4,4);

AWE_US_y_ass_lambda=[AWE_y_ass_lambda(1,:); AWE_y_ass_lambda(2,:)+AWE_y_ass_lambda(4,:); AWE_y_ass_lambda(3,:)];
AWE_US_y_ass_lambda=[AWE_US_y_ass_lambda(:,1), AWE_US_y_ass_lambda(:,2)+AWE_US_y_ass_lambda(:,4), AWE_US_y_ass_lambda(:,3)];

AWE_y_sane = dlmread('AWE_y_sanity.txt');
AWE_y_sane = AWE_y_sane';
AWE_y_sane = AWE_y_sane(1:16);
AWE_y_sane = reshape(AWE_y_sane,4,4);

AWE_US_y_sane=[AWE_y_sane(1,:); AWE_y_sane(2,:)+AWE_y_sane(4,:); AWE_y_sane(3,:)];
AWE_US_y_sane=[AWE_US_y_sane(:,1), AWE_US_y_sane(:,2)+AWE_US_y_sane(:,4), AWE_US_y_sane(:,3)];

% Conditional PBs starting from young
AWE_o_base_pb = AWE_o_base./sum(AWE_o_base,1);
AWE_US_o_base_pb = AWE_US_o_base./sum(AWE_US_o_base,1);

AWE_y_base_pb = AWE_y_base./sum(AWE_y_base,1);
AWE_US_y_base_pb = AWE_US_y_base./sum(AWE_US_y_base,1);

AWE_y_age_pb = AWE_y_age./sum(AWE_y_age,1);
AWE_US_y_age_pb = AWE_US_y_age./sum(AWE_US_y_age,1);

AWE_y_age_lambda_pb = AWE_y_age_lambda./sum(AWE_y_age_lambda,1);
AWE_US_y_age_lambda_pb = AWE_US_y_age_lambda./sum(AWE_US_y_age_lambda,1);

AWE_y_all_pb = AWE_y_all./sum(AWE_y_all,1);
AWE_US_y_all_pb = AWE_US_y_all./sum(AWE_US_y_all,1);

AWE_y_all_lambda_pb = AWE_y_all_lambda./sum(AWE_y_all_lambda,1);
AWE_US_y_all_lambda_pb = AWE_US_y_all_lambda./sum(AWE_US_y_all_lambda,1);

AWE_y_lambda_pb = AWE_y_lambda./sum(AWE_y_lambda,1);
AWE_US_y_lambda_pb = AWE_US_y_lambda./sum(AWE_US_y_lambda,1);

AWE_y_hemp_pb = AWE_y_hemp./sum(AWE_y_hemp,1);
AWE_US_y_hemp_pb = AWE_US_y_hemp./sum(AWE_US_y_hemp,1);

AWE_y_hemp_lambda_pb = AWE_y_hemp_lambda./sum(AWE_y_hemp_lambda,1);
AWE_US_y_hemp_lambda_pb = AWE_US_y_hemp_lambda./sum(AWE_US_y_hemp_lambda,1);

AWE_y_hout_pb = AWE_y_hout./sum(AWE_y_hout,1);
AWE_US_y_hout_pb = AWE_US_y_hout./sum(AWE_US_y_hout,1);

AWE_y_hout_lambda_pb = AWE_y_hout_lambda./sum(AWE_y_hout_lambda,1);
AWE_US_y_hout_lambda_pb = AWE_US_y_hout_lambda./sum(AWE_US_y_hout_lambda,1);

AWE_y_ass_pb = AWE_y_ass./sum(AWE_y_ass,1);
AWE_US_y_ass_pb = AWE_US_y_ass./sum(AWE_US_y_ass,1);

AWE_y_ass_lambda_pb = AWE_y_ass_lambda./sum(AWE_y_ass_lambda,1);
AWE_US_y_ass_lambda_pb = AWE_US_y_ass_lambda./sum(AWE_US_y_ass_lambda,1);

AWE_y_sane_pb = AWE_y_sane./sum(AWE_y_sane,1);
AWE_US_y_sane_pb = AWE_US_y_sane./sum(AWE_US_y_sane,1);

AWE_comb_y =[AWE_US_y_base_pb(:,2)-AWE_US_y_base_pb(:,1),...
            AWE_US_y_ass_pb(:,2)-AWE_US_y_ass_pb(:,1),...
            AWE_US_y_ass_lambda_pb(:,2)-AWE_US_y_ass_lambda_pb(:,1),...
            AWE_US_y_hemp_pb(:,2)-AWE_US_y_hemp_pb(:,1),...
            AWE_US_y_hemp_lambda_pb(:,2)-AWE_US_y_hemp_lambda_pb(:,1),...
            AWE_US_y_hout_pb(:,2)-AWE_US_y_hout_pb(:,1),...
            AWE_US_y_hout_lambda_pb(:,2)-AWE_US_y_hout_lambda_pb(:,1),...
            AWE_US_y_all_pb(:,2)-AWE_US_y_all_pb(:,1),...
            AWE_US_y_all_lambda_pb(:,2)-AWE_US_y_all_lambda_pb(:,1),...
            AWE_US_y_age_pb(:,2)-AWE_US_y_age_pb(:,1),...
            AWE_US_y_age_lambda_pb(:,2)-AWE_US_y_age_lambda_pb(:,1),...
            AWE_US_y_sane_pb(:,2)-AWE_US_y_sane_pb(:,1),...
            AWE_US_o_base_pb(:,2)-AWE_US_o_base_pb(:,1)];

% Exercises starting from old

% load exercises (new)

AWE_o_age = dlmread('AWE_o_age.txt');
AWE_o_age = AWE_o_age';
AWE_o_age = AWE_o_age(1:16);
AWE_o_age = reshape(AWE_o_age,4,4);

AWE_US_o_age=[AWE_o_age(1,:); AWE_o_age(2,:)+AWE_o_age(4,:); AWE_o_age(3,:)];
AWE_US_o_age=[AWE_US_o_age(:,1), AWE_US_o_age(:,2)+AWE_US_o_age(:,4), AWE_US_o_age(:,3)];

AWE_o_age_lambda = dlmread('AWE_o_age_lambda.txt');
AWE_o_age_lambda = AWE_o_age_lambda';
AWE_o_age_lambda = AWE_o_age_lambda(1:16);
AWE_o_age_lambda = reshape(AWE_o_age_lambda,4,4);

AWE_US_o_age_lambda=[AWE_o_age_lambda(1,:); AWE_o_age_lambda(2,:)+AWE_o_age_lambda(4,:); AWE_o_age_lambda(3,:)];
AWE_US_o_age_lambda=[AWE_US_o_age_lambda(:,1), AWE_US_o_age_lambda(:,2)+AWE_US_o_age_lambda(:,4), AWE_US_o_age_lambda(:,3)];

AWE_o_all = dlmread('AWE_o_all.txt');
AWE_o_all = AWE_o_all';
AWE_o_all = AWE_o_all(1:16);
AWE_o_all = reshape(AWE_o_all,4,4);

AWE_US_o_all=[AWE_o_all(1,:); AWE_o_all(2,:)+AWE_o_all(4,:); AWE_o_all(3,:)];
AWE_US_o_all=[AWE_US_o_all(:,1), AWE_US_o_all(:,2)+AWE_US_o_all(:,4), AWE_US_o_all(:,3)];

AWE_o_all_lambda = dlmread('AWE_o_all_lambda.txt');
AWE_o_all_lambda = AWE_o_all_lambda';
AWE_o_all_lambda = AWE_o_all_lambda(1:16);
AWE_o_all_lambda = reshape(AWE_o_all_lambda,4,4);

AWE_US_o_all_lambda=[AWE_o_all_lambda(1,:); AWE_o_all_lambda(2,:)+AWE_o_all_lambda(4,:); AWE_o_all_lambda(3,:)];
AWE_US_o_all_lambda=[AWE_US_o_all_lambda(:,1), AWE_US_o_all_lambda(:,2)+AWE_US_o_all_lambda(:,4), AWE_US_o_all_lambda(:,3)];


AWE_o_lambda = dlmread('AWE_o_lambda.txt');
AWE_o_lambda = AWE_o_lambda';
AWE_o_lambda = AWE_o_lambda(1:16);
AWE_o_lambda = reshape(AWE_o_lambda,4,4);

AWE_US_o_lambda=[AWE_o_lambda(1,:); AWE_o_lambda(2,:)+AWE_o_lambda(4,:); AWE_o_lambda(3,:)];
AWE_US_o_lambda=[AWE_US_o_lambda(:,1), AWE_US_o_lambda(:,2)+AWE_US_o_lambda(:,4), AWE_US_o_lambda(:,3)];

AWE_o_hemp = dlmread('AWE_o_hemp.txt');
AWE_o_hemp = AWE_o_hemp';
AWE_o_hemp = AWE_o_hemp(1:16);
AWE_o_hemp = reshape(AWE_o_hemp,4,4);

AWE_US_o_hemp=[AWE_o_hemp(1,:); AWE_o_hemp(2,:)+AWE_o_hemp(4,:); AWE_o_hemp(3,:)];
AWE_US_o_hemp=[AWE_US_o_hemp(:,1), AWE_US_o_hemp(:,2)+AWE_US_o_hemp(:,4), AWE_US_o_hemp(:,3)];

AWE_o_hemp_lambda = dlmread('AWE_o_hemp_lambda.txt');
AWE_o_hemp_lambda = AWE_o_hemp_lambda';
AWE_o_hemp_lambda = AWE_o_hemp_lambda(1:16);
AWE_o_hemp_lambda = reshape(AWE_o_hemp_lambda,4,4);

AWE_US_o_hemp_lambda=[AWE_o_hemp_lambda(1,:); AWE_o_hemp_lambda(2,:)+AWE_o_hemp_lambda(4,:); AWE_o_hemp_lambda(3,:)];
AWE_US_o_hemp_lambda=[AWE_US_o_hemp_lambda(:,1), AWE_US_o_hemp_lambda(:,2)+AWE_US_o_hemp_lambda(:,4), AWE_US_o_hemp_lambda(:,3)];

AWE_o_hout = dlmread('AWE_o_hout.txt');
AWE_o_hout = AWE_o_hout';
AWE_o_hout = AWE_o_hout(1:16);
AWE_o_hout = reshape(AWE_o_hout,4,4);

AWE_US_o_hout=[AWE_o_hout(1,:); AWE_o_hout(2,:)+AWE_o_hout(4,:); AWE_o_hout(3,:)];
AWE_US_o_hout=[AWE_US_o_hout(:,1), AWE_US_o_hout(:,2)+AWE_US_o_hout(:,4), AWE_US_o_hout(:,3)];

AWE_o_hout_lambda = dlmread('AWE_o_hout_lambda.txt');
AWE_o_hout_lambda = AWE_o_hout_lambda';
AWE_o_hout_lambda = AWE_o_hout_lambda(1:16);
AWE_o_hout_lambda = reshape(AWE_o_hout_lambda,4,4);

AWE_US_o_hout_lambda=[AWE_o_hout_lambda(1,:); AWE_o_hout_lambda(2,:)+AWE_o_hout_lambda(4,:); AWE_o_hout_lambda(3,:)];
AWE_US_o_hout_lambda=[AWE_US_o_hout_lambda(:,1), AWE_US_o_hout_lambda(:,2)+AWE_US_o_hout_lambda(:,4), AWE_US_o_hout_lambda(:,3)];


AWE_o_ass = dlmread('AWE_o_ass.txt');
AWE_o_ass = AWE_o_ass';
AWE_o_ass = AWE_o_ass(1:16);
AWE_o_ass = reshape(AWE_o_ass,4,4);

AWE_US_o_ass=[AWE_o_ass(1,:); AWE_o_ass(2,:)+AWE_o_ass(4,:); AWE_o_ass(3,:)];
AWE_US_o_ass=[AWE_US_o_ass(:,1), AWE_US_o_ass(:,2)+AWE_US_o_ass(:,4), AWE_US_o_ass(:,3)];

AWE_o_ass_lambda = dlmread('AWE_o_ass_lambda.txt');
AWE_o_ass_lambda = AWE_o_ass_lambda';
AWE_o_ass_lambda = AWE_o_ass_lambda(1:16);
AWE_o_ass_lambda = reshape(AWE_o_ass_lambda,4,4);

AWE_US_o_ass_lambda=[AWE_o_ass_lambda(1,:); AWE_o_ass_lambda(2,:)+AWE_o_ass_lambda(4,:); AWE_o_ass_lambda(3,:)];
AWE_US_o_ass_lambda=[AWE_US_o_ass_lambda(:,1), AWE_US_o_ass_lambda(:,2)+AWE_US_o_ass_lambda(:,4), AWE_US_o_ass_lambda(:,3)];

AWE_o_sane = dlmread('AWE_o_sanity.txt');
AWE_o_sane = AWE_o_sane';
AWE_o_sane = AWE_o_sane(1:16);
AWE_o_sane = reshape(AWE_o_sane,4,4);

AWE_US_o_sane=[AWE_o_sane(1,:); AWE_o_sane(2,:)+AWE_o_sane(4,:); AWE_o_sane(3,:)];
AWE_US_o_sane=[AWE_US_o_sane(:,1), AWE_US_o_sane(:,2)+AWE_US_o_sane(:,4), AWE_US_o_sane(:,3)];

% Conditional PBs starting from old

AWE_o_age_pb = AWE_o_age./sum(AWE_o_age,1);
AWE_US_o_age_pb = AWE_US_o_age./sum(AWE_US_o_age,1);

AWE_o_age_lambda_pb = AWE_o_age_lambda./sum(AWE_o_age_lambda,1);
AWE_US_o_age_lambda_pb = AWE_US_o_age_lambda./sum(AWE_US_o_age_lambda,1);

AWE_o_all_pb = AWE_o_all./sum(AWE_o_all,1);
AWE_US_o_all_pb = AWE_US_o_all./sum(AWE_US_o_all,1);

AWE_o_all_lambda_pb = AWE_o_all_lambda./sum(AWE_o_all_lambda,1);
AWE_US_o_all_lambda_pb = AWE_US_o_all_lambda./sum(AWE_US_o_all_lambda,1);

AWE_o_sane_pb = AWE_o_sane./sum(AWE_o_sane,1);
AWE_US_o_sane_pb = AWE_US_o_sane./sum(AWE_US_o_sane,1);

AWE_o_lambda_pb = AWE_o_lambda./sum(AWE_o_lambda,1);
AWE_US_o_lambda_pb = AWE_US_o_lambda./sum(AWE_US_o_lambda,1);

AWE_o_hemp_pb = AWE_o_hemp./sum(AWE_o_hemp,1);
AWE_US_o_hemp_pb = AWE_US_o_hemp./sum(AWE_US_o_hemp,1);

AWE_o_hemp_lambda_pb = AWE_o_hemp_lambda./sum(AWE_o_hemp_lambda,1);
AWE_US_o_hemp_lambda_pb = AWE_US_o_hemp_lambda./sum(AWE_US_o_hemp_lambda,1);

AWE_o_hout_pb = AWE_o_hout./sum(AWE_o_hout,1);
AWE_US_o_hout_pb = AWE_US_o_hout./sum(AWE_US_o_hout,1);

AWE_o_hout_lambda_pb = AWE_o_hout_lambda./sum(AWE_o_hout_lambda,1);
AWE_US_o_hout_lambda_pb = AWE_US_o_hout_lambda./sum(AWE_US_o_hout_lambda,1);

AWE_o_ass_pb = AWE_o_ass./sum(AWE_o_ass,1);
AWE_US_o_ass_pb = AWE_US_o_ass./sum(AWE_US_o_ass,1);

AWE_o_ass_lambda_pb = AWE_o_ass_lambda./sum(AWE_o_ass_lambda,1);
AWE_US_o_ass_lambda_pb = AWE_US_o_ass_lambda./sum(AWE_US_o_ass_lambda,1);

AWE_comb_o =[AWE_US_o_base_pb(:,2)-AWE_US_o_base_pb(:,1),...
            AWE_US_o_ass_pb(:,2)-AWE_US_o_ass_pb(:,1),...
            AWE_US_o_ass_lambda_pb(:,2)-AWE_US_o_ass_lambda_pb(:,1),...
            AWE_US_o_hemp_pb(:,2)-AWE_US_o_hemp_pb(:,1),...
            AWE_US_o_hemp_lambda_pb(:,2)-AWE_US_o_hemp_lambda_pb(:,1),...
            AWE_US_o_hout_pb(:,2)-AWE_US_o_hout_pb(:,1),...
            AWE_US_o_hout_lambda_pb(:,2)-AWE_US_o_hout_lambda_pb(:,1),...
            AWE_US_o_all_pb(:,2)-AWE_US_o_all_pb(:,1),...
            AWE_US_o_all_lambda_pb(:,2)-AWE_US_o_all_lambda_pb(:,1),...
            AWE_US_o_age_pb(:,2)-AWE_US_o_age_pb(:,1),...
            AWE_US_o_age_lambda_pb(:,2)-AWE_US_o_age_lambda_pb(:,1),...
            AWE_US_o_sane_pb(:,2)-AWE_US_o_sane_pb(:,1),...
            AWE_US_y_base_pb(:,2)-AWE_US_y_base_pb(:,1)];
       

%% separation rates - by income and split into EU vs EN
    
trans_byHC_h = filltrans_by(sim_LS_h,sim_hh);
trans_byHC_sp = filltrans_by(sim_LS_sp,sim_hsp);
trans_byHC = filltrans_by([sim_LS_h;sim_LS_sp],[sim_hh;sim_hsp]);

sep_exo = 0.05 *(1:12).^(-0.65);
sep_data = [1.70 1.03 0.76 0.60 0.56 0.49 0.5 0.46 0.43 0.48 0.41 0.48]*2.7/0.82;
sep_data_EU = [0.72	0.53	0.39	0.32	0.26	0.24	0.25	0.23	0.20	0.26	0.21	0.24]*2.7/0.82;
sep_data_EN = [0.98	0.50	0.37	0.28	0.30	0.25	0.25	0.23	0.23	0.22	0.20	0.24]*2.7/0.82;
inc=0.7*(0.1:0.1:1.2);

sep_h = squeeze(1-trans_byHC_h(1,1,:));
sep_sp = squeeze(1-trans_byHC_sp(1,1,:));
sep_all =squeeze(1-trans_byHC(1,1,:))*100;

sep_h_U = squeeze(trans_byHC_h(1,2,:)+trans_byHC_h(1,4,:));
sep_sp_U = squeeze(trans_byHC_sp(1,2,:)+trans_byHC_sp(1,4,:));
sep_all_U = squeeze(trans_byHC(1,2,:)+trans_byHC(1,4,:))*100;

sep_h_N = squeeze(trans_byHC_h(1,3,:));
sep_sp_N = squeeze(trans_byHC_sp(1,3,:));
sep_all_N = squeeze(trans_byHC(1,3,:))*100;

% totals only
fig = figure;
hold on 
plot(inc,sep_all,'-', 'Color','[0.35 0.35 0.35]','LineWidth',3.25)
plot(inc,sep_data,'--','Color','[0.70 0.0 0.0]','LineWidth',3.25)
xlabel('Income (in \$10k)','Interpreter','LaTex','Fontsize',12)
ylabel('Separation Rate (in\%)','Interpreter','LaTex','Fontsize',12)
xticks(0.7*(0.1:0.1:1.2))
yticks(0.5:0.5:7)
xlim([0 0.7*1.3])
ylim([0 7.1])
leg = legend('model','data','Interpreter','LaTex');
set(leg,'Interpreter','LaTex','Fontsize',12,'Location','NorthEast','NumColumns',1)
legend boxoff
set(gca,'XGrid','off','YGrid','off','Fontsize',12)
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'separation_fit.pdf'])
hold off

% decomposition
fig = figure;
hold on 
plot(inc,sep_all,'-',inc,sep_all_U,'--',inc,sep_all_N,':', 'Color','[0.35 0.35 0.35]','LineWidth',3.25)
plot(inc,sep_data,'-',inc,sep_data_EU,'--',inc,sep_data_EN,':','Color','[0.70 0.0 0.0]','LineWidth',3.25)
xlabel('Income (in \$10k)','Interpreter','LaTex','Fontsize',12)
ylabel('Separation Rate (in\%)','Interpreter','LaTex','Fontsize',12)
xticks(0.7*(0.1:0.1:1.2))
yticks(0.5:0.5:7)
xlim([0 0.7*1.3])
ylim([0 7.1])
leg = legend('model (to U or N)','model (to U only)','model (to N only)','data (to U or N)','data (to U only)','data (to N only)','Interpreter','LaTex');
set(leg,'Interpreter','LaTex','Fontsize',12,'Location','NorthEast','NumColumns',1)
legend boxoff
set(gca,'XGrid','off','YGrid','off','Fontsize',12)
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'separation_fit_decomp.pdf'])
hold off

% decomposition relative
fig = figure;
hold on 
plot(inc,sep_all/sep_all(1),'-',inc,sep_all_U/sep_all(1),'--',inc,sep_all_N/sep_all(1),':', 'Color','[0.35 0.35 0.35]','LineWidth',3.25)
plot(inc,sep_data/sep_data(1),'-',inc,sep_data_EU/sep_data(1),'--',inc,sep_data_EN/sep_data(1),':','Color','[0.70 0.0 0.0]','LineWidth',3.25)
xlabel('Income (in \$10k)','Interpreter','LaTex','Fontsize',12)
ylabel('Separation Rate (relative)','Interpreter','LaTex','Fontsize',12)
xticks(0.7*(0.1:0.1:1.2))
yticks(0.1:0.1:1)
xlim([0 0.7*1.3])
ylim([0 1.1])
leg = legend('model (to U or N)','model (to U only)','model (to N only)','data (to U or N)','data (to U only)','data (to N only)','Interpreter','LaTex');
set(leg,'Interpreter','LaTex','Fontsize',12,'Location','NorthEast','NumColumns',1)
legend boxoff
set(gca,'XGrid','off','YGrid','off','Fontsize',12)
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'separation_fit_rel.pdf'])
hold off


%% HC differences by age (and work tenure)

% HC difference/ratio by age group, also compute duration in and out by age group
dur_in_h = cumsum(sim_LS_h==1,2);
dur_in_sp = cumsum(sim_LS_sp==1,2);
dur_out_h = (1:480)-dur_in_h;
dur_out_sp = (1:480)-dur_in_sp;

store_EN = NaN(NN*TT/2,8);
row=0;
for n=1:NN
    for t=1:TT
        if sim_LS(n,t)== 4
            row=row+1;
            store_EN(row,:) = [n, t, sim_hh(n,t), sim_hsp(n,t), dur_in_h(n,t), dur_in_sp(n,t), dur_out_h(n,t), dur_out_sp(n,t)];
        end
        if sim_LS(n,t)== 5
            row=row+1;
            store_EN(row,:) = [n, t, sim_hsp(n,t), sim_hh(n,t), dur_in_sp(n,t), dur_in_h(n,t), dur_out_sp(n,t), dur_out_h(n,t)];
        end
    end
end

store_EN = store_EN(1:row,:);

store_EN(:,9) = hgrid(store_EN(:,3))-hgrid(store_EN(:,4));
store_EN(:,10) = hgrid(store_EN(:,3))./hgrid(store_EN(:,4));

age_min=[1 121 241 361];
age_max=[120 240 360 480];
store_EN_age = NaN(4,8);

for i=1:4
    ind_age = (store_EN(:,2)>=age_min(i)).*(store_EN(:,2)<=age_max(i));
    help = sum(store_EN.*ind_age) / sum(ind_age);
    store_EN_age(i,:) = help(3:end);
end

%% AWE for all age groups

[~,trans_count]=filltrans(sim_LS);
[AWE_all,trans_cond_all] = getAWE(trans_count);

[~,trans_count1]=filltrans(sim_LS(:,1:120));
[AWE_age(:,1),trans_cond_age(:,:,1)] = getAWE(trans_count1);

[~,trans_count2]=filltrans(sim_LS(:,121:240));
[AWE_age(:,2),trans_cond_age(:,:,2)] = getAWE(trans_count2);

[~,trans_count3]=filltrans(sim_LS(:,241:360));
[AWE_age(:,3),trans_cond_age(:,:,3)] = getAWE(trans_count3);

[~,trans_count4]=filltrans(sim_LS(:,361:480));
[AWE_age(:,4),trans_cond_age(:,:,4)] = getAWE(trans_count4);


%% distributions of assets and human capital across benefit counterfactuals

% load results with phiUS=1/12
cd(path_cf1)

sim_asset_phiUS_low = dlmread('sim_asset.txt');
sim_asset_phiUS_low = sim_asset_phiUS_low';
sim_asset_phiUS_low = sim_asset_phiUS_low(1:NN*TT);
sim_asset_phiUS_low = reshape(sim_asset_phiUS_low,NN,TT);

sim_LS_phiUS_low = dlmread('sim_LS.txt');
sim_LS_phiUS_low = sim_LS_phiUS_low';
sim_LS_phiUS_low = sim_LS_phiUS_low(1:NN*TT);
sim_LS_phiUS_low = reshape(sim_LS_phiUS_low,NN,TT);

sim_hh_phiUS_low = dlmread('sim_hh.txt');
sim_hh_phiUS_low = sim_hh_phiUS_low';
sim_hh_phiUS_low = sim_hh_phiUS_low(1:NN*TT);
sim_hh_phiUS_low = reshape(sim_hh_phiUS_low,NN,TT);

sim_hsp_phiUS_low = dlmread('sim_hsp.txt');
sim_hsp_phiUS_low = sim_hsp_phiUS_low';
sim_hsp_phiUS_low = sim_hsp_phiUS_low(1:NN*TT);
sim_hsp_phiUS_low = reshape(sim_hsp_phiUS_low,NN,TT);

% load results with phiUS=1
cd(path_cf2)

sim_asset_phiUS_high = dlmread('sim_asset.txt');
sim_asset_phiUS_high = sim_asset_phiUS_high';
sim_asset_phiUS_high = sim_asset_phiUS_high(1:NN*TT);
sim_asset_phiUS_high = reshape(sim_asset_phiUS_high,NN,TT);

sim_LS_phiUS_high = dlmread('sim_LS.txt');
sim_LS_phiUS_high = sim_LS_phiUS_high';
sim_LS_phiUS_high = sim_LS_phiUS_high(1:NN*TT);
sim_LS_phiUS_high = reshape(sim_LS_phiUS_high,NN,TT);

sim_hh_phiUS_high = dlmread('sim_hh.txt');
sim_hh_phiUS_high = sim_hh_phiUS_high';
sim_hh_phiUS_high = sim_hh_phiUS_high(1:NN*TT);
sim_hh_phiUS_high = reshape(sim_hh_phiUS_high,NN,TT);

sim_hsp_phiUS_high = dlmread('sim_hsp.txt');
sim_hsp_phiUS_high = sim_hsp_phiUS_high';
sim_hsp_phiUS_high = sim_hsp_phiUS_high(1:NN*TT);
sim_hsp_phiUS_high = reshape(sim_hsp_phiUS_high,NN,TT);


sim_HC=[sim_hh;sim_hsp];
sim_HC_phiUS_low=[sim_hh_phiUS_low;sim_hsp_phiUS_low];
sim_HC_phiUS_high=[sim_hh_phiUS_high;sim_hsp_phiUS_high];

for i=1:HH
    dist_HC(i) = sum(sim_HC(:)==i)/(NN*TT*2);
    dist_HC_phiUS_low(i) = sum(sim_HC_phiUS_low(:)==i)/(NN*TT*2);
    dist_HC_phiUS_high(i) = sum(sim_HC_phiUS_high(:)==i)/(NN*TT*2);
end

for i=1:AA
    dist_ass(i) = sum(sim_asset(:)==i)/(NN*TT);
    dist_ass_phiUS_low(i) = sum(sim_asset_phiUS_low(:)==i)/(NN*TT);
    dist_ass_phiUS_high(i) = sum(sim_asset_phiUS_high(:)==i)/(NN*TT);
end


% AWE low phiUS
[~,trans_count_phiUS_low]=filltrans(sim_LS_phiUS_low);
AWE_all_phiUS_low = getAWE(trans_count_phiUS_low);

[~,trans_count_phiUS_low1]=filltrans(sim_LS_phiUS_low(:,1:120));
AWE_age_phiUS_low(:,1) = getAWE(trans_count_phiUS_low1);

[~,trans_count_phiUS_low2]=filltrans(sim_LS_phiUS_low(:,121:240));
AWE_age_phiUS_low(:,2) = getAWE(trans_count_phiUS_low2);

[~,trans_count_phiUS_low3]=filltrans(sim_LS_phiUS_low(:,241:360));
AWE_age_phiUS_low(:,3) = getAWE(trans_count_phiUS_low3);

[~,trans_count_phiUS_low4]=filltrans(sim_LS_phiUS_low(:,361:480));
AWE_age_phiUS_low(:,4) = getAWE(trans_count_phiUS_low4);

% AWE high phiUS
[~,trans_count_phiUS_high]=filltrans(sim_LS_phiUS_high);
AWE_all_phiUS_high = getAWE(trans_count_phiUS_high);

[~,trans_count_phiUS_high1]=filltrans(sim_LS_phiUS_high(:,1:120));
AWE_age_phiUS_high(:,1) = getAWE(trans_count_phiUS_high1);

[~,trans_count_phiUS_high2]=filltrans(sim_LS_phiUS_high(:,121:240));
AWE_age_phiUS_high(:,2) = getAWE(trans_count_phiUS_high2);

[~,trans_count_phiUS_high3]=filltrans(sim_LS_phiUS_high(:,241:360));
AWE_age_phiUS_high(:,3) = getAWE(trans_count_phiUS_high3);

[~,trans_count_phiUS_high4]=filltrans(sim_LS_phiUS_high(:,361:480));
AWE_age_phiUS_high(:,4) = getAWE(trans_count_phiUS_high4);



fig = figure;
hold on 
plot(1:HH,dist_HC,'-',1:HH,dist_HC_phiUS_low,'--',1:HH,dist_HC_phiUS_high,':','Color','[0.35 0.35 0.35]','LineWidth',3.25)
xlabel('Human Capital (h)','Interpreter','LaTex','Fontsize',12)
ylabel('PDF(h)','Interpreter','LaTex','Fontsize',12)
xticks(1:12)
yticks(0.05:0.05:0.25)
xlim([0 13])
ylim([0 0.3])
leg = legend('$\phi^{US}=1/6$','$\phi^{US}=1/12$','$\phi^{US}=1$','Interpreter','LaTex');
set(leg,'Interpreter','LaTex','Fontsize',12,'Location','NorthEast','NumColumns',1)
legend boxoff
set(gca,'XGrid','off','YGrid','off','Fontsize',12)
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'policy_dist_HC.pdf'])
hold off

fig = figure;
hold on 
plot(assgrid,dist_ass,'-',assgrid,dist_ass_phiUS_low,'--',assgrid,dist_ass_phiUS_high,':','Color','[0.35 0.35 0.35]','LineWidth',3.25)
xlabel('Assets (a)','Interpreter','LaTex','Fontsize',12)
ylabel('PDF(a)','Interpreter','LaTex','Fontsize',12)
xticks(5:5:100)
yticks(0.05:0.05:0.25)
xlim([0 51])
ylim([0 0.3])
leg = legend('$\phi^{US}=1/6$','$\phi^{US}=1/12$','$\phi^{US}=1$','Interpreter','LaTex');
set(leg,'Interpreter','LaTex','Fontsize',12,'Location','NorthEast','NumColumns',1)
legend boxoff
set(gca,'XGrid','off','YGrid','off','Fontsize',12)
set(gca,'TickLabelInterpreter','LaTex')
saveas(gcf,[plot_path 'policy_dist_ass.pdf'])
hold off


%% functions
function [trans,trans_count]=filltrans(sim_LS)

    N = size(sim_LS,1);
    T = size(sim_LS,2);

    LS_max = max(sim_LS(:));

    trans_count = zeros(LS_max,LS_max);

    for n=1:N
        for t=2:T
            trans_count(sim_LS(n,t-1),sim_LS(n,t)) = trans_count(sim_LS(n,t-1),sim_LS(n,t))+1;
        end
    end

    for i = 1:LS_max
        trans(i,:)=trans_count(i,:)/sum(trans_count(i,:));
    end

end

function [trans,trans_count]=filltrans_by(sim_LS,by)

    N = size(sim_LS,1);
    T = size(sim_LS,2);

    LS_max = max(sim_LS(:));
    by_max = max(by(:));

    trans_count = zeros(LS_max,LS_max,by_max);

    for n=1:N
        for t=2:T
            trans_count(sim_LS(n,t-1),sim_LS(n,t),by(n,t-1)) = trans_count(sim_LS(n,t-1),sim_LS(n,t),by(n,t-1))+1;
        end
    end

    for i = 1:LS_max
        for j=1:by_max
            trans(i,:,j)=trans_count(i,:,j)/sum(trans_count(i,:,j));
        end
    end

end

function [AWE,trans_cond] = getAWE(trans_count)

    % counts
    trans_cond_N(1,1) = trans_count(4,1) + trans_count(5,1);
    trans_cond_N(2,1) = trans_count(5,3)+trans_count(5,15)+trans_count(4,2)+trans_count(4,16);
    trans_cond_N(3,1) = trans_count(5,5)+trans_count(4,4);
    
    trans_cond_N(1,2) = trans_count(4,3)+trans_count(5,2)+trans_count(4,15)+trans_count(5,16);
    trans_cond_N(2,2) = trans_count(4,6)+trans_count(4,14)+trans_count(4,10)+trans_count(4,13) + ...
                        trans_count(5,6)+trans_count(5,14)+trans_count(5,10)+trans_count(5,13);
    trans_cond_N(3,2) = trans_count(4,7)+trans_count(5,8)+trans_count(4,11)+trans_count(5,12);
    
    
    % probabilities
    trans_cond(:,1) = trans_cond_N(:,1)/ sum(trans_cond_N(:,1));
    trans_cond(:,2) = trans_cond_N(:,2)/ sum(trans_cond_N(:,2));
    
    AWE = trans_cond(:,2) - trans_cond(:,1);

end
