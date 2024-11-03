%% Model Counterpart to Data Leads and Lags for AWE

% In Stata, you are in regression sample if 
% 1) head has EE, EU transition (note: no EN) and
% 2) spouse has NE, NU, NN transition

clear; clc; close all;

%% Load data

% Dimensions
dim = dlmread('dimensions.txt');
AA = dim(1);    % number of asset grid points
TT = dim(2);    % number of working age time periods
HH = dim(3);    % number of human capital grid points
NN = dim(4);    % number of HH in simulation

% Labor market states simulation
% 1 - E, 2 - U, 3 - N, 4 - S
sim_LS_h = dlmread('sim_LS_h.txt');
sim_LS_h = sim_LS_h';
sim_LS_h = sim_LS_h(1:NN*TT);
sim_LS_h = reshape(sim_LS_h,NN,TT);

sim_LS_sp = dlmread('sim_LS_sp.txt');
sim_LS_sp = sim_LS_sp';
sim_LS_sp = sim_LS_sp(1:NN*TT);
sim_LS_sp = reshape(sim_LS_sp,NN,TT);

% Age restrictions
group = 1;
if group == 1
    sim_LS_h = sim_LS_h(:,1:120);
    sim_LS_sp = sim_LS_sp(:,1:120);
    TT = 120;
elseif group == 2
    sim_LS_h = sim_LS_h(:,121:240);
    sim_LS_sp = sim_LS_sp(:,121:240);
    TT = 120;
elseif group == 3
    sim_LS_h = sim_LS_h(:,241:360);
    sim_LS_sp = sim_LS_sp(:,241:360);
    TT = 120;
elseif group == 4
    sim_LS_h = sim_LS_h(:,360:480);
    sim_LS_sp = sim_LS_sp(:,360:480);
    TT = 121;
end

% Head EU or EU/ES transitions
EU_ES = 2; % 1: just EU; 2: both

%% Prepare model output

% Head EU transitions
head_eu = NaN(NN*2,TT-1);
for i = 1:NN
    for t = 2:TT
        if EU_ES == 1
            % treat head as head
            if sim_LS_h(i,t-1) == 1 && sim_LS_h(i,t) == 2
                head_eu(i,t-1) = 1;
            elseif sim_LS_h(i,t-1) == 1 && sim_LS_h(i,t) == 1
                head_eu(i,t-1) = 0;
            end
            % treat spouse as head
            if sim_LS_sp(i,t-1) == 1 && sim_LS_sp(i,t) == 2
                head_eu(NN+i,t-1) = 1;
            elseif sim_LS_sp(i,t-1) == 1 && sim_LS_sp(i,t) == 1
                head_eu(NN+i,t-1) = 0;
            end
        elseif EU_ES == 2
            % treat head as head
            if sim_LS_h(i,t-1) == 1 && (sim_LS_h(i,t) == 2 || sim_LS_h(i,t) == 4)
                head_eu(i,t-1) = 1;
            elseif sim_LS_h(i,t-1) == 1 && sim_LS_h(i,t) == 1
                head_eu(i,t-1) = 0;
            end
            % treat spouse as head
            if sim_LS_sp(i,t-1) == 1 && (sim_LS_sp(i,t) == 2 || sim_LS_sp(i,t) == 4)
                head_eu(NN+i,t-1) = 1;
            elseif sim_LS_sp(i,t-1) == 1 && sim_LS_sp(i,t) == 1
                head_eu(NN+i,t-1) = 0;
            end
        end
    end
end

% Spouse NL transitions
spouse_nl = NaN(NN*2,TT-1);
for i = 1:NN
    for t = 2:TT
        % treat spouse as spouse
        if sim_LS_sp(i,t-1) == 3 && (sim_LS_sp(i,t) == 1 || sim_LS_sp(i,t) == 2 || sim_LS_sp(i,t) == 4)
            spouse_nl(i,t-1) = 1;
        elseif sim_LS_sp(i,t-1) == 3 && sim_LS_sp(i,t) == 3
            spouse_nl(i,t-1) = 0;
        end
        % treat head as spouse
        if sim_LS_h(i,t-1) == 3 && (sim_LS_h(i,t) == 1 || sim_LS_h(i,t) == 2 || sim_LS_h(i,t) == 4)
            spouse_nl(NN+i,t-1) = 1;
        elseif sim_LS_h(i,t-1) == 3 && sim_LS_h(i,t) == 3
            spouse_nl(NN+i,t-1) = 0;
        end
    end
end

% Leads and lags
head_eu_lead2 = head_eu(:,3:TT-1);
spouse_nl_lead2 = spouse_nl(:,1:TT-3);

head_eu_lead1 = head_eu(:,2:TT-1);
spouse_nl_lead1 = spouse_nl(:,1:TT-2);

head_eu_lag1 = head_eu(:,1:TT-2);
spouse_nl_lag1 = spouse_nl(:,2:TT-1);

head_eu_lag2 = head_eu(:,1:TT-3);
spouse_nl_lag2 = spouse_nl(:,3:TT-1);
        
%% Regressions

% Contemporaneous
head_eu_vec = reshape(head_eu,2*NN*(TT-1),1);
aux = size(head_eu_vec);
aux = aux(1);
intercept = ones(aux,1);
head_eu_vec = [intercept,head_eu_vec];
spouse_nl_vec = reshape(spouse_nl,2*NN*(TT-1),1);
coef_t = regress(spouse_nl_vec,head_eu_vec);

spouse_nl_vec_cont = spouse_nl_vec;
head_eu_vec_cont = head_eu_vec(:,2);

% Two periods ahead
head_eu_vec = reshape(head_eu_lead2,2*NN*(TT-3),1);
aux = size(head_eu_vec);
aux = aux(1);
intercept = ones(aux,1);
head_eu_vec = [intercept,head_eu_vec];
spouse_nl_vec = reshape(spouse_nl_lead2,2*NN*(TT-3),1);
coef_lead2 = regress(spouse_nl_vec,head_eu_vec);

spouse_nl_vec_lead2 = spouse_nl_vec;
head_eu_vec_lead2 = head_eu_vec(:,2);

% One period ahead
head_eu_vec = reshape(head_eu_lead1,2*NN*(TT-2),1);
aux = size(head_eu_vec);
aux = aux(1);
intercept = ones(aux,1);
head_eu_vec = [intercept,head_eu_vec];
spouse_nl_vec = reshape(spouse_nl_lead1,2*NN*(TT-2),1);
coef_lead1 = regress(spouse_nl_vec,head_eu_vec);

spouse_nl_vec_lead1 = spouse_nl_vec;
head_eu_vec_lead1 = head_eu_vec(:,2);

% One period after
head_eu_vec = reshape(head_eu_lag1,2*NN*(TT-2),1);
aux = size(head_eu_vec);
aux = aux(1);
intercept = ones(aux,1);
head_eu_vec = [intercept,head_eu_vec];
spouse_nl_vec = reshape(spouse_nl_lag1,2*NN*(TT-2),1);
coef_lag1 = regress(spouse_nl_vec,head_eu_vec);

spouse_nl_vec_lag1 = spouse_nl_vec;
head_eu_vec_lag1 = head_eu_vec(:,2);

% Two periods after
head_eu_vec = reshape(head_eu_lag2,2*NN*(TT-3),1);
aux = size(head_eu_vec);
aux = aux(1);
intercept = ones(aux,1);
head_eu_vec = [intercept,head_eu_vec];
spouse_nl_vec = reshape(spouse_nl_lag2,2*NN*(TT-3),1);
coef_lag2 = regress(spouse_nl_vec,head_eu_vec);

spouse_nl_vec_lag2 = spouse_nl_vec;
head_eu_vec_lag2 = head_eu_vec(:,2);

%% Prepare to load into Stata

clearvars -except spouse_nl_vec_cont head_eu_vec_cont ...
                  spouse_nl_vec_lead2 head_eu_vec_lead2 ...
                  spouse_nl_vec_lead1 head_eu_vec_lead1 ...
                  spouse_nl_vec_lag1 head_eu_vec_lag1 ...
                  spouse_nl_vec_lag2 head_eu_vec_lag2 group

if group == 1

    csvwrite('head_eu_vec_cont_1.csv',head_eu_vec_cont);
    csvwrite('spouse_nl_vec_cont_1.csv',spouse_nl_vec_cont);
    
    csvwrite('spouse_nl_vec_lead2_1.csv',spouse_nl_vec_lead2);
    csvwrite('head_eu_vec_lead2_1.csv',head_eu_vec_lead2);
    
    csvwrite('spouse_nl_vec_lead1_1.csv',spouse_nl_vec_lead1);
    csvwrite('head_eu_vec_lead1_1.csv',head_eu_vec_lead1);
    
    csvwrite('spouse_nl_vec_lag1_1.csv',spouse_nl_vec_lag1);
    csvwrite('head_eu_vec_lag1_1.csv',head_eu_vec_lag1);
    
    csvwrite('spouse_nl_vec_lag2_1.csv',spouse_nl_vec_lag2);
    csvwrite('head_eu_vec_lag2_1.csv',head_eu_vec_lag2);

elseif group == 4

    csvwrite('head_eu_vec_cont_4.csv',head_eu_vec_cont);
    csvwrite('spouse_nl_vec_cont_4.csv',spouse_nl_vec_cont);
    
    csvwrite('spouse_nl_vec_lead2_4.csv',spouse_nl_vec_lead2);
    csvwrite('head_eu_vec_lead2_4.csv',head_eu_vec_lead2);
    
    csvwrite('spouse_nl_vec_lead1_4.csv',spouse_nl_vec_lead1);
    csvwrite('head_eu_vec_lead1_4.csv',head_eu_vec_lead1);
    
    csvwrite('spouse_nl_vec_lag1_4.csv',spouse_nl_vec_lag1);
    csvwrite('head_eu_vec_lag1_4.csv',head_eu_vec_lag1);
    
    csvwrite('spouse_nl_vec_lag2_4.csv',spouse_nl_vec_lag2);
    csvwrite('head_eu_vec_lag2_4.csv',head_eu_vec_lag2);

end




