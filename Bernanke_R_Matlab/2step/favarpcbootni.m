% estimates favar by principal components
% confidence intervals are from a two-step bootstrap
% X is TxN, Y is TxM , F is TxK
% state equation:       [F(t)' Y(t)']'=B(L)[F(t-1)' Y(t-1)']'+v(t), v ~ N(0,Q), Q unrestricted
% X is Stock&Watson balanced panel less Y, observable factors
% Piotr Eliasz, 9/11/2002
profile on -detail builtin

for num=1:5
    

load('nsbalpanel.txt','-ascii')
% need to standardize for PC:
T=size(nsbalpanel,1);
stdffr=std(nsbalpanel(:,77))
nsbalpanel=(nsbalpanel-repmat(mean(nsbalpanel,1),T,1))./repmat(std(nsbalpanel,1),T,1);

%%%%%%%%%%%%%%%%%%%%%%%%% choose specification %%%%%%%%%%%%%%%%%%%%%%%%
% Y=nsbalpanel(:,[16 107 77]); % this is IP, CPI and FFR
Y=nsbalpanel(:,77); % this is FFR
X=[nsbalpanel(:,1:76),nsbalpanel(:,78:120)];

% choose X from balanced panel (remember -- with Y excluded, so -1 after 77)
xindex=[16;107;77;80;95;92;74;101;17;49;50;51;26;48;117;54;62;71;119];


%varnames={'IP','CPI','3m TREASURY BILLS','5y TREASURY BONDS','M1','M2','MONETARY BASE','TOTAL INST RESERVES',...
%        'NONBORROWED INST R','EXCHANGE RATE YEN','COMMODITY PRICE INDEX','CAPACITY UTIL RATE',...
%        'PERSONAL CONSUMPTION','EMPLOYMENT','AVG WEEKLY HOURS',...
%        'HOUSING STARTS','NEW ORDERS','NYSE COMPOSITE','AVG HOURLY EARNINGS',...
%        'CONSUMER EXPECTATIONS'};


slowindex=[(1:53)'; (102:118)'];

% number of factors, lags in B(L):
numfac=[0,1,3,5,7];
K=numfac(num); 
lags=13;
nrep1=1; % on observation equation
nrep2=100; % on VAR equation
nsteps=48;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

N=size(X,2);
M=size(Y,2);

% shock=[zeros(1,K+M-1) .078125]'; % in terms of standard deviation, identification is recursive
shock=[zeros(1,K+M-1) .25/stdffr]'; % in terms of standard deviation, identification is recursive
[FYresp,Xresp]=irfbootfac2step(Y,lags,X,xindex,slowindex,K,shock,nsteps-1,nrep1,nrep2);	% nsteps x equations x error bands
FYresp=permute(FYresp,[2,1,3]);		% equations x nsteps x error bands
Xresp=permute(Xresp,[2,1,3]);		% equations x nsteps x error bands

VARresp=[squeeze(Xresp(1,:,1))' squeeze(Xresp(2,:,1))' squeeze(FYresp(end,:,1))'];

name=['FAVARresp' int2str(K)];

eval(['save ' name '.mat VARresp']);         % Saving results to output file: IRF's, 
%     
% 
% names={'FY','X'};
% for n = 1:2
%     save([mydir sprintf('%srespni_%d_%d_%d_%d.mat',names{n},K,lags,nrep1,nrep2)],...
%         sprintf('%sresp',names{n}),'-mat');
% end
%clear  % when computing for many K's at one execution
end
% profile report


% Plotting Impulse Response Functions

resp=Xresp;

% index=[16 5;107 5;77 1;80 1;91 5;95 5;96 5;97 5;74 5;101 1;17 1;49 5;32 5;46 1;54 4;62 1;66 5;118 5;119 1];
% varnames={'IP','CPI','3m TREASURY BILLS','5y TREASURY BONDS','M1','MONETARY BASE','TOTAL INST RESERVES',...
%         'NONBORROWED INST R','EXCHANGE RATE YEN','COMMODITY PRICE INDEX','CAPACITY UTIL RATE',...
%         'PERSONAL CONSUMPTION','EMPLOYMENT','AVG WEEKLY HOURS',...
%         'HOUSING STARTS','NEW ORDERS','NYSE COMPOSITE','AVG HOURLY EARNINGS',...
%         'CONSUMER EXPECTATIONS'};


index=[16 5;107 5;77 1;80 1;95 5;92 5;74 5;101 1;17 1;49 5;50 5;51 5;26 1;48 1;117 5;54 4;62 1;71 1;119 1];
varnames={'IP','CPI','3m TREASURY BILLS','5y TREASURYBONDS','MONETARY BASE','M2',...
        'EXCHANGE RATE YEN','COMMODITY PRICE INDEX','CAPACITY UTIL RATE',...
        'PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS','UNEMPLOYMENT','EMPLOYMENT','AVG HOURLY EARNINGS',...
        'HOUSING STARTS','NEW ORDERS','DIVIDENDS','CONSUMER EXPECTATIONS'};

nsteps=48;
n=size(resp,1);

% transform back to levels
for i=1:n
    if index(i,2)==4
        resp(i,:,:)=exp(resp(i,:,:))-ones(1,nsteps,3);
    elseif index(i,2)==5
        resp(i,:,:)=exp(cumsum(resp(i,:,:),2))-ones(1,nsteps,3);
    end
end

s=1:nsteps; z=zeros(nsteps,1);	% to plot level zero
figure

   subplot(4,5,1)
   plot(s,[z squeeze(FYresp(end,:,:))],'LineWidth',1.5);
   set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
   title('FFR')

for i=1:19
   subplot(4,5,i+1)
   plot(s,[z squeeze(resp(i,:,:))],'LineWidth',1.5);
   set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
   title(varnames(i))
end