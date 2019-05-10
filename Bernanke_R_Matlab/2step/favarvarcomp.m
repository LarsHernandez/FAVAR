% estimates favar by principal components
% confidence intervals are from a two-step bootstrap
% X is TxN, Y is TxM , F is TxK
% state equation:       [F(t)' Y(t)']'=B(L)[F(t-1)' Y(t-1)']'+v(t), v ~ N(0,Q), Q unrestricted
% X is Stock&Watson balanced panel less Y, observable factors
% Piotr Eliasz, 9/11/2002
profile on -detail builtin

for num=1:5;
    

mydir = 'c:/research/BBE/may2003/';    
% mydir = '/home/peliasz/october/';
%mydir = '\piotr\DRI\october\';

load('nsbalpanel.txt','-ascii')
% need to standardize for PC:
T=size(nsbalpanel,1);
stdffr=std(nsbalpanel(:,77))
nsbalpanel=(nsbalpanel-repmat(mean(nsbalpanel,1),T,1))./repmat(std(nsbalpanel,1),T,1);

%%%%%%%%%%%%%%%%%%%%%%%%% choose specification %%%%%%%%%%%%%%%%%%%%%%%%


xindex=zeros(120,1);
xindex([78;81;92;96;97;98;74;102;17;49;32;46;54;62;66;119;120])=1; % index defined w.r.t. nsbalpanel
xindorder=[9;10;11;12;13;14;8;15;1;4;2;3;5;6;7;16;17];

%varnames={'3m TREASURY BILLS','5y TREASURY BONDS','M1','M2','MONETARY BASE','TOTAL INST RESERVES',...
%        'NONBORROWED INST R','EXCHANGE RATE YEN','COMMODITY PRICE INDEX','CAPACITY UTIL RATE',...
%        'PERSONAL CONSUMPTION','EMPLOYMENT','AVG WEEKLY HOURS',...
%        'HOUSING STARTS','NEW ORDERS','NYSE COMPOSITE','AVG HOURLY EARNINGS',...
%        'CONSUMER EXPECTATIONS'};
slowindex=zeros(120,1);
slowindex([(1:53)'; (103:119)'])=1; % index defined w.r.t. nsbalpanel


yindex=[16 108 77]; % index defined w.r.t. nsbalpanel

nyindex=ones(120,1);
nyindex(yindex)=0;
Y=nsbalpanel(:,yindex); % this is IP, CPI and FFR
X=nsbalpanel(:,find(nyindex==1));
xindex=xindex(find(nyindex==1));     % index defined w.r.t. X
xindex=find(xindex==1);
xindex=xindex(xindorder);
slowindex=find(slowindex(find(nyindex==1))==1); % index defined w.r.t. X


% number of factors, lags in B(L):
numfac=[0,1,3,5,7];
K=numfac(num); 
lags=13;
nrep1=10; % on observation equation
nrep2=10; % on VAR equation
nsteps=48;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

N=size(X,2);
M=size(Y,2);

% shock=[zeros(1,K+M-1) .078125]'; % in terms of standard deviation, identification is recursive
shock=[zeros(1,K+M-1) .25/stdffr]'; % in terms of standard deviation, identification is recursive
FYresp=irfvarcomp(Y,lags,X,xindex,slowindex,K,shock,nsteps-1);	% nsteps x equations x error bands


name=['VARresp' int2str(K)];

eval(['save ' name '.mat FYresp']);         % Saving results to output file: IRF's, 
    

% Plotting Impulse Response Functions

nsteps=48;
n=size(FYresp,1);


s=1:nsteps; z=zeros(nsteps,1);	% to plot level zero
figure

   subplot(2,2,1)
   plot(s,[z FYresp(:,end)]);
   set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
   title('FFR')

   subplot(2,2,2)
   resp=exp(cumsum(FYresp(:,end-2),1))-ones(nsteps,1);

   plot(s,[z resp]);
   set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
   title('IP')

   subplot(2,2,3)
   resp=exp(cumsum(FYresp(:,end-1),1))-ones(nsteps,1);

   plot(s,[z resp]);
   set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
   title('CPI')

   
end