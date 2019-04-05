% plots impulse responses in FAVAR model estimated by Gibbs or PC
clear all
% directory:
mydir = '\...\';
replications=10000;
discard=2000;  % discard initial draws
draws=replications-discard;

lags=13;
nsteps=48;
k=3;    % number of factors

t=1:nsteps;
z=zeros(nsteps,1);

% choose variables from the panel:
index=[77 1;16 5;108 5;78 1;81 1;96 5;93 5;74 5;102 1;17 1;49 5;50 5;51 5;26 1;48 1;118 5;54 4;62 1;71 1;120 1];
varnames={'FFR','IP','CPI','3m TREASURY BILLS','5y TREASURY BONDS','MONETARY BASE','M2',...
        'EXCHANGE RATE YEN','COMMODITY PRICE INDEX','CAPACITY UTIL RATE',...
        'PERSONAL CONSUMPTION','DURABLE CONS','NONDURABLE CONS','UNEMPLOYMENT','EMPLOYMENT','AVG HOURLY EARNINGS',...
        'HOUSING STARTS','NEW ORDERS','DIVIDENDS','CONSUMER EXPECTATIONS'};

% to transform scale of impulse responses to standard deviation form:
load([mydir 'nsbalpanel.txt'],'-ascii')
x=nsbalpanel;
x=x(:,index(:,1));
scale=std(x);

names={'L','B','Q'};
for n = 1:size(names,2)
    load([mydir 'draws\' sprintf('%s_%d_%d_%d.mat',names{n},k,lags,replications)]);
end
L=Ldraw; B=Bdraw; Q=Qdraw;
clear Ldraw Bdraw Qdraw

km=size(L,3);

% consider triangular identification
% chol takes only 2d argumets, therefore need to proceed draw by draw
responses1=zeros(draws,km,km,nsteps);

% vector of initial shocks (policy)
inishocks=diag([zeros(1,km-1) .25]);
        
for rep1=1:draws
    rep1
    
    Bv=squeeze(B(discard+rep1,:,:,:));
    Qv=squeeze(Q(discard+rep1,:,:));
    
    smat=chol(Qv);
    d=diag(diag(smat));
    smat=inv(d)*smat;
        
    % smat is upper triangular decomposition of omega; gives matrix of initial shocks with 1's on the diagonal
    smat=inishocks*smat;
        
    responses1(rep1,:,:,:)=impulse(Bv,smat,nsteps);
end

% restrict to policy shocks
responses1=squeeze(responses1(:,:,km,:));

L=permute(L,[2 3 1]);
L=L(index(:,1),:,:); % this is X*F*draws
responses1=permute(responses1,[2 3 1]);

responses=zeros(size(index,1),nsteps,draws);
for rep2=1:draws
    rep2
    responses(:,:,rep2)=L(:,:,discard+rep2)*responses1(:,:,rep2);
end
% this is series*nsteps*draws

% transform back to levels
for i=1:size(responses,1);
    if index(i,2)==4
        responses(i,:,:)=exp(responses(i,:,:))-ones(1,nsteps,draws);
    elseif index(i,2)==5
        responses(i,:,:)=exp(cumsum(responses(i,:,:),2))-ones(1,nsteps,draws);
    end
end

% sort 
responses=sort(responses,3);

% impulse response estimate
response=median(responses,3);

% confidence intervals
leftc=responses(:,:,.05*draws);
rightc=responses(:,:,.95*draws);

% concatenate the estimate and confidence bounds
respconf=cat(3,leftc,response,rightc);

% use only if notst data
% transform scale to std
respconf=respconf./repmat(scale',[1 nsteps 3]);

figure
% plot
for i=1:12
    subplot(3,4,i)
    plot(t,z,':',t,squeeze(respconf(i,:,1)),'--',t,squeeze(respconf(i,:,2)),'-',...
        t,squeeze(respconf(i,:,3)),'--','LineWidth',1.5);
    set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
    title(varnames(i))
end
figure
for i=13:20
    subplot(3,4,i-12)
    plot(t,z,':',t,squeeze(respconf(i,:,1)),'--',t,squeeze(respconf(i,:,2)),'-',...
        t,squeeze(respconf(i,:,3)),'--','LineWidth',1.5);
    set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
    title(varnames(i))
end