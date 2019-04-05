% estimates favar by likelihood-based Gibbs sampling on not standardized data
% observation equation: [X(t)' Y(t)']'=[Lf Ly;0 I][F(t)' Y(t)']'+[e(t) 0], e ~ N(0,R), R diagonal
% X is TxN, Y is TxM , F is TxK, R is NxN
% state equation:       [F(t)' Y(t)']'=B(L)[F(t-1)' Y(t-1)']'+v(t), v ~ N(0,Q), Q unrestricted, e,v uncorrelated
% state vector is: [F(t)' Y(t)' ... F(t-lags+1)' Y(t-lags+1)']'
% to normalize the factors Lf(NxK) =[I(K);Lfd((N-K)xK)]
% data is a not standardized balanced panel of Stock&Watson (nsbalpanel.txt)
% Piotr Eliasz, 18/3/2002
% proper priors introduced on 1/11/2003
clear all

% directory
mydir = '';

load([mydir 'nsbalpanel.txt'],'-ascii')

%%%%%%%%%%%%%%%%%%%%%%%%% choose specification %%%%%%%%%%%%%%%%%%%%%%%%
% number of Gibbs replications:
gibbsrep=10000;

% number of factors, lags in B(L):
K=3;
lags=13;

% choose Y from the balanced panel
indexY=[77];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
indexYn=[1:size(nsbalpanel,2)];
indexYn(indexY)=[];
Y=nsbalpanel(:,indexY);
nsbalpanel(:,indexY)=[];
X=nsbalpanel;

[T,N]=size(X);
M=size(Y,2);
const=ones(T,1);

X=X-repmat(mean(X),T,1);
Y=Y-repmat(mean(Y),T,1);

% STANDARDIZE for PC only
X_st=X./repmat(std(X,1),T,1);
Y_st=Y./repmat(std(Y,1),T,1);

% first step - extract PC from X
[F0,Lf]=extract(X_st,K);

% regress X on F0 and Y, obtain loadings
Lfy=olssvd(X_st(:,K+1:N),[F0 Y_st])';     % upper KxM block of Ly set to zero
Lf=[Lf(1:K,:);Lfy(:,1:K)];
Ly=[zeros(K,M);Lfy(:,K+1:K+M)];

% transform factors and loadings for LE normalization
[ql,rl]=qr(Lf');
Lf=rl;  % do not transpose yet, is upper triangular
F0=F0*ql;
% need identity in the first K columns of Lf, call them A for now
A=Lf(:,1:K);
Lf=[eye(K),inv(A)*Lf(:,(K+1):N)]';
F0=F0*A;
% obtain R:
e=X_st-Y_st*Ly'-F0*Lf';
R=e'*e./T;
R=diag(diag(R));

% run a VAR in [F,Y], obtain initial B and Q
[B,Bc,v,Q,invFYFY]=estvar([F0,Y],lags,[]);

% put it all in state-space representation, write obs equ as XY=FY*L+e
XY=[X,Y];   %Tx(N+M)
FY=[F0,Y];
km=K+M;

L=[Lf Ly;zeros(M,K),eye(M)];    %(N+M)xkm
R=diag([diag(R);zeros(M,1)]);   %(N+M)x(N+M)

% adjust for lags in state equation, Q is kmxkm
Q=[Q zeros(km,km*(lags-1));zeros(km*(lags-1),km*lags)];
B=[B(:,:);eye(km*(lags-1)) zeros(km*(lags-1),km)];

% start with
S0=zeros(km*lags,1);
P0=eye(km*lags);

% store draws in:
Ldraw=zeros(gibbsrep,N+M,km);
for i=1:M
    Ldraw(:,indexY(i),K+i)=1;
end
Bdraw=zeros(gibbsrep,km,km,lags);
Qdraw=zeros(gibbsrep,km,km);
Fdraw=zeros(gibbsrep,T,K);    

% proper priors:
% on VAR -- Normal-Wishart, after Kadiyala, Karlsson, 1997
% on Q -- si
% on B -- increasing tightness
% on observable equation:
% N(0,I)-iG(3,0.001)

% prior distributions for VAR part, need B and Q
vo=km+2;
s0=3;
alpha=0.001;
L_var_prior=eye(km);
Qi=zeros(km,1);

% singles out latent factors
indexnM=[ones(K,lags);zeros(M,lags)];
indexnM=find(indexnM==1);

for rep = 1:gibbsrep
    rep
    
    % generate Gibbs draws of the factors
    FY=kfgibbsnv(XY,S0,P0,L,R,B,Q,M,indexnM);
    % demean
    FY=FY-repmat(mean(FY),T,1);
       
    % given factor draws sample all the coefficients from their posterior

    % state equation
    % first univ AR for scale in priors
    for i=1:km
        [Bi,Bci,vi,Qi(i),invFYFYi]=estvar(FY(:,i),lags,[]);
    end
    Q_prior=diag(Qi);
    B_var_prior=diag(kron(1./Qi',1./[1:lags]));
    [Bd,Bdc,v,Qd,invFYFY]=estvar(FY,lags,[]);
    B_hat=Bd(:,:)';
    Z=zeros(T,km,lags);
    for i=1:lags
        Z(lags+1:T,:,i)=FY(lags+1-i:T-i,:);
    end
    Z=Z(:,:);
    Z=Z(lags+1:T,:);
    iB_var_prior=inv(B_var_prior);
    B_var_post=inv(iB_var_prior+Z'*Z);
    B_post=B_var_post*(Z'*Z)*B_hat;
    Q_post=B_hat'*Z'*Z*B_hat+Q_prior+(T-lags)*Qd-B_post'*(iB_var_prior+Z'*Z)*B_post;
    
    % draw Q from inverse Wishart
    iQd=randn(T+vo,km)*chol(inv(Q_post));
    Qd=inv(iQd'*iQd);
    Q(1:km,1:km)=Qd;

    % draw B conditional on Q
    vecB_post=reshape(B_post,km*km*lags,1);
    vecBd = vecB_post+chol(kron(Qd,B_var_post))'*randn(km*km*lags,1);
    Bd = reshape(vecBd,km*lags,km)';
    B(1:km,:)=Bd;
    % truncate to ensure stationarity
    while max(abs(eig(B)))>0.999
        vecBd = vecB_post+chol(kron(Qd,B_var_post))'*randn(km*km*lags,1);
        Bd = reshape(vecBd,km*lags,km)';
        B(1:km,:)=Bd;
    end
    
    % observation equation
    % first consider first K series, with individual factors as regressors
    
    for n=1:N
        if n <= K
            Ld=zeros(km,1);
            Ld(n)=1;
        else
            Ld=olssvd(X(:,n),FY);
        end
        ed=X(:,n)-FY*Ld;

        % draw R(n,n)
        R_bar=s0+ed'*ed+Ld'*inv(L_var_prior+inv(FY'*FY))*Ld;        
        Rd=chi2rnd(T+alpha);
        Rd=R_bar/Rd;
        R(n,n)=Rd;

        % draw L(n,1:km):
        if n > K
            L_var_post=inv(inv(L_var_prior)+FY'*FY);
            Lpostmean=L_var_post*(FY'*FY)*Ld;
            Ld=Lpostmean'+randn(1,km)*chol(Rd*L_var_post);
            L(n,1:km)=Ld;
        end
    end
    
    % save draws
    Ldraw(rep,indexYn,:)=L(1:N,1:km);
    Bdraw(rep,:,:,:)= reshape(Bd,km,km,lags);
    Qdraw(rep,:,:)=Qd;
    Fdraw(rep,:,:)=FY(:,1:K);
end
Fdraw=squeeze(median(Fdraw,1));

names={'L','B','Q','F'};
for n = 1:size(names,2)
    save([mydir sprintf('%s_%d_%d_%d.mat',names{n},K,lags,gibbsrep)],...
        sprintf('%sdraw',names{n}),'-mat');
end

% use plot_oi.m to plot the resulting impulse responses
