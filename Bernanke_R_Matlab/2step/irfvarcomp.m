% Bootstrap confidence interval on IRF's, Based on Kilian (1998)
% Jean Boivin
% 11/08/01
% modified 9/14/02 to resample factors

% y = T x K matrix of dependent variables
% x = T x N1 (N1 <= N) matrix of series of interest
% k = number of factors
% shock = K x 1 vector of initial impulse
% nir = number of periods in the IRF
% nrep1 = number of bootstrap replications -- factors
% nrep2                                    -- parameters
% xindex specifies elements of x we want impulse responses

function [imp]=irfvarcomp(y,lags,x,xindex,slowindex,nf,shock,nir)

[T,N]=size(x);
M=size(y,2);

% x's for impulse responses
xir=x(:,xindex);

% first step - extract PC from X
[F0,Lf0]=extract(x,nf);
u0=x-F0*Lf0';

xslow=x(:,slowindex);
[Fslow0,Lfslow0]=extract(xslow,nf);

Fr0 = facrot(F0,y(:,end),Fslow0);

fy0=[Fr0,y];

% compute lagged fy
lfy0=ones(size(y,1),1);	% need 1's for irfbootstrap.m
for i=1:lags
    lfy0=[lfy0 lagn(fy0,i)];
end
fy0=fy0(lags+1:end,:);
lfy0=lfy0(lags+1:end,:);
b0 = olssvd(fy0,lfy0);
e0 = fy0 - lfy0*b0;
% Sige0 = e0'*e0/(size(e0,1)-lags*cols(e0)-1);
Sige0=cov(e0);
smat0 = chol(Sige0)';
% d=diag(diag(smat0));
% smat0=inv(d)*smat0;
k = size(fy0,2);
p = (size(lfy0,2)-1)/k;

% Variance Decomposition


Fb0 = companion(b0(2:end,:)',lags,cols(e0));

Sige = e0'*e0/(size(e0,1)-p*k-1);
	
smat = chol(Sige)';
d=diag(diag(smat));
smat=inv(d)*smat;

imp = irfsim(b0,smat,shock,p,nir);
