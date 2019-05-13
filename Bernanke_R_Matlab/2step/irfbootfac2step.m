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

function [imp,impx]=irfbootfac2step(y,lags,x,xindex,slowindex,nf,shock,nir,nrep1,nrep2)

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

bx0 = olssvd(xir,fy0);

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

% Variance Decomposition


Fb0 = companion(b0(2:end,:)',lags,cols(e0));


% shock=zeros(cols(e0),1);
% shock(end)=1;
% [r2com,vardcom]=vardecx(xir,fy0,bx0,Fb0,smat0,36,shock);

% vardcom
% 
% r2com
% stop

imp=repmat(NaN,(nir+1)*(M+nf),nrep1*nrep2);
impx=repmat(NaN,(nir+1)*size(xindex,1),nrep1*nrep2);
repetition=0;
for frep=1:nrep1
    
    T = size(y,1);
  	i = ones(T+1,1)+(T-1)*rand(T+1,1);
	i = round(i);
    x = F0*Lf0' + u0(i(1:end-1),:);
    xslow=x(:,slowindex);
    
    [F,Lf]=extract(x,nf);
    [Fslow,Lfslow]=extract(xslow,nf);
    Fr = facrot(F,y(:,end),Fslow);
    fy=[Fr,y];

    bx = olssvd(xir,fy);
    ex = xir - fy*bx;

    % compute lagged fy

    lfy=ones(size(fy,1),1);	% need 1's for irfbootstrap.m
    for i=1:lags
        lfy=[lfy lagn(fy,i)];
    end
    fy=fy(lags+1:end,:);
    lfy=lfy(lags+1:end,:);

    b = olssvd(fy,lfy);
    e = fy - lfy*b;
    
    T = size(fy,1);
    k = size(fy,2);
    p = (size(lfy,2)-1)/k;

    br = zeros(size(b));
    bxr = zeros (size(bx));

    btilda=b;
    bxtilda=bx;
           
    % Step 2a: IRF bootstrap using the bias-adjusted coeffs.
     
    for rep = 1:nrep2
        repetition=repetition+1;
        
    	i = ones(T+1,1)+(T-1)*rand(T+1,1);
    	i = round(i);
        lfyi = lfy(round(1+(T-1)*rand),:);
        lfyr = zeros(T+1,size(lfyi,2));
        fyr	 = zeros(T+1,size(e,2));
   
        for ii = 1:T+1
            fyi = lfyi*btilda + e(i(ii),:);
            fyr(ii,:) = fyi;
            lfyi = [1,fyi,lfyi(:,2:end-k)];
            lfyr(ii,:) = lfyi;
        end
        lfyr=lfyr(1:T,:);
        fyr=fyr(2:T+1,:);
      	xr = fyr*bxtilda + ex(i(1:end-1),:);
        bxr = olssvd(xr,fyr);
    	br = olssvd(fyr,lfyr);
    	er = fyr - lfyr*br;
      	Siger = er'*er/(size(er,1)-p*k-1);
    	smatr = chol(Siger)';
        d=diag(diag(smatr));
        smatr=inv(d)*smatr;
        impr = irfsim(br,smatr,shock,p,nir);
        impxr = impr*bxr;
        imp(:,repetition) = reshape(impr,(nir+1)*(M+nf),1);
        impx(:,repetition) = reshape(impxr,(nir+1)*size(xindex,1),1);
    end
    frep
end

Sige = e0'*e0/(size(e0,1)-p*k-1);
	
smat = chol(Sige)';
d=diag(diag(smat));
smat=inv(d)*smat;

impest = irfsim(b0,smat,shock,p,nir);
impxest = impest*bx0;

imp=reshape(imp,nir+1,M+nf,nrep1*nrep2);
impx=reshape(impx,nir+1,size(xindex,1),nrep1*nrep2);

imp = sort(imp,3);
impx = sort(impx,3);

nrep=nrep1*nrep2;
impci = imp(:,:,[.05*nrep .95*nrep]);
impxci = impx(:,:,[.05*nrep .95*nrep]);


imp=cat(3,impest,impci);
impx = cat(3,impxest,impxci);