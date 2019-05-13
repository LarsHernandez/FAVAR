% Forms the Companion matrix of a VAR
% Jean Boivin
% 11/18/01

% b = k x k*p+1 matrix of VAR paramters (includes the constant)
% p = number of lags
% k = number of variables in the VAR

function [F]=companion(b,p,k);


F = zeros(p*k,p*k);
F(1:k,:) = b;		% removes the constant estimate
for jj = 1:p-1;
	F(k*jj+1:(jj+1)*k,(jj-1)*k+1:k*jj)=eye(k);
end;