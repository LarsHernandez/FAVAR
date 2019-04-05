% Computes IRF through "simulations" 
%(Note: Not clear if using companion matrix is more efficent)
%Jean Boivin
%11/08/01

function imp=irfsim(b,smat,shock,p,nimp);

b=b(2:end,:);
imp=(smat*shock)';
k=size(smat,1);

ly=[imp zeros(1,(p-1)*k)];

for i=1:nimp
   
   impt	=	ly*b;
   ly 	=	[impt ly(:,1:(p-1)*k)];
   imp	=	cat(1,imp,impt);
   
end