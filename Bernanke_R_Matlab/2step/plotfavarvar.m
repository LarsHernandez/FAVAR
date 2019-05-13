load FAVARresp5;
FAVAR=VARresp;

load VARresp0;
VAR=FYresp;

load VARresp1;
VARF1=FYresp;

load VARresp3;
VARF3=FYresp;

load VARresp5;
VARF5=FYresp;


nsteps=48;
n=size(FYresp,1);


s=1:nsteps; z=zeros(nsteps,1);	% to plot level zero
figure

subplot(2,2,1)
plot(s,[FAVAR(:,end) VAR(:,end) VARF1(:,end) VARF3(:,end) VARF5(:,end)]);
set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
legend('Baseline (Y=FFR, K=5)','VAR (Y={IP,CPI,FFR}, K=0)','VAR & 1 factor (Y={IP,CPI,FFR}, K=1)',...
'VAR & 3 factor (Y={IP,CPI,FFR}, K=3)','VAR & 5 factor (Y={IP,CPI,FFR}, K=5)');
title('FFR')

hold on
plot(s,z)
hold off

subplot(2,2,2)
respFAVAR=exp(cumsum(FAVAR(:,end-2),1))-ones(nsteps,1);
respVAR=exp(cumsum(VAR(:,end-2),1))-ones(nsteps,1);
respVARF1=exp(cumsum(VARF1(:,end-2),1))-ones(nsteps,1);
respVARF3=exp(cumsum(VARF3(:,end-2),1))-ones(nsteps,1);
respVARF5=exp(cumsum(VARF5(:,end-2),1))-ones(nsteps,1);

plot(s,[respFAVAR respVAR respVARF1 respVARF3 respVARF3]);
set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
   title('IP')
hold on
plot(s,z)
hold off

subplot(2,2,3)
respFAVAR=exp(cumsum(FAVAR(:,end-1),1))-ones(nsteps,1);
respVAR=exp(cumsum(VAR(:,end-1),1))-ones(nsteps,1);
respVARF1=exp(cumsum(VARF1(:,end-1),1))-ones(nsteps,1);
respVARF3=exp(cumsum(VARF3(:,end-1),1))-ones(nsteps,1);
respVARF5=exp(cumsum(VARF5(:,end-1),1))-ones(nsteps,1);

plot(s,[respFAVAR respVAR respVARF1 respVARF3 respVARF3]);
set(gca,'XLim',[0 nsteps],'XTick',[0 nsteps],'FontSize',8)
title('CPI')

hold on
plot(s,z)
hold off
   
  