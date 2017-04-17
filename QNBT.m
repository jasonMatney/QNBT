pan=load('trimmed_Pano.csv'); 
nrs=load('trimmed_NRRS.csv'); 

% here I sum all the years data 
x=sum(pan(:,2:end),2); y=sum(nrs(:,2:end),2); 
a=find(x>0 & y >0);  x=x(a); y=y(a); 

% one way to fit a power low is to log10 transformal both variables. 
P=polyfit(log10(x),log10(y),1); 
B=P(1);  Yo=10^P(2);   % these are you parameters
ypred=Yo*x.^B;   % this is the predicted curve  - 10^3.08 * x^0.32
figure; subplot(2,1,1); loglog(x,y,'ko'); hold on; loglog(x,ypred,'r')
title(['Linear fit to log10 transformed data   B= ' num2str(B) ' Yo= ' num2str(Yo) ''])
set(gca,'FontSize',16); ylim([0,10^6]); grid on; 


% another way is to use a nonlinear curve fitting routine on the data
% (don't transform it). 
% set the initial guesses for the variable to be fit 
po=[0.3, 100];    % inital guesses for B and Yo
lb=[ 0.01, 1];       % lower bounds 
ub=[5, 10000];      % upper bounds 

% the function to be minized 
options=optimset('Display', 'iter', 'TolFun', 1e-16);
fun = @(p)((p(2).*x.^p(1))-(y));      %% or this would be the same as log10 fitting fun = @(p)(log10(p(2).*x.^p(1))-log10(y));  
xout = lsqnonlin(fun,po,lb,ub);     % perform the minimization and return xout = [Omega, Fcor, Qfac, n] 
B2=xout(1); Yo2=xout(2); 
ypred2=Yo2*x.^B2;  
subplot(2,1,2); loglog(x,y,'ko'); hold on; loglog(x,ypred2,'r')
title(['This is the NLS solution   B= ' num2str(B2) ' Yo= ' num2str(Yo2) ''])
set(gca,'FontSize',16); ylim([0,10^6]); grid on; 
    
