 syms x;
 syms y;

 clear Q*;
 clear CQ*;

 step = 60;
 mttf = 4.3*30*24*60*step/10;
 c = ((1/mttf)^2)/6;

T = 15*step;
RA= 45*step;
ub = 1/(65*step);
ua = log(10)/(15*step);
xs = 0.1/(1-exp(-15/(65*step)));
%<15
g(x) = ua*exp(-ua*x);
%>15
h(x) = xs*ub*exp(-ub*x);

%F=@(x,n)1./((1:100)+x);%定义被积函数
%quadv(@(x)F(x,5),0,1)%
for RB=1:1:15*step,
FA = @(x,y)y*(2*x-y)*g(x)*g(y);
FB = @(x,y)x^2*g(x)*g(y);
FBT = @(x,y)x^2*g(x)*h(y);
FC = @(x,y)y*(2*x-y)*h(x)*g(y);
FD = @(x,y)RB*(2*x-RB)*h(x)*g(y);
FDT = @(x,y)RB*(2*x-RB)*h(x)*h(y);
FE = @(x,y)y*(2*RA-y)*h(x)*g(y);
FF = @(x,y)RB*(2*RA-RB)*h(x)*g(y);
FFT = @(x,y)RB*(2*RA-RB)*h(x)*h(y);
%Q = dblquad(FA,0,y,0,2)
QA(RB) = int(int(FA,y,0,x),x,0,RB) + int(int(FB,y,x,T),x,0,RB) + int(int(FBT,y,T,inf),x,0,RB) + int(int(FC,y,0,RB),x,RB,RA) + int(int(FD,y,RB,T),x,RB,RA) +int(int(FDT,y,T,inf),x,RB,RA) + int(int(FE,y,0,RB),x,RA,inf) + int(int(FF,y,RB,T),x,RA,inf)+ int(int(FFT,y,T,inf),x,RA,inf);
CQA(RB) = c*QA(RB);
end;

ua = 1/(sqrt(2*pi)*7.5*step);
ub = 1/(sqrt(2*pi)*50*step/3);
uaxs = 0.9*2/0.6827
ubxs = 0.1/0.9973
%<15
g(x) = uaxs*ua*exp(-((x-7.5*step)/(7.5*step))^2);
%>15
h(x) = ubxs*ub*exp(-((x-65*step)*3/(50*step))^2);

%F=@(x,n)1./((1:100)+x);%定义被积函数
%quadv(@(x)F(x,5),0,1)%
for RB=1:1:15*step,
FA = @(x,y)y*(2*x-y)*g(x)*g(y);
FB = @(x,y)x^2*g(x)*g(y);
FBT = @(x,y)x^2*g(x)*h(y);
FC = @(x,y)y*(2*x-y)*h(x)*g(y);
FD = @(x,y)RB*(2*x-RB)*h(x)*g(y);
FDT = @(x,y)RB*(2*x-RB)*h(x)*h(y);
FE = @(x,y)y*(2*RA-y)*h(x)*g(y);
FF = @(x,y)RB*(2*RA-RB)*h(x)*g(y);
FFT = @(x,y)RB*(2*RA-RB)*h(x)*h(y);
%Q = dblquad(FA,0,y,0,2)
QB(RB) = int(int(FA,y,0,x),x,0,RB) + int(int(FB,y,x,T),x,0,RB) + int(int(FBT,y,T,inf),x,0,RB) + int(int(FC,y,0,RB),x,RB,RA) + int(int(FD,y,RB,T),x,RB,RA) +int(int(FDT,y,T,inf),x,RB,RA) + int(int(FE,y,0,RB),x,RA,inf) + int(int(FF,y,RB,T),x,RA,inf)+ int(int(FFT,y,T,inf),x,RA,inf);
CQB(RB) = c*QB(RB);
end;

upT = 115*step;
%<15
g(x) = 0.9/(15*step);
%>15
h(x) = 0.1/(100*step);

%F=@(x,n)1./((1:100)+x);%定义被积函数
%quadv(@(x)F(x,5),0,1)%
for RB=1:1:15*step,
FA = @(x,y)y*(2*x-y)*g(x)*g(y);
FB = @(x,y)x^2*g(x)*g(y);
FBT = @(x,y)x^2*g(x)*h(y);
FC = @(x,y)y*(2*x-y)*h(x)*g(y);
FD = @(x,y)RB*(2*x-RB)*h(x)*g(y);
FDT = @(x,y)RB*(2*x-RB)*h(x)*h(y);
FE = @(x,y)y*(2*RA-y)*h(x)*g(y);
FF = @(x,y)RB*(2*RA-RB)*h(x)*g(y);
FFT = @(x,y)RB*(2*RA-RB)*h(x)*h(y);
%Q = dblquad(FA,0,y,0,2)
QC(RB) = int(int(FA,y,0,x),x,0,RB) + int(int(FB,y,x,T),x,0,RB) + int(int(FBT,y,T,upT),x,0,RB) + int(int(FC,y,0,RB),x,RB,RA) + int(int(FD,y,RB,upT),x,RB,RA) +int(int(FDT,y,T,upT),x,RB,RA) + int(int(FE,y,0,RB),x,RA,upT) + int(int(FF,y,RB,T),x,RA,upT)+ int(int(FFT,y,T,upT),x,RA,upT);
CQC(RB) = c*QC(RB);
end;

plot(CQA)
hold on
plot(CQB)
plot(CQC)
grid on
xlabel('The Repair Time of Critical Blocks(minutes)')
ylabel('Probability of Data Unavailability')
title('Data Unavailability')
legend('exponential','normal','uniform')
hold off