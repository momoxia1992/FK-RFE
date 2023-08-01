rm(list = ls())
library(MASS)
library(Matrix)
n=100;
p=100;

###########Linear model#####
q=5
beta=c(1,1,1,1,1,rep(0,p-q))
rhos=0.5
sigma<-matrix(0,nrow=p,ncol=p)
for(j in 1:p)
{ for(k in 1:p)
{if(j==k) sigma[j,k]<-1
else sigma[j,k]<-rhos^(abs(j-k))}}
x<-mvrnorm(n,rep(0,p),sigma)
#####model 1.a###
e<-rnorm(n,0,1)
y=x%*%beta+e
#####model 1.b###
e<-rcauchy(n,0,1)
y=x%*%beta+e
######model 1.c####
e<-rnorm(n,0,1)
y=exp(x%*%beta+e)

######model 1.d 不可用
#u<-rcauchy(p*n,0,1)
#dim(u)=c(p,n)
#x=sigma%*%u
#e<-rnorm(n,0,1)
#y=x%*%beta+e
######model 1.e 不可用
#u<-rcauchy(p*p,0,1)
#dim(u)=c(p,n)
#x=sigma%*%u
#e<-rcauchy(n,0,1)
#y=x%*%beta+e

######model 2####
rhos=0.5
sigma<-matrix(0,nrow=p,ncol=p)
for(j in 1:p)
{ for(k in 1:p)
{if(j==k) sigma[j,k]<-1
else sigma[j,k]<-rhos^(abs(j-k))}}
x<-mvrnorm(n,rep(0,p),sigma)
e<-rnorm(n,0,1)
beta=c(rep(1,4),rep(0,(p-4)))
######model 2.a######
y=2*(x[,1]+x[,2])+2*tan(pi*x[,3]/2)+5*x[,4]+e
#####model 2.b######不太理想
x4=rep(0,n)
x4[which(x[,4]>0)]=1
y=3*x[,1]+3*x[,2]^3+3/x[,3]+5*x4+e
####model 2.c#####
y=3*x[,1]*x[,2]+2*x[,3]+2*x[,4]+e
####model 2.d######也不太理想
x4=rep(0,n)
x4[which(x[,4]<0)]=1
y=3*x[,1]*x[,2]+2*x[,3]*x4+e
######model 2.e#####效果不好，不用
#y=2*x[,1]+5*x[,2]^3*exp(-5*(x[,3]+x[,4]^2))+e

######model 3 mai2015 ex1#####
q=2
rhos=0.5
sigma<-matrix(0,nrow=p,ncol=p)
for(j in 1:p)
{ for(k in 1:p)
{if(j==k) sigma[j,k]<-1
else sigma[j,k]<-rhos^(abs(j-k))}}
x<-mvrnorm(n,rep(0,p),sigma)
e<-rnorm(n,0,1)
beta=c(2.8,-2.8,rep(0,(p-2)))
#######model 3.a#######
y=x%*%beta+e
######model 3.b####
y=2.8*sign(x[,1])*abs(x[,1])^{1/9}-2.8*sign(x[,2])*abs(x[,2])^{1/9}+e
######model 3.c####
y1=x%*%beta+e
y=y1^9

#######model 4 mai2015 ex3-5#####
######model 4.a#####
beta=c(1,1,rep(0,p-2))####虚
e<-rnorm(n,0,1)
x<-rcauchy(p*n,0,1)
dim(x)=c(n,p)
y=(x[,1]+x[,2]+1)^3+e
#####model 4.b###
beta=c(1,1,1,rep(0,p-3))####虚
x<-runif(n*p,0,1)
dim(x)=c(n,p)
e<-rnorm(n,0,1)
y=4*x[,1]+2*tan(pi*x[,2]/2)+5*x[,3]^2+e
#######model 4.c######
beta=c(0.8,-0.8,rep(0,p-2))
x=rt(n*p,2)
dim(x)=c(n,p)
u=exp(x%*%beta)
y=rpois(n,u)

#####Possion regression ####
q=5
beta=c(1,1,1,1,1,rep(0,p-q))
rhos=0.5
sigma<-matrix(0,nrow=p,ncol=p)
for(j in 1:p)
{ for(k in 1:p)
{if(j==k) sigma[j,k]<-1
else sigma[j,k]<-rhos^(abs(j-k))}}
x<-mvrnorm(n,rep(0,p),sigma)
######2
y=rpois(n,exp(x%*%beta))

