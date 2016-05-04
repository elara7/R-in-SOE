beta0=1:1000
beta1=1:1000
b1=20;b2=0.6;n=25
x=10:34
for(i in 1:1000){
        u=rnorm(25)
        y=b1+b2*x+u
        lm.m=lm(y~x)
        beta0[i]=as.numeric(coef(lm.m)[1])
        beta1[i]=as.numeric(coef(lm.m)[2])
}
mean(beta0)
mean(beta1)