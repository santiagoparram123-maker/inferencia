

#Clase 5 de agosto

n=15
p=0.4

pbinom(9,n,p)-pbinom(6,n,p)


mu=n*p;mu
sigma=sqrt(n*p*(1-p));sigma

pnorm(9.5,mu,sigma)-pnorm(6.5,mu,sigma)

n=100
p=0.4
mu=n*p;mu
var=n*p*(1-p);var
sigma=sqrt(var);sigma

pbinom(29,n,p)
pnorm(29.5,mu,sigma)




#Distribución chi cuadrado

gl=10
qchisq(0.05,gl,lower.tail = FALSE)
qchisq(0.95,gl)


gl=15
qchisq(0.975,gl)
qchisq(0.025,gl,lower.tail = FALSE)
qchisq(0.025,gl)


n=10
s2=3.76
varianza_pobla=2

chi.2=s2*(n-1)/varianza_pobla;chi.2

pchisq(chi.2,n-1,lower.tail=FALSE)

n=10
qchisq(0.95,n-1)/9

qchisq(0.05,n-1)/9

