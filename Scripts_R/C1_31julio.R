

n=500
p=0.55
se=sqrt(p*(1-p)/n);se

pnorm(0.6,p,se,lower.tail = FALSE)

z=(0.6-p)/se;z
pnorm(z,lower.tail = FALSE)


p=0.3
n=100
se=sqrt(p*(1-p)/n);se
pnorm(0.4,p,se)-pnorm(0.2,p,se)
pnorm(0.25,p,se,lower.tail=FALSE)
