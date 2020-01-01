# Statistical-tables-in-R

#Standard Normal Tables 
rm(list = ls())
s=seq(0,4,0.01)
a=pnorm(s)
m=matrix(a,ncol = 10,byrow = TRUE)
rownames(m)=seq(0,4,.1)
colnames(m)=seq(0,.09,.01)
round(m,4)

# Chisquare Tables
rm(list = ls())
s=c(99.5,99,97.5,95,10,5,2.5,1,0.5,0.1)
for(i in 1:100){
  r1=qchisq(1-(s/100),i)
  cat(i,round(r1,2),"\n")
}


# F Tables,alpha=0.05,0.01
rm(list = ls())
m=seq(1,12,1)
i=1
n=as.integer(readline(prompt = "What % of values do you want? For 5%, press 1 and for 1%, press 2:"))
if(n==1){
while(i<101){
r2=qf(0.95,m,i)
cat(i,round(r2,3),"\n")
i=i+1}
  }else{
if(n==2){
  while(i<101){
  r2=qf(0.99,m,i)
  cat(i,round(r2,3),"\n")
  i=i+1}
  }}
#Student-t tables
rm(list = ls())
a=c(25,10,5,2,1,0.2,0.1)
i=1
while(i<51){
r3=qt((1-(a/100)/2),i)
cat(i,round(r3,4),"\n")
i=i+1}
