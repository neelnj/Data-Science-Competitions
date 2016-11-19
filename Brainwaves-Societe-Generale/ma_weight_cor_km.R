E=read.csv("train.csv")

cl=E[,ncol(E)]
cl=as.factor(cl)
E=E[,-ncol(E)]

F=read.csv("test.csv")
E=rbind(E,F)

E=E[,-1]
E=E[1:200,]

ma <- function(x,n){filter(x,2*c(1:n)/(n*(n+1)), sides=2)}
n=20
ME=ma(E,n)
#ME=ME[-(1:n),]
#TE=E[-(1:n),]
rem=which(is.na(ME[,1]))
ME=ME[-(rem),]
TE=E[-(rem),]

NE=matrix(-1,ncol = ncol(ME),nrow = nrow(ME))
for(i in 1:nrow(ME))
{
  for(j in 1:ncol(ME))
  {
    if(ME[i,j]>=TE[i,j])
    {
      NE[i,j]=1
    }
  }
}
C=cor(NE)

km=kmeans(C,7)
pred=km$cluster