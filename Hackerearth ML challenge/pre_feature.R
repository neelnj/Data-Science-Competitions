setwd("C:/Users/neel jambhekar/Desktop/hackerearth ML")
E=read.csv("train_indessa.csv")
train=E
F=read.csv("test_indessa.csv")
test=F
cl=train[,45]
train=train[,-45]

na=vector(length = ncol(train))
for(i in 1:ncol(train))
{
  na[i]=length(which(is.na(train[,i])==T))
}
print(na)

#feat_1_train=train[,2]-train[,3]
#feat_1_train=sign(feat_1_train)

#feat_2_train=train[,3]-train[,4]
#feat_2_train=sign(feat_2_train)

#feat_3_train=train[,2]-train[,4]
#feat_3_train=sign(feat_3_train)

#feat_mat=cbind(feat_1_train,feat_2_train,feat_3_train)

a13=which(is.na(train[,13])==TRUE)
train[a13,13]=0

a22=which(is.na(train[,22])==TRUE)
train[a22,22]=-1

a23=which(is.na(train[,23])==TRUE)
train[a23,23]=-1

a26=which(is.na(train[,26])==TRUE)
train[a26,26]=-1

a27=which(is.na(train[,27])==TRUE)
train[a27,27]=-1

a29=which(is.na(train[,29])==TRUE)
train[a29,29]=-1

a30=which(is.na(train[,30])==TRUE)
train[a30,30]=-1

a36=which(is.na(train[,36])==TRUE)
train[a36,36]=-1

m41=median(train[,41],na.rm = TRUE)
a41=which(is.na(train[,41])==TRUE)
train[a41,41]=m41

a42=which(is.na(train[,42])==TRUE)
train[a42,42]=-1
a43=which(is.na(train[,43])==TRUE)
train[a43,43]=-1
a44=which(is.na(train[,44])==TRUE)
train[a44,44]=-1

train=train[,-c(1,6,10,16,18,19,24,25,37)]

features_set=train

train=test
na=vector(length = ncol(train))
for(i in 1:ncol(train))
{
  na[i]=length(which(is.na(train[,i])==T))
}
print(na)

#feat_1_train=train[,2]-train[,3]
#feat_1_train=sign(feat_1_train)

#feat_2_train=train[,3]-train[,4]
#feat_2_train=sign(feat_2_train)

#feat_3_train=train[,2]-train[,4]
#feat_3_train=sign(feat_3_train)

#feat_mat=cbind(feat_1_train,feat_2_train,feat_3_train)

a13=which(is.na(train[,13])==TRUE)
train[a13,13]=0

a22=which(is.na(train[,22])==TRUE)
train[a22,22]=-1

a23=which(is.na(train[,23])==TRUE)
train[a23,23]=-1

a26=which(is.na(train[,26])==TRUE)
train[a26,26]=-1

a27=which(is.na(train[,27])==TRUE)
train[a27,27]=-1

a29=which(is.na(train[,29])==TRUE)
train[a29,29]=-1

a30=which(is.na(train[,30])==TRUE)
train[a30,30]=-1

a36=which(is.na(train[,36])==TRUE)
train[a36,36]=-1

m41=median(train[,41],na.rm = TRUE)
a41=which(is.na(train[,41])==TRUE)
train[a41,41]=m41

a42=which(is.na(train[,42])==TRUE)
train[a42,42]=-1
a43=which(is.na(train[,43])==TRUE)
train[a43,43]=-1
a44=which(is.na(train[,44])==TRUE)
train[a44,44]=-1

train=train[,-c(1,6,10,16,18,19,24,25,37)]

mat=rbind(features_set,train)
mat=data.matrix(mat)

train=mat[1:nrow(features_set),]
test=mat[(nrow(features_set)+1):nrow(mat),]