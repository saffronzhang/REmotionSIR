#�ȸ�������
#�ǵ�ÿ�����е�ʱ����Ҫ��ʼ��
rm(list=ls())
library("igraph")
b=matrix(0,nrow=3,ncol=3)
b[2,3]=1
b[1,2]=1
b[1,3]=1
b
xulie=c(1,2, 1,3, 2,3)
#u <- graph(xulie, directed=F)
#plot(u, layout=layout.fruchterman.reingold)

for(i in 3 :100){
b=rbind(cbind(b,rep(0,i)),rep(0,i+1))###�����ϵ��һ����i  һ����i+1
x=sample(1:i,1)
b[x,i+1]=1#  j�����鵽�ĸ���  ����4��2 ��������
#��һ���ǲ���Ϊ�Գƾ���
for(k in 1:i) {
for(m in (k+1):(i+1)){
b[m,k]=b[k,m]
}
}
#which(a[,i+1]!=0)#��������Ҫʹ��
xulie=c(xulie,i+1,which(b[,i+1]!=0))
u <- graph(xulie, directed=F)
#plot(u, layout=layout.fruchterman.reingold)
}
plot(u, layout=layout.fruchterman.reingold)
degree(u)
#����������������������������������������������������������������������������������������
#���沿���ǹ����ʼ���������
max(degree(u))#�����Ϊ
MaxPoint<-which(degree(u)==max(degree(u)))
#��ͬ�ڵ�������Ⱦ�ڵ㣩
NumV=length(which(b[MaxPoint,]!=0))
#���� �ڵ�Ϊ
NumH=100-length(which(b[MaxPoint,]!=0))
��Ч��������=x/y=NumV/NumH
x<-0.00869
y=0.2
for(cishu in 1:10){
#ͨ���������Ƿ���3�Ķ������3Ϊ���ȵĽڵ�
MaxPoint<-which(degree(u)==max(degree(u)))
#�鿴���Ƚڵ����Щ�ڵ����� ����ͬ�ڵ������
NumV=length(which(b[MaxPoint,]!=0))
#���㽡���ڵ������
NumH=100-length(which(b[MaxPoint,]!=0))
#���������ڵ���x���������Ƚڵ����ӣ���ͬ�ڵ���y�����������ӡ�
#��101���ڵ�  ��ʱ9���Ǹ�Ⱦ�ڵ� 91��Ϊ�����ڵ�
Numganran=floor(NumH*x)#Ϊ��Ⱦ������  ����Ϊ10
Numkangfu=floor(NumV*y)#Ϊ����������   ����Ϊ4
total=c(1:101)[-MaxPoint]
#����ط�Ӧ���´���
#��ȡ��Ⱦ�������ڽ����������
ganranxulieid=which(b[MaxPoint,]!=0)
jiankangxulieid=which(b[MaxPoint,]==0)
#��ȡ�������� ��Ҫ�ڸ�Ⱦ�������
sum1=sample(total[-ganranxulieid],Numganran)#Ҫ���ȡһ���� ��Ϊ�ܿ��ܳ鵽3  �����Ǹ�Ⱦ����
sum2=sample(total[-jiankangxulieid],Numkangfu)#���һ���� ��ʾ��������
for(p in 1:Numganran){
b[MaxPoint,sum1[p]]<-1
b[sum1[p],MaxPoint]<-1
}
for(q in 1:Numkangfu){
b[MaxPoint,sum2[q]]<-0
b[sum2[q],MaxPoint]<-0
}
}#��forѭ����Ӧ
#View(b)
#���沿��ֻ�Ǿ�����ʮ�ε��ݻ��������ǻ�ͼ
#��һ������δӾ�����ȡ�������� ��ס���ʱ���ǶԳƾ���

#���ɷ���
b=b[-101,]
#���������Ǿ���
for(i in 1:100 ){
for(j in 1:100){
if(i>=j){
b[i,j]=0
}else{
b[i,j]=b[i,j]
}
}
}

#����
xulie2=c(which(b!=0)[1]%%100,round(which(b!=0)[1]/100,0)+1)
for(po in 2:length(which(b!=0))){
xulie2=c(xulie2,which(b!=0)[po]%%100,round(which(b!=0)[po]/100,0)+1)
#����
#round(which(b!=0)[3]/100,0)+1
}
#��������ͼ
u <- graph(xulie2, directed=F)
plot(u, layout=layout.fruchterman.reingold)
degree(u)
#�ʼ��ʱ��u�ǶԳƾ��� �����ڼ���degree��ʱ��᲻����������Ǹ�����