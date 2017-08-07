library(igraph)
library(plyr)

g<-erdos.renyi.game(100,0.4,directed=T)#生成ER随机图表

g
degree<-degree(g,mode="all",normalized=T)#mode=in点入度;out=点出度;total点度中心度，三者统称绝对点中心度,相对点中心度=绝对点中心度/最大度数
plot(table(degree),type="h")#绘制直方图


g<-erdos.renyi.game(100,0.4,directed=T)#生成ER随机图表
m<-gsize(g)#获取边数
m
n<-vcount(g)#获取顶点数
n
l<-mean_distance(g)##计算平均路径长度
l
c<-transitivity(g)#计算聚类系数
degree<-degree(g,mode="all",normalized=T)#mode=in点入度;out=点出度;total点度中心度，三者统称绝对点中心度,相对点中心度=绝对点中心度/最大度数
table(degree)#度统计
plot(table(degree),type="h")#绘制直方图
degree.distribution(g)#查看度分布
closeness(g,mode="in")##计算接近中心度，点与其他点距离之和的倒数
order(closeness(g,mode="in"))#排序
betweenness(g,normalized=T)#查看点的中间中心度,代表最短距离是否经过该点
edge.betweenness(g)#查看线的中间中心度
evcent(g,scale = F)$vector#计算点的特征向量中心度
page.rank(g)$vector#计算邻接矩阵,计算点的特征向量中心度



##无障碍##
g1<-graph.lattice(c(6,6,1),directed=T,mutual = T)#生成一个6*6的矩阵
V(g1)$name<-c(11,12,13,14,15,16,21,22,23,24,25,26,31,32,33,34,35,36,41,42,43,44,45,46,51,52,53,54,55,56,61,62,63,64,65,66)#设置顶点名称
V(g1)$color<-"pink"#设置顶点颜色
V(g1)$size<-12#设置顶点大小
pa<-get.all.shortest.paths(g1,which(V(g1)$name==11),which(V(g1)$name==66))$res[[1]]##最短路径算法
V(g1)[pa]$color<-"green"
#E(g1)$color<-"grey"
E(g1,path=pa)$color<-"red"#设置边颜色
plot(g1,layout=layout.grid)

##随机生成一个图
random<-erdos.renyi.game(50,0.2,directed=T)
#gnm<-sample_gnm(10,20,directed = F)#基于gnm生成图
#gnp<-sample_gnp(10,0.3,directed = F)#基于gnp生成图
V(random)$color<-"pink"#设置顶点颜色
V(random)$size<-10#设置顶点大小
E(random)$color<-"grey"#设置边颜色
pa<-get.all.shortest.paths(random,1,20)$res[[1]]##最短路径算法
E(random,path=pa)$color<-"red"#设置路径边的颜色
plot(random,layout=layout.fruchterman.reingold)
svg(filename = paste("/Users/xu/Desktop/1.svg",width=200,height= 200))
plot(random,vertex.size = 1,layout=layout.fruchterman.reingold)
dev.off()
