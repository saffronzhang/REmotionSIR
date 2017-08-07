
#情感网络构建

#载入原始数据
#网络关系关注属性输入
from <- c("a","a","e","b","b","c","d","d","d","f")
#网络关系被关注属性输入
to <- c("c","e","c","e","c","d","g","g","f","d")
#建立关系数据
data <- data.frame(from = from, to = to)

#去重，元素集合重复元素合并
labels <- union(unique(data[,1]), unique(data[,2]))
#元素集合输出
labels

#建立向量名
ids <- 1:length(labels)
#向量命名
names(ids) <- labels
#输出向量
ids

#数据框data转化为edges矩阵
from <- as.character(data[,1])
to <- as.character(data[,2])
#转置矩阵生成
edges <- matrix(c(ids[from], ids[to]), nc = 2)

#加载igraph包
library("igraph")

#建立一个空的关系网络
library("igraph")
#网络方向为F，即无方向网络
g <- graph.empty(directed = F)

#添加点
g <- add.vertices(g, length(labels))
#为每个点添加名称信息
V(g)$label = labels 

#向关系网g中添加线edges信息
#t(edges) #矩阵转置
g <- add.edges(g, t(edges)) 

#合并重复线，把重复的次数作为线权重weigth
E(g) $weight <- count.multiple(g)
E(g) $weight

#添加情感属性 
V(g)$emotion = c("neutral")
V(g)$emotion



#构造情感网络参数函数
init.igraph <- function(data, dir = F, rem.multi = T){
  #去重
  labels <- union(unique(data[,1]), unique(data[,2]))
  #建立向量名
  ids <- 1:length(labels)
  #向量命名
  names(ids) <- labels
  #数据框转化为矩阵
  from <- as.character(data[,1])
  to <- as.character(data[,2])
  #转置矩阵生成
  edges <- matrix(c(ids[from], ids[to]), nc = 2)
  #设置网络方向性参数
  g <- graph.empty(directed = dir)
  #添加点
  g <- add.vertices(g, length(labels))
  V(g)$label = labels
  #添加情感属性
  V(g)$emotion = c("neutral")
  #合并重复线
  g <- add.edges(g, t(edges)) 
  if(rem.multi){
    E(g) $weight <- count.multiple(g)
    g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
  }
  g
}



#随机游走的社群发现

#根据数据读取生成基础情感网络
g <- init.igraph(data, dir =F)
#网络展示布局参数设置
par(mfcol = c(1,1))
#输出基础情感网络
plot(g,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g))




#设置随机游走的社群属性
member<- walktrap.community(g)
#把社群属性加载给网络特征
V(g)$member<- member$membership
member$membership

#获取分类社群数量
member.num<- length(table(V(g)$member))
#数量展示
member.num
#定义不同颜色给不同社群
mem.col<- rainbow(3,alpha = 0.3)
V(g)$color<- mem.col[member$membership]

#设置一个临时社群链表参数
member.list<- list()
for(i in 1:member.num){
  member.list<- c(member.list,list(which(V(g)$member==i)))
}
#划分好社群展示 
plot(g, vertex.color=V(g)$color, mark.groups = member.list, vertex.label= NA ,vertex.size =5 ,edge.color = grey(0.1))

#效果展示排布
par(mfcol = c(1,member.num))

#由社群不同生成的子图展示
for(i in 1:member.num){
  h<- induced_subgraph(g,V(g)$member==i)
  
  #展示
  plot(h,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(h))
}

#效果展示排布
par(mfcol = c(1,1))

#第一子图
g1<- induced_subgraph(g,V(g)$member==1)

plot(g1,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g1))

#用于产生随机数的，程序涉及到了模拟或仿真的算法，42为编号 
set.seed(42)
g1.ba <- barabasi.game(10,directed = F, start.graph = g1)

#第一子图增长完成 
plot(g1.ba,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g1.ba))

#第二子图
g2<- induced_subgraph(g,V(g)$member==2)

label2 <- V(g)$label[which(V(g)$member==2)]

V(g2)$label = label2 

plot(g2,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g2))

set.seed(42)
g2.ba <- barabasi.game(10,directed = F, start.graph = g2)

plot(g2.ba,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g2.ba))


#图形合并
gn <- graph.union(g1.ba,g2.ba)

#完成增长效果
set.seed(50)
gn <- barabasi.game(20,directed = F, start.graph = g)
plot(gn,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(gn))












