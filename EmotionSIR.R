

#感染参数初始化
a <- 0.3
b <- 0.1
y <- 0.05

#传播迭代次数
times <- 40

#初始感染密度
i0 <- 0.02
r0 <- 0


#传播模型方程仿真函数构建
#设置参数
sir.test<-function(a, b, y, i0, r0, times){
  #迭代次数
  t <- seq(0, times, 1)
  len <- length(t)
  s <- numeric(len)
  i <- numeric(len)
  r <- numeric(len)
  
  for(ti in (1:(len-1)) ){
    
    s[1] <- (1- i0- r0)
    i[1] <- i0
    r[1] <- r0
    #方程实现
    s[ti+1] <- (s[ti]-(a+b)*i[ti]*s[ti] - y*r[ti]*s[ti])
    i[ti+1] <- (a*i[ti]*s[ti]+i[ti])
    r[ti+1] <- (b*i[ti]*s[ti] + y*r[ti]*s[ti] +r[ti])
    
  }
  
  #图形绘制
  plot(t, ylim = c(0, 1.2), xlab = "times", ylab = "density")
  lines(t, s, ylim = c(0, 1.2), lwd =3, col= "blue")
  lines(t, i, ylim = c(0, 1.2), lwd =3,  col= "red")
  lines(t, r, ylim = c(0, 1.2), lwd =3,  col= "green")
  
}

#方法实现
sir.test(a, b, y, i0, r0, times)



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


#数据读取
root="/Users/xu/Desktop/shiyan /r/"
init1.data<-read.csv(file = paste(root, "11111 3.csv", sep= ""), header = F)
init2.data<-read.csv(file = paste(root, "11111 2.csv", sep= ""), header = F)
init3.data<-read.csv(file = paste(root, "11111.csv", sep= ""), header = F)
data1 <- data.frame(from = init1.data[ , 2], to = init1.data [ ,1])
data2 <- data.frame(from = init2.data[ , 2], to = init2.data [ ,1])
data3 <- data.frame(from = init3.data[ , 2], to = init3.data [ ,1])

#随机游走的社群发现
#根据数据读取生成基础情感网络
g1 <- init.igraph(data1, dir =F)
g2 <- init.igraph(data2, dir =F)
g3 <- init.igraph(data3, dir =F)
#网络展示布局参数设置
par(mfcol = c(1,1))
#输出基础情感网络
plot(g1,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g1))


#设置随机游走的社群属性
member<- walktrap.community(g1)
#把社群属性加载给网络特征
V(g1)$member<- member$membership
member$membership

#获取分类社群数量
member.num<- length(table(V(g1)$member))
#数量展示
member.num
#定义不同颜色给不同社群
mem.col<- rainbow(3,alpha = 0.3)
V(g1)$color<- mem.col[member$membership]

#设置一个临时社群链表参数
member.list<- list()
for(i in 1:member.num){
  member.list<- c(member.list,list(which(V(g1)$member==i)))
}
#划分好社群展示 
plot(g1, vertex.color=V(g1)$color, mark.groups = member.list, vertex.label= NA ,vertex.size =5 ,edge.color = grey(0.1))

#效果展示排布
par(mfcol = c(1,1))

#完成增长效果
plot(g2,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.fruchterman.reingold(g2))


plot(g3,vertex.size =5 ,vertex.label= NA, edge.color = grey(0.1),layout = layout.kamada.kawai(g3))






