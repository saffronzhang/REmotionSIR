#自编译函数init.igraph
#这个函数有这么几个参数：
#data,是两列关系数据，前面已经讲过了，只能两列，而且要同等长度；
#dir，逻辑值，T代表有向图，F无向图；
#rem.multi，逻辑，T删除重复变量并更新线权重weight，F不删除并且线权重为1。
#使用方法直接init.igraph(data，dir=T,rem.multi=T)即可。
init.igraph<-function(data,dir=F,rem.multi=T){
  labels<-union(unique(data[,1]),unique(data[,2]))
  ids<-1:length(labels);names(ids)<-labels
  from<-as.character(data[,1]);to<-as.character(data[,2])
  edges<-matrix(c(ids[from],ids[to]),nc=2)
  g<-graph.empty(directed = dir)
  g<-add.vertices(g,length(labels))
  V(g)$label=labels
  g<-add.edges(g,t(edges))
  if (rem.multi){
    E(g)$weight<-count.multiple(g)
    g<-simplify(g,remove.multiple = TRUE,
                remove.loops = TRUE,edge.attr.comb = "mean")
  }
  g
}

#http://blog.csdn.net/sinat_26917383/article/details/51436643

#文本型数据
adjm <- matrix(sample(0:1, 100, replace=TRUE, prob=c(0.9,0.1)), nc=10)
g1 <- graph_from_adjacency_matrix( adjm ,weighted=TRUE,mode="undirected")

## 给稀疏矩阵行列进行命名

rownames(adjm) <- sample(letters, nrow(adjm))
colnames(adjm) <- seq(ncol(adjm))
g10 <- graph_from_adjacency_matrix(adjm, weighted=TRUE, add.rownames="row",add.colnames="col")

#一些基本操作
g<-graph.empty(directed=F)

add.vertices(g,length(labels))#关系网络中加入“点”
g<-g-V(g)[degree(g)==0] 

neighbors(g.zn,v=which(V(g.zn)$label=="会计"))
V(g.zn)$label[neighbors(g.zn,v=which(V(g.zn)$label=="取向"),mode="total")]
#默认mode设置是out，还有 in,total；其中V(g.zn)$label和V(g.zn)一个返回标签，一个返回值

which_loop(g)  #线是否能够指回自己，1-1就是指回自己
which_multiple(g)  #是否有重复线，后面1-1与前面1-1重复了

g<-set_vertex_attr(g,"name",value=V(g)$label)
temp<-E(g)[order(E(g)$weight>25000)]  

g<-add.edges(g,t(edges))  #edges需要先转置
g<-g-E(g)[(weight>1)]  #删除部分线

#系网络的重复性问题
h <- graph( c(1,2,1,2,3,3,2,1,4,4) );h   
is_simple(h)
simplify(h, remove.loops=FALSE)   #线重复，删除a->b,a->b删掉
simplify(h, remove.loops=TRUE)    #在线方向性重复基础上删掉点重复，a->a,b->b
simplify(h, remove.multiple=FALSE)#删掉点重复
simplify(h, remove.multiple=TRUE)#删掉点重复同时，删除线a->b,a->b
simplify(h, remove.multiple=TRUE,remove.loops=TRUE) #删掉线重复、点重复



#http://blog.csdn.net/sinat_26917383/article/details/51443846

#中心度
degree(g,mode="in")      #mode=in点入度；out=点出度；total点度中心度，三者统称绝对点中心度
degree(g,normalized = T) #相对点中心度=绝对点中心度/最大度数（可以作为不同网络结构的比较，相对数与绝对数的区别）
degree.distribution(g)  


closeness(g,vids=which(V(g)$label=="c"))  #某点四周的接近中心度，mode="out"是有向图，默认是无向图为in
#设置normalized = T为相对接近中心度

betweenness(g,normalized = T)
#normalized = T代表相对数，默认值为F为绝对值
#mode有Out和in分别代表有向和无向


edge.betweenness(g)
#normalized = T代表相对数，默认值为F为绝对值
#mode有Out和in分别代表有向和无向

evcent(g,scale = F)$vector  #系统不论是否重要，都会计算所有点四周的相邻矩阵，根据矩阵求得点的特征向量，耗时巨大
#scale=F没有归一化，T代表输出数据进行标准化
#mode有Out和in分别代表有向和无向

page.rank(g,scale=F)$vector
#scale=F没有归一化，T代表输出数据进行标准化
#mode有Out和in分别代表有向和无向

transitivity(g)


graph.density(g.zn)
graph.density(group1)
graph.density(group2)
#从中可以看到不同社群与整体之间的网络密度情况（关联程度）


#http://blog.csdn.net/sinat_26917383/article/details/51444536

clusters(g.dir,mode="weak")

member<-walktrap.community(g.undir,weights=E(g)$weight,step=4)

member<-spinglass.community(g.undir,weights=E(g.undir)$weight,spins=2)  
#需要设置参数weights，因为无默认值  


member<-edge.betweenness.community(g.undir,weight=E(g)$weight,directed=F)  

#社群发现方法五：标签传播社群发现  
member<-label.propagation.community(g.undir,weights=V(g.undir)$weight)  
V(g.undir)$member  
member<-label.propagation.community(g.undir,weights = E(g.undir)$weight,initial = c(1,1,-1,-1,2,-1,1))  
V(g.undir)$member  
member<-label.propagation.community(g.undir,weights = E(g.undir)$weight,  
                                    initial = c(1,1,-1,-1,2,-1,1),fixed=c(T,F,F,F,F,F,T))  


modularity(g.undir,membership=c(1,1,1,2,2,2,2))  #社群总差异，membership设置社群号  

transitivity(g)


graph.density(g.zn)  
graph.density(group1)  
graph.density(group2)  
#从中可以看到不同社群与整体之间的网络密度情况（关联程度）  


plot(g.test,layout=layout.fruchterman.reingold,edge.arrow.size=0.1,vertex.color=rainbow(7,alpha=0.3),edge.arrow.mode = "-")  



#vertex.size=1表示节点的大小  
#layout表示布局方式（发散性）  
#vertex.label=NA,不显示任何点信息，默认显示idx号  
#vertex.color=V(g)$color 点的颜色设置  
#mark.groups表示设置分组  
#vertex.shape='none'不带边框   
#vertex.label.cex=1.5, #节点字体大小  
#vertex.label.color='red'  
#edge.arrow.size=0.7 #连线的箭头的大小  
#edge.color = grey(0.5)#线的颜色  
#edge.arrow.mode = "-" 箭头换成线  
#vertex.label.dist=5  点标签和节点之间的距离一般0.1，便于错开重叠 



plot.membership<-function(graph,membership,main=""){  
  V(graph)$member<-membership  
  mem.col<-rainbow(length(unique(membership)),alpha=0.3)  
  V(graph)$color<-mem.col[membership]  
  plot(graph,edge.width=E(graph)$weight,vertex.color=V(graph)$color,main=main)  
}  


plot.membership(g.undir,clusters(g.undir)$membership,"无向图的社群发现")  



V(gg)$size = 5  
V(gg)[degree(g)>=3000]$size = 15  


mem.col<-rainbow(length(unique(V(g)$member)),alpha = 0.3)  
V(g)$color<-mem.col[V(g)$member]  



V(g)$label=NA  
V(g)[degree(g)>=3000]$label=V(gg)[degree(g)>=3000]$name 


svg(filename=paste("C:/Users/long/Desktop","/1.svg",sep = ""),width = 40,height = 40)  
#plot(data.g,layout=layout.fruchterman.reingold,vertex.color=V(g)$color,vertex.label=V(g)$label,<span style="font-family: Arial, Helvetica, sans-serif;">vertex.size=V(g)$size<span>)  
dev.off() 









