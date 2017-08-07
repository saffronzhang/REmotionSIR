#点集合Vertexs和点属性数据
V(g.undir) #点集合

length(V(g.undir))#点长度 

V(g.undir)$label #点属性数据

    #可以为V(g.undir)中添加属性数据，例如每组分配颜色
member <- spinglass.community(g.undir)#spinglass.community发现社区函数
V(g.undir) $ membership <- member $ membership
V(g.undir) $ membership
mem.col <- rainbow(length(unique(V(g.undir) $ membership)), alpha = 0.3)
V(g.undir) $ color <- mem.col[V(g.undir) $ membership]
V(g.undir) $ color
plot(g.undir, edge.width = E(g.undir)$weight, vertex.color = V(g.undir) $ color)


#点筛选和删除

    #读取点信息
V(g.undir) [1:3]
V(g.undir)$label [1:3]

    #通过which函数筛选特定点数据
V(g.undir) [which(V(g.undir) $ membership == 1)] #简写 V(g.undir) [membership == 1)]
V(g.undir)$label [which(V(g.undir) $ membership == 1)] #简写 V(g.undir) [membership == 1)] $label

    #使用g - V(g)[i]删除部分点
g.undir <- g.undir - V(g.undir) [degree(g.undir) == 0] #删除孤立点

g.undir <- g.undir - V(g.undir) [membership == 1] #删除第一组数据所有点


#相邻点的集合
    #无向图g.undir
V(g.undir) [neighbors(g.undir, v= which(V(g.undir)$label == "d"))] #查询d相邻的点
V(g.undir)$label [neighbors(g.undir, v= which(V(g.undir)$label == "d"))]

    #有向图g.dir ,可以设置mode参数指定neighbors计算方式
        #mode参数out , d的相邻点以d为起点
V(g.dir)$label [neighbors(g.dir, v= which(V(g.dir)$label == "d"), mode="out")]
        #mode参数in , d的相邻点以d为接收点
V(g.dir)$label [neighbors(g.dir, v= which(V(g.dir)$label == "d"), mode="in")]
        #mode参数total , d的相邻点以d为起点接收点总和
V(g.dir)$label [neighbors(g.dir, v= which(V(g.dir)$label == "d"), mode="total")]


#点的度数
    #无向图g.undir
        #点的度数等于该点相邻点数
length(neighbors(g.undir, v= which(V(g.undir)$label == "d")))
        #使用igraph包中degree函数计算 方式一  设置参数v的方式
degree(g.undir, v= which(V(g.undir)$label == "d"))
        #使用igraph包中degree函数计算 方式二  向量的方式
degree(g.undir) [which(V(g.undir)$label == "d")]
degree(g.undir)   #degree(g.undir)以向量的形式返回关系网g.undir所有的点度数

    #有向图g.dir ,可以设置mode参数指定degree计算方式
        #使用igraph包中degree函数计算 方式一  设置参数v的方式
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="out") 
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="in") 
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="total") 
        #使用igraph包中degree函数计算 方式二  向量的方式  
degree(g.dir,mode="out") [ which(V(g.dir)$label == "d")]
degree(g.dir,mode="in") [ which(V(g.dir)$label == "d")]
degree(g.dir,mode="total") [ which(V(g.dir)$label == "d")]


#线集合和线的属性数据
    #E(g)可以返回关系网g所有线集合E(edges),并通过length直接返回线数目
E(g.undir)
length(E(g.undir))
E(g.dir)
length(E(g.dir))
        #线权重属性weight
E(g.undir) $ weight <- count.multiple(g.undir)


#线的筛选和删除
E(g.undir) [(weight > 1)] #读取无向图线权重大于1的线
g.undir <- g.undir - E(g.undir) [(weight > 1)] #使用g - V(g)[i]删除部分线


#两点之间最短路径和距离
    #使用igraph包中get.shortest.paths函数读取两个点最短路径
        #from读取点与其他点最近路径
get.shortest.paths(g.undir, from = V(g.undir)[label == "a"])
        #from到to读取点与另一点最近路径
get.shortest.paths(g.undir, from = V(g.undir)[label == "a"], 
                   to = V(g.undir)[label == "b"])
        #有向图mode计算方式
get.shortest.paths(g.dir, from = V(g.dir)[label == "c"],
                   to = V(g.dir)[label == "g"], mode = "in")
get.shortest.paths(g.dir, from = V(g.dir)[label == "c"],
                   to = V(g.dir)[label == "g"], mode = "out")
get.shortest.paths(g.dir, from = V(g.dir)[label == "c"],
                   to = V(g.dir)[label == "g"], mode = "all")

    #使用igraph包中shortest.paths函数读取两个点间距离即最短长度
        #读取所有点与其他点最近长度
shortest.paths(g.undir)
        #设置参数v，读取某点与其他点最近长度
shortest.paths(g.undir, v =  V(g.undir)[label == "a"])
        #设置参数to，读取某点与另一点最近长度
shortest.paths(g.undir, v =  V(g.undir)[label == "a"], to = V(g.undir)[label == "b"])
        #有向图mode读取某点与另一点最近长度 设置参数mode = "in/out/all"

    #画出无向图c到g之间最短路径，并绘制红线加粗，且路径所经过点设置为绿色
pa <- get.shortest.paths(g.undir, from = V(g.undir)[label == "c"], 
                         to = V(g.undir)[label == "g"])[[1]]
pa
E(g.undir)$ color<- 'black'
#E(g.undir, path = pa)
E(g.undir, path = pa)$color<- 'red'
E(g.undir, path = pa)$width<- 3
V(g.undir)[pa]$color<- 'green'
plot(g.undir)


#关系网络筛选
    #使用induced.subgraph函数通过点筛选关系网络
tmp.v <- c((which(V(g.undir) $ label == "c")), 
             (which(V(g.undir) $ label == "d")),
             (which(V(g.undir) $ label == "g")),
             (which(V(g.undir) $ label == "f")))#预筛选点的idx号
g.undir.1 <- induced.subgraph(g.undir, tmp.v)
#等同于g.undir.1 <- induced.subgraph(g.undir,V(g.undir)[tmp.v])

    #使用函数通过关系筛选关系网络
pa <- get.shortest.paths(g.undir, from = which(V(g.undir)$label == "c"),
                         to = which(V(g.undir)$label == "g"))[[1]]
g.undir.2 <- subgraph.edges(g.undir, E(g.undir, path = pa))
g.undir.3 <- subgraph.edges(g.undir, which(E(g.undir)$ weight>1))

    #展示筛选结果
par(mfcol = c(2,2))
plot(g.undir, edge.width = E(g.undir)$weight, main = "g.undir的关系网络图")
plot(g.undir.1, edge.width = E(g.undir.1)$weight, main = "g.undir中筛选出法f,d,g,c")
plot(g.undir.2, edge.width = E(g.undir.2)$weight, main = "g.undir中筛选点c和g之间连线")
plot(g.undir.3, edge.width = E(g.undir.3)$weight, main = "g.undir中筛选权重大于1的连线")










