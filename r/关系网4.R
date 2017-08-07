#点度中心度
    #无向图   点度中心度=点度数，与该点相连的点总数
        #点的度数等于该点相邻点数
length(neighbors(g.undir, v= which(V(g.undir)$label == "d")))
        #使用igraph包中degree函数计算 方式一  设置参数v的方式
degree(g.undir, v= which(V(g.undir)$label == "d"))
        #使用igraph包中degree函数计算 方式二  向量的方式
degree(g.undir) [which(V(g.undir)$label == "d")]
degree(g.undir)   #degree(g.undir)以向量的形式返回关系网g.undir所有的点度数

    #有向图，点度中心度=内点度中心度+外点度中心度，内点度中心度=点入度，外点度中心度=点出度
        #可以设置mode参数指定degree计算方式
        #使用igraph包中degree函数计算 方式一  设置参数v的方式
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="out") 
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="in") 
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="total") 
        #使用igraph包中degree函数计算 方式二  向量的方式  
degree(g.dir,mode="out") [ which(V(g.dir)$label == "d")]
degree(g.dir,mode="in") [ which(V(g.dir)$label == "d")]
degree(g.dir,mode="total") [ which(V(g.dir)$label == "d")]


#相对点度中心度  设置degree函数的normailized参数为TRUE计算
    #无向图   相对点度中心度=点度数/(n-1)
degree(g.undir, normalized = T) [which(V(g.undir)$label == "d")]
    #有向图，相对点度中心度=(内点度中心度+外点度中心度)/(2n-2)
      #相对内点度中心度=点入度/(n-1)，相对外点度中心度=点出度/(n-1)
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="out", normalized = T) 
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="in", normalized = T) 
degree(g.dir, v= which(V(g.dir)$label == "d"), mode="total", normalized = T) 



#接近中心度closeness 点的核心地位
    #无向图
closeness(g.undir)
closeness(g.undir, vids = which(V(g.undir)$ label == "c" ))
    #有向图 
closeness(g.dir, vids = which(V(g.dir)$ label == "c" ) , mode = "out")

#相对接近中心度
    #无向图
closeness(g.undir,normalized = T)
closeness(g.undir, vids = which(V(g.undir)$ label == "c" ), normalized = T)
    #有向图 
closeness(g.dir, vids = which(V(g.dir)$ label == "c" ) , mode = "out", normalized = T)






#中间中心度

#点的中间中心度
g <- graph.tree(40, 3, mode = "undirected")
member <- spinglass.community(g, spins = 3)
V(g)$ member<- member$ membership
mem.col <- rainbow(3, alpha =  0.3)
V(g)$ color <- mem.col[member$ membership]
V(g)[1]$ color <-"red"
plot(g, vertex.color= V(g)$ color)

g <- g.undir- V(g.undir)[c(which(V(g.undir)$ label=="f"),which(V(g.undir)$ label=="g"))]
plot(g)

    #用betweenness计算点的中间中心度， 有向图设置mode参数计算方式
betweenness(g, v = V(g)[which(V(g)$ label=="c")])

#点的相对中间中心度 的normailized参数为TRUE计算
betweenness(g, v = V(g)[which(V(g)$ label=="c")], normalized = T)

#线中间中心度
    #计算b--c的中间中心度
edge.betweenness(g, e=E(g, path =c(which(V(g.undir)$ label=="b"),
                                   which(V(g.undir)$ label=="c"))))


#特征向量中心度
    evcent(g.undir, scale = F)$ vector#无向图
    evcent(g.undir)$ vector#有向图
#page.rank中心度    
page.rank(g.undir)$ vector









