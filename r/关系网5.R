#模块化指标Q modularity
modularity(g.undir, membership = c(1,1,1,2,2,2,2))

plot.membership <- function(graph, membership, main = ""){
  V(graph) $ member <- membership
  mem.col <- rainbow(length(unique(membership)), alpha = 0.3)
  V(graph) $ color <- mem.col[membership]
  plot(graph, edge.width = E(graph)$ weights, vertex.color =   V(graph) $ color,main = main)
}

par(mfcol = c(1,3))
plot.membership(g.undir,clusters(g.undir)$ membership)
plot.membership(g.dir,clusters(g.dir, mode = "weak")$ membership)
plot.membership(g.dir,clusters(g.dir,mode = "strong")$ membership)

no.clusters(g.dir, mode = "strong")

#随机游走社群

member <- walktrap.community(g.undir,steps = 4)
V(g.undir) $ member <- member$ membership
mem.col <- rainbow(3, alpha = 0.3)
V(g.undir) $ color <- mem.col[member$ membership]
member.num <- length(table(V(g.undir) $ member))
member.list <- list()
for(i in  1:member.num){
  member.list <- c(member.list,  list(which(V(g.undir) $ member==i)))
}
plot(g.undir, vertex.color= V(g.undir) $ color, mark.groups = member.list)










