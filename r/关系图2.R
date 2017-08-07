
#自定义的函数init.igraph用于建立关系网
init.igraph <- function(data, dir = F, rem.multi = T){
  labels <- union(unique(data[,1]), unique(data[,2]))
  ids <- 1:length(labels)
  names(ids) <- labels
  from <- as.character(data[,1])
  to <- as.character(data[,2])
  edges <- matrix(c(ids[from], ids[to]), nc = 2)
  g <- graph.empty(directed = dir)
  g <- add.vertices(g, length(labels))
  V(g)$label = labels
  g <- add.edges(g, t(edges)) 
  if(rem.multi){
    E(g) $weight <- count.multiple(g)
    g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
  }
  g
}

#使用init.igraph函数建立无向图和有向图，用plot函数
par(mfcol = c(1,2))
g.undir <- init.igraph(data)
plot(g.undir, edge.width = E(g.undir)$weight, main = "无向图g.undir", edge.label = E(g.undir)$weight)
g.dir <- init.igraph(data, dir = T)
plot(g.dir, edge.width = E(g.dir)$weight, main = "有向图g.dir", edge.label = E(g.dir)$weight)

