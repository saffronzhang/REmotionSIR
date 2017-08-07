from <- c("a","a","e","b","b","c","d","g","d","c")
to <- c("c","e","c","e","c","d","g","d","f","e")
data <- data.frame(from = from, to = to)

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

par(mfcol = c(1,2))
g.undir <- init.igraph(data)
plot(g.undir,layout = layout.fruchterman.reingold(g.undir), edge.width =8, vertex.size =30)
g.dir <- init.igraph(data, dir = T)
plot(g.dir, layout = layout.fruchterman.reingold(g.dir),edge.width =8,  vertex.size =30, edge.arrow.size=1)



member<- walktrap.community(g.undir)
V(g.undir)$member<- member$membership
mem.col<- rainbow(3,alpha = 0.3)
V(g.undir)$color<- mem.col[member$membership]
member.num<- length(table(V(g.undir)$member))
member.list<- list()
for(i in 1:member.num){
  member.list<- c(member.list,list(which(V(g.undir)$member==i)))
}
plot(g.undir, vertex.color=V(g.undir)$color, mark.groups = member.list )




