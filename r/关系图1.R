#载入原始数据
from <- c("a","a","e","b","b","c","d","d","d","f")
to <- c("c","e","c","e","c","d","g","g","f","d")
data <- data.frame(from = from, to = to)

#去重，元素集合
labels <- union(unique(data[,1]), unique(data[,2]))
labels

#建立向量名
ids <- 1:length(labels)
names(ids) <- labels
ids

#数据框data转化为edges矩阵
from <- as.character(data[,1])
to <- as.character(data[,2])
edges <- matrix(c(ids[from], ids[to]), nc = 2)

#建立一个空的关系网络
library("igraph")
g <- graph.empty(directed = F)

#添加点
g <- add.vertices(g, length(labels))
V(g)$label = labels #为每个点添加名称信息

#向关系网g中添加线edges信息
#t(edges) #矩阵转置
g <- add.edges(g, t(edges)) 

#合并重复线，把重复的次数作为线权重weigth
E(g) $weight <- count.multiple(g)
E(g) $weight

g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
E(g) $weight                  