init.data <- read.csv(file = paste("/Users/xu/Desktop/1.csv", sep = ""), header = F)

data <- data.frame(from = init.data[,2], to = init.data[,1])
g <- init.igraph(data, dir = F,rem.multi = T)
svg(filename = paste("/Users/xu/Desktop/1.svg",width=200,height= 200))
plot(g, vertex.size = 1, layout= layout.fruchterman.reingold, vertex.label = NA)
dev.off()
