  root="/Users/xu/Desktop/shiyan /r/"
  init.data<-read.csv(file = paste(root, "11111.csv", sep= ""), header = F)
  
  
  data <- data.frame(from = init.data[ , 1], to = init.data [ ,2])
  
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
    
    V(g)$emotion = c("neutral")
    
    g <- add.edges(g, t(edges)) 
    if(rem.multi){
      E(g) $weight <- count.multiple(g)
      g <- simplify(g, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = 'mean')
    }
    g
  }

  g <- init.igraph(data, dir =F)
 
  
  
  #感染参数初始化
  a <- 0.3
  b <- 0.1
  y <- 0.05
  
  #传播迭代次数
  times <- 40
  
  #初始感染密度
  i0 <- 0.02
  r0 <- 0
  

  emsir <- function(g, a, b, y, i0, r0 ,times){
    
    #参数有效性验证
    if (is.null(V(g))) {
      print("Cannot run SIR model on empty graph")
    }
    if (a < 0) {
      print("a must be non-negative in SIR model");
    }
    if (b < 0) {
      print("b must be non-negative in SIR model");
    }
    if (y < 0) {
      print("y must be non-negative in SIR model");
    }
    if (times <= 0) {
      print("Number of SIR simulations must be positive");
    }
    
    par(mfcol = c(2,2))
    
    V(g)$emotion = c("neutral")
    
    V(g)$emotion
    
    V(g)$emotion[which(V(g)$label== "40")]= c("positive")
    V(g)$emotion[which(V(g)$label== "88")]= c("positive")
    V(g)$emotion[which(V(g)$label== "145")]= c("positive")
    V(g)$emotion[which(V(g)$label== "210")]= c("positive")
    
    v.cols <-character(75)
    v.cols[V(g)$emotion== "neutral"] <- "yellow"
    v.cols[V(g)$emotion== "positive"] <- "green"
    v.cols[V(g)$emotion== "negitive"] <- "black"
    plot(g , layout = layout.kamada.kawai(g) , edge.color = grey(0.1), vertex.size =6 ,vertex.label= NA, vertex.color=v.cols)
    
    
    for(i in 1:5){
      V(g)$emotion[which((V(g)$emotion[neighbors(g, v= (which(V(g)$emotion == "positive")))]) == "neutral")] = c("positive")
      
    }
    
    V(g)$label[which(V(g)$emotion == "positive")]
    V(g)$emotion[which(V(g)$label== "90")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "130")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "221")]= c("negitive")
    
    v.cols <-character(75)
    v.cols[V(g)$emotion== "neutral"] <- "yellow"
    v.cols[V(g)$emotion== "positive"] <- "green"
    v.cols[V(g)$emotion== "negitive"] <- "black"
    plot(g , layout = layout.kamada.kawai(g) , edge.color = grey(0.1), vertex.size =6 ,vertex.label= NA, vertex.color=v.cols)
    
    
    V(g)$emotion = c("positive")
    
    V(g)$emotion[which(V(g)$label== "6")]= c("neutral")
    V(g)$emotion[which(V(g)$label== "135")]= c("neutral")
    V(g)$emotion[which(V(g)$label== "185")]= c("neutral")
    V(g)$emotion[which(V(g)$label== "230")]= c("neutral")
    V(g)$emotion[which(V(g)$label== "90")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "130")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "221")]= c("negitive")
    
    
    for(i in 1:5){
      V(g)$emotion[which((V(g)$emotion[neighbors(g, v= (which(V(g)$emotion == "neutral")))]) == "positive")] = c("neutral")
      V(g)$emotion[which((V(g)$emotion[neighbors(g, v= (which(V(g)$emotion == "negitive")))]) == "positive")] = c("negitive")
    }
    
    v.cols <-character(75)
    v.cols[V(g)$emotion== "neutral"] <- "yellow"
    v.cols[V(g)$emotion== "positive"] <- "green"
    v.cols[V(g)$emotion== "negitive"] <- "black"
    plot(g , layout = layout.kamada.kawai(g) , edge.color = grey(0.1), vertex.size =6 ,vertex.label= NA, vertex.color=v.cols)
    
    V(g)$emotion = c("positive")
    
    V(g)$emotion[which(V(g)$label== "90")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "130")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "221")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "30")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "155")]= c("negitive")
    V(g)$emotion[which(V(g)$label== "186")]= c("negitive")
    
    for(i in 1:5){
      V(g)$emotion[which((V(g)$emotion[neighbors(g, v= (which(V(g)$emotion == "negitive")))]) == "positive")] = c("negitive")
    }
    
    v.cols <-character(75)
    v.cols[V(g)$emotion== "neutral"] <- "yellow"
    v.cols[V(g)$emotion== "positive"] <- "green"
    v.cols[V(g)$emotion== "negitive"] <- "black"
    plot(g , layout = layout.kamada.kawai(g) , edge.color = grey(0.1), vertex.size =6 ,vertex.label= NA, vertex.color=v.cols)
    
  }
  
  
  
  emsir(g, a, b, y, i0, r0, times)
  
  
  
  
  
  
  
  
  
 