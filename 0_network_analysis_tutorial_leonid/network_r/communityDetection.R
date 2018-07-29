library('igraph')
library("doParallel")
library("foreach")

## load karate club data
setwd("C:/Users/chuang/Desktop/network_r")
g = graph.famous("Zachary")
plot(g)

## find clique with largest possible size
maximal.cliques(g, min = 4, max = 5) # maximal cliques of sizes 4 and 5

largest = largest.cliques(g) ## largest number of cliques
labels = rep(0, vcount(g))   ## generate a list with all 0s 
labels[largest[[1]]] = 2
plot(g, vertex.color = labels)


## community detection 

# edge.betweenness.community [Newman and Girvan, 2004]
# fastgreedy.community Clauset et al., 2004
# label.propagation.community [Raghavan et al., 2007]
# leading.eigenvector.community [Newman, 2006]
# multilevel.community Blondel et al., 2008
# optimal.community [Brandes et al., 2008]
# spinglass.community [Reichardt and Bornholdt, 2006]
# walktrap.community [Pons and Latapy, 2005]
# infomap.community [Rosvall and Bergstrom, 2008]

##################################
## Newman-Girvan Edge-Betweenness
##################################

#The Newman-Girvan algorithm detects communities by progressively removing edges from the original network. The Girvan-Newman algorithm focuses on edges that are most likely "between" communities.
#Step 1: the betweenness of all existing edges in the network is calculated first.
#Step 2: the edge with the highest betweenness is removed.
#Step 3: the betweenness of all edges affected by the removal is recalculated.
#Step 4: steps 2 and 3 are repeated until no edges remain.
#The best partition is selected based on modularity.

betw <- edge.betweenness(g)
eb <- edge.betweenness.community(g)
plot(eb, g)

## A bit more hand-made way
# color_map = c("grey","blue","black","yellow","red","green")
# membership = cutat(eb, no = 4)
# membership = eb$membership
# plot(g, vertex.color = eb$membership)
dendPlot(eb, mode="hclust", rect = 5)


##################################
## Greedy Modularity maximization
##################################
mm <- fastgreedy.community(g)
plot(rev(mm$modularity), xlab = 'Number of clusters', ylab = 'Modularity value')
which.max(rev(mm$modularity))      ## the best number of clusters
plot(mm, g)

##################################
## label propagation
##################################

#Label propagation algorithm consists of four steps:
  
## Step 1: Initialize labels
## Step 2: Randomize node ordering
## Step 3: For every node replace its label with occurring with the highest frequency among neighbors
## Step 4: Repeat steps 2-3 until every node will have a label that the maximum number of its neighbors have

lp <- label.propagation.community(g)
plot(lp, g)

####################
## overlapping community using clique percolation 
###################
clique.community.opt.par <- function(graph, k){
  if(!require(foreach)){
    install.packages("foreach")
    library(foreach)
  }
  clq <- cliques(graph, min=k, max=k)
  #find edges between cliques
  edges <- c()
  edges <- foreach (i=1:(length(clq)-1), .combine=c) %dopar% 
  {
    tmp_edg <- c()
    for (j in (i+1):length(clq)) {
      if ( length(unique(c(clq[[i]], clq[[j]]))) == k+1 ) {
        tmp_edg <- c(tmp_edg, c(i,j))
      }
    }
    return(tmp_edg)
  }
  #Create an empty graph and then adding edges
  clq.graph <- make_empty_graph(n = length(clq)) %>% add_edges(edges)
  clq.graph <- simplify(clq.graph)
  V(clq.graph)$name <- seq_len(vcount(clq.graph))
  comps <- decompose.graph(clq.graph)
  lapply(comps, function(x) {
    unique(unlist(clq[ V(x)$name ]))
  })
}

res<-clique.community.opt.par(g,4)
res
