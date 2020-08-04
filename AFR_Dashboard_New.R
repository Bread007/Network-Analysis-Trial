library(igraph)
setwd("C:/Users/t0d00dh/01_Projects/AFR_Dashboard")

nodes_fcpa <- read.csv("Nodes.csv", header=T, as.is=T) 
links_fcpa <- read.csv("edges_filt.csv", header=T, as.is=T)

net_work = function(country){
  nodes_fcpa = nodes_fcpa[nodes_fcpa$Country == country,]
  links_fcpa = links_fcpa[links_fcpa$Market ==country, ]
  
  net_fcpa <- graph_from_data_frame(d=links_fcpa, vertices=nodes_fcpa, directed=T)
  net_fcpa <- simplify(net_fcpa, remove.multiple = F, remove.loops = T)
  
  V(net_fcpa)$size <- V(net_fcpa)$freq*0.2
  E(net_fcpa)$width <- E(net_fcpa)$weight/8
  E(net_fcpa)$arrow.size <- .2
  
  return(net_fcpa)
}


plot_list = list()
pdf("Text_Network.pdf")

layouts = c("layout_as_star","layout_on_grid")

k = 0

for (i in 1:length(unique(nodes_fcpa$Country))){
  
  country = unique(nodes_fcpa$Country)
  net_fcpa = net_work(country[i])
  
  for(j in 1:length(layouts)){
    k=k+1
    l = do.call(layouts[j], list(net_fcpa))
    p = plot(net_fcpa, edge.arrow.mode=0, layout=l, main=country,
             vertex.label=V(net_fcpa)$word,
             vertex.label.dist=3,
             vertex.color="gray70",
             vertex.frame.color="gray70",
             vertex.label.font=3, 
             vertex.label.color="gray20", 
             vertex.label.cex=1.2, 
             edge.color="tomato")
    
    plot_list[[k]] = p
    print(plot_list[[k]])
    
  }
  
}
  

