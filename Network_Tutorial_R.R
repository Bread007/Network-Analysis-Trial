
library(igraph)
getwd()
setwd("C:/Users/t0d00dh/01_Projects/Network_Analysis_Readings")


nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T) 
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

nodes2 <- read.csv("Dataset2-Media-User-Example-NODES.csv", header=T, as.is=T) 
links2 <- read.csv("Dataset2-Media-User-Example-EDGES.csv", header=T, row.names=1)

#########################################
# Turning networks into igraph objects
#########################################
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 

E(net) # The edges of the "net" object 
V(net) # The vertices of the "net" object 
E(net)$type # Edge attribute "type" 
V(net)$media # Vertex attribute "media"


#################################################################
##  Basic Setups
#################################################################

#########################
# Plot 1 ----Draft Plot
#########################
plot(net, edge.arrow.size=.4,vertex.label=NA)

# ???x plot by removing the loops in the graph *****???? Not very sure
net <- simplify(net, remove.multiple = F, remove.loops = T)

#############################
# Plot 2  
# - Add Node Names
# - Set Edge and Node Color
#############################
plot(net, edge.arrow.size=.2, edge.curved=0, 
     vertex.color="orange", vertex.frame.color="#555555", 
     vertex.label=V(net)$media, 
     vertex.label.color="black", vertex.label.cex=.7)

#############################################
# Plot 3 
# - set attributes by seting igraph objects
#############################################

# -- Generate colors based on media type: 
colrs <- c("gray50", "tomato", "gold") 
V(net)$color <- colrs[V(net)$media.type]

# -- Set node size based on audience size:
# -- in FCPA context, use the frequency of word as size
V(net)$size <- V(net)$audience.size*0.7


# -- The labels are currently node IDs. 
# -- Setting them to NA will render no labels: 
V(net)$label.color <- "black" 
V(net)$label <- NA


# -- Set edge width based on weight: 
# -- in FCPA context, use the cooccurence times of 2 words to set edge
E(net)$width <- E(net)$weight/3 # be careful about the denominator, for it's weighting the cooccurence

# -- Set arrow size and edge color: 
E(net)$arrow.size <- .2 
E(net)$edge.color <- "gray80"
#E(net)$width <- 1+E(net)$weight/12


plot(net)

# -- override the attributes explicitly in the plot:
plot(net, edge.color="orange", vertex.color="gray50")

#############################################
# Plot 4
# -- add a legend explaining the colors
#############################################

plot(net) 
legend(x=-1.5, y=-1.1, 
       c("Newspaper","Television", "Online News"), 
       pch=21, col="#777777", pt.bg=colrs, 
       pt.cex=2, cex=.8, bty="n", ncol=1)

#############################################
# Plot 5
# -- add a legend explaining the colors
#############################################
plot(net, vertex.shape="none", ### no shape
     vertex.label=V(net)$media, 
     vertex.label.font=2, 
     vertex.label.color="gray40", 
     vertex.label.cex=.7, 
     edge.color="gray85")



#############################################
# Plot 6
# -- choose the right layouts
#############################################
layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
layouts <- layouts[!grepl("bipartite|merge|norm|sugiyama|tree", layouts)]

par(mfrow=c(4,4), mar=c(1,1,1,1)) 
for (layout in layouts) { 
  print(layout)
  l <- do.call(layout, list(net)) 
  plot(net, edge.arrow.mode=0, layout=l, main=layout) }



#################################################################
##  Improving network plots
## -- to see if we can sparsify the network
## -- only keep the most important ties and discarding the rest. 
#################################################################

hist(links$weight) 
mean(links$weight) 
sd(links$weight)

#########################################################
# Plot 7
# -- keep the nodes whose weight higher than the mean
#########################################################
cut.off <- mean(links$weight)
net.sp <- delete_edges(net, E(net)[weight<cut.off])   
plot(net.sp)

#########################################################
# Plot 8
# --  plot the two tie types (hyperlink & mention)
#########################################################
net.m <- net - E(net)[E(net)$type=="hyperlink"]
net.h <- net - E(net)[E(net)$type=="mention"]
# Plot the two links separately: 
par(mfrow=c(1,2))
plot(net.h, vertex.color="orange", main="Tie: Hyperlink") 
plot(net.m, vertex.color="lightsteelblue2", main="Tie: Mention")

# Make sure the nodes stay in place in both plots:
###--- remember to add node names 
l <- layout_with_fr(net) 
plot(net.h, vertex.color="orange", layout=l, main="Tie: Hyperlink") 
plot(net.m, vertex.color="lightsteelblue2", layout=l, main="Tie: Mention")
#dev.off()


#################################################################
## Interactive plotting with tkplot 
#################################################################

#### How to add details and show the details in the interactive plot

tkid <- tkplot(net) #tkid is the id of the tkplot that will open 
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot 
tk_close(tkid, window.close = T) 
plot(net, layout=l)



#################################
# Plot  ---- Projection 1 plot
#################################

net2 <- graph_from_incidence_matrix(links2)
#  calculate the projections manually 
#### as_incidence_matrix(net2) %*% t(as_incidence_matrix(net2))

net2.bp <- bipartite.projection(net2)
net2.bp$proj1

plot(net2.bp$proj1, vertex.label.color="black", 
     vertex.label.dist=1, vertex.size=7, 
     vertex.label=nodes2$media[!is.na(nodes2$media.type)])

#################################
# Plot  ---- Projection 2 plot
#################################
plot(net2.bp$proj2, vertex.label.color="black", 
     vertex.label.dist=1, vertex.size=7, 
     vertex.label=nodes2$media[ is.na(nodes2$media.type)])
















