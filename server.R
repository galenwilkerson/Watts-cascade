

# author:  Galen J. Wilkerson
# gjwilkerson@gmail.com
# July 23, 2017
##################################################################

# function to initialize network
initNetwork <- function(N = 30, p = .5, n_seeds = 2, minPhi = 0.0, maxPhi = 1.0) {
  
  G = igraph::erdos.renyi.game(N, p, directed = FALSE)
  
  # set graph attributes: labels and phi values
  
  # the labels for the cascade
  # set all nodes to 0
  V(G)$label = 0
  
  # choose some random start nodes from 0 to N - 1
  seed_nodes <- sample(1:(N), n_seeds, replace=F)
  
  # set seed nodes to 1    
  for(node in seed_nodes) {
    V(G)$label[node] <- 1
  }
  
  # the phi parameter for each node (for now set to slider value)
  V(G)$phi = 0
  
  # use a random value
  for (node in as_ids(V(G))) {
    V(G)$phi[node] = runif(n = 1, min = minPhi, max = maxPhi)
  }
  
  G = updateColor(G, "red", "grey", "green")
  
  return(G)
}

# function to randomly cascade one node
cascadeOneNode <- function(G, nodeID) {
  neighbornodes = neighbors(G, v = nodeID)
  num_neighbors = length(neighbornodes)
  
  if (num_neighbors == 0) {
    return(G)  
  }
  
  num_labeled_neighbors = length(neighbornodes[neighbornodes$label == 1])
  
  fractionLabeled = num_labeled_neighbors/num_neighbors
  
  if (fractionLabeled >= V(G)[nodeID]$phi) {
    V(G)[nodeID]$label = 1
  }
  G = updateColor(G, "red", "grey", "green")
  return(G)
}

# function to test cascade many times
runCascade <- function(G, maxNumSteps) {
  unlabeled_nodes = V(G)[V(G)$label <= 0]
  
  num_steps = 0
  while(length(unlabeled_nodes > 0) && num_steps < maxNumSteps) {
    nodeID = sample(as_ids(unlabeled_nodes), 1)
    G = cascadeOneNode(G, nodeID)
    num_steps = num_steps + 1
  }
  
  return(G)
}


# determine if a node is vulnerable
isVulnerable <- function(G, nodeID) {
  neighbornodes = neighbors(G, v = nodeID)
  num_neighbors = length(neighbornodes)
  
  if (num_neighbors == 0) {
    return(FALSE)  
  }
  
  num_labeled_neighbors = length(neighbornodes[neighbornodes$label == 1])
  
  fractionLabeled = num_labeled_neighbors/num_neighbors
  
  if (fractionLabeled >= V(G)[nodeID]$phi) {
    return(TRUE)
  }
  return(FALSE)
}


# find the vulnerable nodes for next cascade step
findVulnerableNodes <- function(G) {
  # find the unlabeled nodes
  # for each unlabeled node, determine if it is vulnerable
  # add to list, return
  
  unlabeled_nodes = as_ids(V(G)[V(G)$label <= 0])
  
  vulnerableNodes = c()
  
  for (nodeID in unlabeled_nodes) {
    if (isVulnerable(G, nodeID)) {
      vulnerableNodes = c(vulnerableNodes, nodeID)
    }
  }

  return(vulnerableNodes)
}

# return the number of vulnerable nodes
numVulnerableNodes <- function(G) {
  return(length(findVulnerableNodes(G)))
}


# function to update colors based on labels
# and create english label names (unlabeled, labeled, vulnerable)
updateColor <- function(G, labelColor, neutralColor, vulnerableColor = NULL) {
  # V(G)$color=V(G)$label #assign the "label" attribute as the vertex color
  # V(G)$color=gsub("1", labelColor, V(G)$color) #seeds will be red
  # V(G)$color=gsub("0", neutralColor, V(G)$color) #non-seeds will be grey

  V(G)[V(G)$label == 1]$color = labelColor #labeled nodes will be red
  V(G)[V(G)$label == 0]$color = neutralColor #non-labeled nodes will be grey
  
  # color vulnerable nodes
  if (! is.null(vulnerableColor)) {
    vulnerableNodes = findVulnerableNodes(G)
    print(vulnerableNodes)
    
    # set the label and color of vulnerable nodes
    V(G)[vulnerableNodes]$label = -1
    V(G)[vulnerableNodes]$color = vulnerableColor
    
  }
  
  V(G)[V(G)$label == 1]$label_name = "labeled"
  V(G)[V(G)$label == 0]$label_name = "unlabeled"
  V(G)[V(G)$label == -1]$label_name = "vulnerable"
  
  
  return(G)
}

# draw a graph
drawNetwork <- function(G, layout = NULL) {
  N = vcount(G)  
  plot(G, vertex.size = 10*(1 - N/501), edge.color = "lightgrey", vertex.label = V(G), vertex.label.dist = 2, layout = layout)
}

# draw a graph using ggplot2
drawNetworkGGplot <- function(G, layout = NULL) {
  
  #get the node coordinates
  if (is.null(layout)) {
    plotcord <- data.frame(layout_with_fr(G))
  }
  else {
    plotcord <- data.frame(layout)
  }
  
  colnames(plotcord) = c("X1","X2")
  
  #get edges, which are pairs of node IDs
  edgelist <- get.edgelist(G)
  
  #convert to a four column edge data frame with source and destination coordinates
  edges <- data.frame(plotcord[edgelist[,1],], plotcord[edgelist[,2],])
  colnames(edges) <- c("X1","Y1","X2","Y2")
  
  ggplot() + geom_segment(aes(x=X1, y=Y1, xend = X2, yend = Y2), data=edges, size = 0.5, colour="grey") + geom_point(aes(X1, X2), data=plotcord)
}

# return the fraction of nodes labeled in the network
cascadeFraction <- function(G) {
  labeled_nodes = V(G)[V(G)$label == 1]
  endCascadeSize = length(labeled_nodes)
  return (endCascadeSize/vcount(G))
}

##################################################################

# # min and max number of nodes
# min_N = 1
# max_N = 50
# init_N = 10
# init_p_val = 0.5
# init_n_seeds = 5
# init_min_phi = 0.0
# init_max_phi = 1.0

# init network in global scope for observers later
G <- initNetwork(N = init_N, p = init_p_val, n_seeds = init_n_seeds, minPhi = init_min_phi, maxPhi = init_max_phi)
layout <- layout_with_fr(G)

server <- function(input, output, session) {
  
  # keep track of inserted UI elements
  numInserted <- 0
  
  # update the number of seeds so that the max value is N
  observe(
    updateSliderInput(session, 'n_seeds', min = 1, max = input$N)
  )
  
  # create the Graph object in response to the redraw button and slider parameters
  # save the layout when G changes
  observeEvent(input$redraw, {
   
    G <<- initNetwork(N = input$N, p = input$p, n_seeds = input$n_seeds, minPhi = input$phi[1], maxPhi = input$phi[2])
    if (input$layout_choice == "force-directed") {
      layout <<- layout_with_fr(G)
    }
    else if (input$layout_choice == "grid") {
      layout <<- layout_on_grid(G) 
    }
    else {
      layout <<- layout_randomly(G)
    }
    
  })
  
  cascadeSizes = c()
  # draw the network
  output$plot <- renderPlot({
    
    # based on run_cascade toggle, use the old layout, renew G by running cascade one step
    if (! is.null(input$run_cascade) && input$run_cascade %% 2 == 1) { 
      G <<- runCascade(G, maxNumSteps = 1) # run asynchronous cascade, randomly picking one node

      # control the speed, stop when no more nodes are vulnerable
      if (numVulnerableNodes(G) > 0) {
        invalidateLater(isolate(input$speed))
      }
      
      cascadeSizes <<- c(cascadeSizes, cascadeFraction(G))
    }
    else{  # we are re-drawing G, getting new layout
      
      input$redraw
      cascadeSizes <<- c()
    }
    
    # layout shape - not needed for single plots
    par(mfrow=c(1,1))
    par(pin=c(4, 4))
    
    drawNetwork(G, layout)
    
    legend("bottomleft", inset=c(-0.27, -0.17), legend = unique(V(G)$label_name), col=unique(V(G)$color), pch=19, bty = "n")
    
    # 
    # # degree hist, kernel density plot
    # degree_dist <- density(degree(G)) # returns the density data
    # plot(degree_dist , xlab = "degree", ylab = "density") # plots the results 
    # abline(v = mean(degree(G)), untf = FALSE, col = 'red')
    # 
    # plot(density(V(G)$phi), xlab = "phi", ylab = "density")
    # abline(v = mean(V(G)$phi), untf = FALSE, col = 'red')
    # 
    # plot(cluster.distribution(G), xlab = "cluster size", ylab = "density")
    # 
    # if(length(cascadeSizes > 0)) {
    #   plot(cascadeSizes, xlab = "timestep", ylab = "cascade fraction", ylim = c(0,1), type = "line")
    # }
    
    #print(findVulnerableNodes(G))
  })

}


