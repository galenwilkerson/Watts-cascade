# author:  Galen J. Wilkerson
# gjwilkerson@gmail.com
# July 23, 2017
##################################################################

library(shiny)
library(igraph)
library(ggplot2)

# min and max number of nodes
min_N = 1
max_N = 50
init_N = 10
init_p_val = 0.5
init_n_seeds = 5
init_min_phi = 0.0
init_max_phi = 1.0
