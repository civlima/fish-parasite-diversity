# New records of fish parasites in the reservoirs of Paraiba state
# Written by Vitoria Lima, 2024 
# Thesis chapter 1 

#### Bipartite network

# Set the directory
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/Thesis/Chapter 1 - Parasites of Paraiba")

# Install packages
library(tidyverse); library(bipartite);

# Upload data for taxa
peld_parasites_taxa <- read.csv("peld_parasites_taxa.csv", header = TRUE, sep = ";") 

# Sum abundances within hosts 
peld_parasites <- aggregate(.~species, data = peld_parasites_taxa, FUN = sum)

# Extract species names
hosts_names <- unique(peld_parasites$species)

# Convert to abundance matrix
abundance_matrix <- as.matrix(peld_parasites[, -1])

# Reagreggate the host names
rownames(abundance_matrix) <- hosts_names

# Plot the network
plotweb(abundance_matrix, method = "cca",
        col.interaction = "grey",
        col.high = "darkblue",
        col.low = "darkgreen",
        text.rot = 90,
        y.lim=c(-1,2.5),
        arrow = "down",
        high.spacing = 0.05,
        low.spacing = 0.182)
