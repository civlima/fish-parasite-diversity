# Unveiling new data on fish parasite diversity in reservoirs of the Brazilian semi-arid
# Written by Vitoria Lima, 2024 
# Thesis chapter 1 

#### Bipartite network

# Set the directory
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/Thesis/Chapter 1 - Parasites of Paraiba")

# Load the previous script
source("Scripts/2_Relative abundance plot.R")

# Install packages
library(bipartite);

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
