# Unveiling new data on fish parasite diversity in reservoirs of the Brazilian semi-arid
# Written by Vitoria Lima, 2024 
# Thesis chapter 1 

#### Collector curve by host species

# Set the directory
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/Thesis/Chapter 1 - Parasites of Paraiba")

# Install packages
library(vegan); library(dplyr); library(iNEXT); library(ggplot2); library(patchwork); library (tidyr); library (tibble);

# Using iNEXT
# Upload data for taxa
peld_parasites_taxa <- read.csv("peld_parasites_taxa.csv", header = TRUE, sep = ";") 

# Convert to long format and sum abundances
peld_parasites_ab <- peld_parasites_taxa %>%
  group_by(species) %>% 
  summarise(across(everything(), sum))

# Transpose rows ans columns
peld_parasites_transposed <- peld_parasites_ab %>%
  column_to_rownames("species") %>% 
  t() %>% 
  as.data.frame()

# Applying iNEXT function to create rarefaction
rarefaction_q0 <- iNEXT(peld_parasites_transposed, q = 0, datatype = "abundance")
rarefaction_q1 <- iNEXT(peld_parasites_transposed, q = 1, datatype = "abundance")
rarefaction_q2 <- iNEXT(peld_parasites_transposed, q = 2, datatype = "abundance")

# Plot q0
ggiNEXT(rarefaction_q0, type = 1) +
  theme(panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.text = element_text(face = "italic", size = 9),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(2, "lines")) +
    labs(y = "Parasites diversity",
         x = "Abundance of parasite/host") 

# Plot q1
ggiNEXT(rarefaction_q1, type = 1) +
  theme(panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.text = element_text(face = "italic", size = 9),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(2, "lines")) +
    labs(y = "Parasites diversity",
       x = "Abundance of parasite/host") 

# Plot q2
ggiNEXT(rarefaction_q2, type = 1) +
  theme(panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.text = element_text(face = "italic", size = 9),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(2, "lines")) +
    labs(y = "Parasites diversity",
       x = "Abundance of parasite/host") 
