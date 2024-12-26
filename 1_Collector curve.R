# New records of fish parasites in the reservoirs of Paraiba state
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
  theme(
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.text = element_text(face = "italic", size = 9),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(2, "lines")) +
    labs(y = "Parasites diversity",
         x = "Abundance of parasite/host") 

# Plot q1
ggiNEXT(rarefaction_q1, type = 1) +
  theme(
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.text = element_text(face = "italic", size = 9),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(2, "lines")) +
  labs(y = "Parasites diversity",
       x = "Abundance of parasite/host") 

# Plot q2
ggiNEXT(rarefaction_q2, type = 1) +
  theme(
    panel.background = element_blank(), 
    panel.grid = element_blank(), 
    axis.line = element_line(color = "black"),
    legend.text = element_text(face = "italic", size = 9),
    legend.spacing.x = unit(0.5, "cm"),
    legend.key.width = unit(2, "lines")) +
  labs(y = "Parasites diversity",
       x = "Abundance of parasite/host") 

### Separate by hosts
list_species <- split(peld_parasites_taxa, peld_parasites_taxa$species)

# Remove non-interaction columns
species_list <- lapply(list_species, function(df) {
  df <- df[, colSums(df != 0) > 0]
  return(df)
})

# A. bimaculatus
a_bimaculatus <- species_list[[2]]
a_bimaculatus <- as.data.frame(a_bimaculatus)
a_bimaculatus$species <- NULL
result_q0 <- iNEXT(a_bimaculatus, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("A. bimaculatus")

# C. monoculus
c_monoculus <- species_list[[3]]
c_monoculus <- as.data.frame(c_monoculus)
c_monoculus$species <- NULL
result_q0 <- iNEXT(c_monoculus, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("C. monoculus")
  
# C. orientale
c_orientale <- species_list[[4]]
c_orientale <- as.data.frame(c_orientale)
c_orientale$species <- NULL
result_q0 <- iNEXT(c_orientale, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("C. orientale")

# G. brasiliensis
g_brasiliensis <- species_list[[5]]
g_brasiliensis <- as.data.frame(g_brasiliensis)
g_brasiliensis$species <- NULL
result_q0 <- iNEXT(g_brasiliensis, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("G. brasiliensis")

# H. marginatus
h_marginatus <- species_list[[6]]
h_marginatus <- as.data.frame(h_marginatus)
h_marginatus$species <- NULL
result_q0 <- iNEXT(h_marginatus, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("H. marginatus")

# H. malabaricus
h_malabaricus <- species_list[[7]]
h_malabaricus <- as.data.frame(h_malabaricus)
h_malabaricus$species <- NULL
result_q0 <- iNEXT(h_malabaricus, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("H. malabaricus")

# H. pusarum
h_pusarum <- species_list[[8]]
h_pusarum <- as.data.frame(h_pusarum)
h_pusarum$species <- NULL
result_q0 <- iNEXT(h_pusarum, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("H. pusarum")

# L. piau
l_piau <- species_list[[9]]
l_piau <- as.data.frame(l_piau)
l_piau$species <- NULL
result_q0 <- iNEXT(l_piau, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("L. piau")

# M. costae
m_costae <- species_list[[10]]
m_costae <- as.data.frame(m_costae)
m_costae$species <- NULL
result_q0 <- iNEXT(m_costae, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("M. costae")

# O. niloticus
o_niloticus <- species_list[[11]]
o_niloticus <- as.data.frame(o_niloticus)
o_niloticus$species <- NULL
result_q0 <- iNEXT(o_niloticus, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("O. niloticus")

# P. squamosissimus
p_squamosissimus <- species_list[[12]]
p_squamosissimus <- as.data.frame(p_squamosissimus)
p_squamosissimus$species <- NULL
result_q0 <- iNEXT(p_squamosissimus, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("P. squamosissimus")

# P. vivipara
p_vivipara <- species_list[[13]]
p_vivipara <- as.data.frame(p_vivipara)
p_vivipara$species <- NULL
result_q0 <- iNEXT(p_vivipara, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("P. vivipara")

# P. brevis
p_brevis <- species_list[[14]]
p_brevis <- as.data.frame(p_brevis)
p_brevis$species <- NULL
result_q0 <- iNEXT(p_brevis, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("P. brevis")

# S. brasiliensis
s_brasiliensis <- species_list[[15]]
s_brasiliensis <- as.data.frame(s_brasiliensis)
s_brasiliensis$species <- NULL
result_q0 <- iNEXT(s_brasiliensis, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("S. brasiliensis")


# S. notonota
s_notonota <- species_list[[16]]
s_notonota <- as.data.frame(s_notonota)
s_notonota$species <- NULL
result_q0 <- iNEXT(s_notonota, q = 0, datatype = "abundance")
ggiNEXT(result_q0) +
  theme_bw(base_size = 18) +
  ggtitle("S. notonota")