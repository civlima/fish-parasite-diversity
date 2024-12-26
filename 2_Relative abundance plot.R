# Unveiling new data on fish parasite diversity in reservoirs of the Brazilian semi-arid
# Written by Vitoria Lima, 2024 
# Thesis chapter 1 

#### Relation between parasites diversity and host species

# Set the directory
setwd("C:/Users/vitor/OneDrive/Papers/Lima et al/2024/Thesis/Chapter 1 - Parasites of Paraiba")

# Load the previous script
source("Scripts/1_Collector curve.R")

# Upload the file
taxa_hosts <- read.csv("PELD_Parasitos_Grandes grupos.csv", header = T, sep = ";")

# Create relative abundance
relative_ab <- taxa_hosts %>%
  pivot_longer(cols= -Hosts, names_to = "Group", values_to = "Abundance") %>%
  group_by(Hosts) %>%
  mutate(Proportion = Abundance / sum(Abundance)) %>%
  ungroup()

# Plot
ggplot(relative_ab, aes(x = Group, y = Proportion, fill = Hosts)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "", y = "", fill = "Hosts", aes(font.format = italic)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.text = element_text(face = "italic")) +
  coord_flip()
