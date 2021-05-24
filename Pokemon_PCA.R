# Set directory
# setwd("~/GitHub/Pokemon_PCA")

# Read Pokemon list
PKM_values_7 <- readLines("Pokemon_list_g7.txt", encoding = "UTF-8")

# To simplify the analysis
# Remove Pokemons with special form, like mega, Alolan...
library(tidyverse)
PKM_without_spf <- str_count(PKM_values_7, "\\|") == 8 # Pokemons without specail form
PKM_values_7_without_spf <- PKM_values_7[PKM_without_spf]
PKM_values_7_without_spf_split <- str_split(PKM_values_7_without_spf, "\\|")

# Make a Pokemon table
PKM_values_7_without_spf_df <- data.frame(
  Number = unlist(PKM_values_7_without_spf_split)[seq(2, 792*9, 9)],
  Name = unlist(PKM_values_7_without_spf_split)[seq(3, 792*9, 9)],
  generation = c(rep(1, 151-0), rep(2, 251-151), rep(3, 385-251), rep(4, 490-385), rep(5, 640-490), rep(6, 707-640), rep(7, 792-707)),
  HP = unlist(PKM_values_7_without_spf_split)[seq(4, 792*9, 9)] %>% as.character() %>% as.numeric(),
  ATK = unlist(PKM_values_7_without_spf_split)[seq(5, 792*9, 9)] %>% as.character() %>% as.numeric(),
  DEF = unlist(PKM_values_7_without_spf_split)[seq(6, 792*9, 9)] %>% as.character() %>% as.numeric(),
  SATK = unlist(PKM_values_7_without_spf_split)[seq(7, 792*9, 9)] %>% as.character() %>% as.numeric(),
  SDEF = unlist(PKM_values_7_without_spf_split)[seq(8, 792*9, 9)] %>% as.character() %>% as.numeric(),
  SPEED = unlist(PKM_values_7_without_spf_split)[seq(9, 792*9, 9)] %>% str_replace_all("\\}","") %>% as.character() %>% as.numeric()
)

# Address types table
PKM_types_7 <- readLines("Pokemon_types.txt")
PKM_types_7_number <- c()
for (i in 1:876) {
  PKM_types_7_number[i] <- str_split(PKM_types_7, "\\|")[[i]][[3]]
}

# Remove Pokemon number containing letters
position_without_letters <- str_detect(PKM_types_7_number, "^[0-9]*$")
PKM_types_7_number_without_letters <- PKM_types_7_number[position_without_letters]


PKM_types_7_name <- c()
for (i in 1:876) {
  PKM_types_7_name[i] <- str_split(PKM_types_7, "\\|")[[i]][[4]]
}

PKM_types_7_types1 <- c()
for (i in 1:876) {
  PKM_types_7_types1[i] <- str_split(PKM_types_7, "\\|")[[i]][[6]] %>% str_remove_all("\\}")
}

# Make Pokemon types 1 table
PKM_types_7_df <- data.frame(
  Number = PKM_types_7_number,
  Name = PKM_types_7_name,
  types1 = PKM_types_7_types1
)

# Remove number with letters
PKM_types_7_df_t <- filter(PKM_types_7_df, Number %in% PKM_types_7_number_without_letters)


# Merge Pokemon table and types table by number
PKM_merged_df <- merge(PKM_types_7_df_t, PKM_values_7_without_spf_df, by = "Number")
# Remove Chinese names
PKM_merged_df <- PKM_merged_df[,-4]
colnames(PKM_merged_df)[2] <- "Name"

# Check the correlation between the variables
library(reshape2)
head(melt(cor(PKM_merged_df[, 5:ncol(PKM_merged_df)])))

# Plot heatmap
ggplot(melt(cor(PKM_merged_df[, 5:ncol(PKM_merged_df)])),
       aes(Var1, Var2)) +
  geom_tile(aes(fill = value), colour = "white") +
  scale_fill_gradient2(low = "firebrick4", high = "steelblue",
                       mid = "white", midpoint = 0) +
  guides(fill=guide_legend(title="Correlation")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title = element_blank())

# PCA
pca.model <- prcomp(PKM_merged_df[, 5:ncol(PKM_merged_df)], T)

# Show pca summary
summary(pca.model)
pca.model$rotation

# Make a pca table to plot
p1_p2_table <- pca.model$x[,1:2] %>% as.data.frame()
rownames(p1_p2_table) <- PKM_merged_df[,1]
legend_number <- c(144:146, 150:151, 
                   243:245, 249:251,
                   377:386,
                   479:494,
                   638:649,
                   716:721,
                   785:809
)
legend <- rep(F, nrow(p1_p2_table))
legend_position <- which(PKM_merged_df[, "Number"] %in% legend_number)
legend[legend_position] <- rep(T, length(legend_position))

p1_p2_table_t <- cbind(p1_p2_table, 
                     Number = PKM_merged_df[, "Number"],
                     Name = PKM_merged_df[, "Name"],
                     generation = PKM_merged_df["generation"],
                     types1 = PKM_merged_df[, "types1"],
                     legend = legend
)

p1_p2_table_t[, "generation"] <- as.character(p1_p2_table_t[, "generation"])

# Visualization
pca_gg <- ggplot(data = p1_p2_table_t, aes(x=PC1, y=PC2, label = Number))+
  geom_point(size = 2.5)
# label with generation
pca_gg_generation <- ggplot(data = p1_p2_table_t, aes(x=PC1, y=PC2, label = Number, color = generation))+
  geom_point(size = 2.5)
# label with first types
pca_gg_types1 <- ggplot(data = p1_p2_table_t, aes(x=PC1, y=PC2, label = Number, color = types1))+
  geom_point(size = 2.5)
# label with legend Pokemon
pca_gg_legend <- ggplot(data = p1_p2_table_var, aes(x=PC1, y=PC2, label = Number, color = legend))+
  geom_point(size = 2.5)

# User-interactive visualization
library(plotly)
pca_ggly <- ggplotly(pca_gg)
