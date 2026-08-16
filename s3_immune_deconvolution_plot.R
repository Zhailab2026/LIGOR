setwd("../script/")
library(pheatmap)
library(RColorBrewer)
data <- read.csv("../data/cohort1_cibersort_cibersort.csv", header = TRUE)
data$cluster <- factor(data$cluster, levels = unique(data$cluster))
data <- data[order(data$cluster), ]
mat <- t(as.matrix(data[, 3:ncol(data)]))
colnames(mat) <- data$idd
annotation_col <- data.frame(Cluster = data$cluster)
rownames(annotation_col) <- data$idd
ann_colors <- list(Cluster = c("TME1" = "#F8766D", "TME2" = "#00BFC4", "TME3" = "#FFD966"))
gap_cols <- cumsum(rle(as.character(data$cluster))$lengths)[-length(unique(data$cluster))]
heatmap_colors <- colorRampPalette(c("#313695", "#FEF0D9", "#A50026"))(100)
my_breaks <- seq(-3, 3, length.out = 101)
pheatmap(mat, annotation_col = annotation_col, annotation_colors = ann_colors, cluster_rows = F, cluster_cols = FALSE, show_colnames = FALSE, scale = "row", 
         gaps_col = gap_cols, color = heatmap_colors, breaks = my_breaks, border_color = NA, fontsize_row = 9)