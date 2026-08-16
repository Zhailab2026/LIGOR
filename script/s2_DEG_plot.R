setwd("../script/")
library(dplyr)
library(ggplot2)
library(ggrepel)

deg_results <- read.csv("../data/cohort1_DEG.csv",header = T)
deg_results <- deg_results %>%
  mutate(Significance = case_when(
    adj.P.Val < 0.05 & logFC > 1 ~ "UP Significant",
    adj.P.Val < 0.05 & logFC < -1 ~ "DOWN Significant",
    TRUE ~ "Not Significant"
  ))

top_up1 <- deg_results %>%
  filter(Significance == "UP Significant", logFC > 0) %>%
  arrange(desc(logFC)) %>%
  head(10)
top_up2 <- deg_results %>%
  filter(Significance == "UP Significant", logFC > 0) %>%
  arrange(adj.P.Val) %>%
  head(2)
top_down1 <- deg_results %>%
  filter(Significance == "DOWN Significant", logFC < 0) %>%
  arrange(logFC) %>%
  head(10)
top_down2 <- deg_results %>%
  filter(Significance == "DOWN Significant", logFC < 0) %>%
  arrange(adj.P.Val) %>%
  head(2)

top_genes <- rbind(top_up1, top_up2,top_down1,top_down2)

df_significant <- deg_results %>% 
  dplyr::filter(Significance != "Not Significant")

df_not_significant_sampled <- deg_results %>% 
  dplyr::filter(Significance == "Not Significant") %>% 
  slice_sample(prop = 0.05)

deg_results <- bind_rows(df_significant, df_not_significant_sampled)

# plot
ggplot(deg_results, aes(x = logFC, y = -log10(adj.P.Val))) +
  geom_point(aes(color = Significance), alpha = 0.8, size = 1) +
  scale_color_manual(values = c("Not Significant" = "grey", "UP Significant" = "#E4004B", "DOWN Significant" = "#799EFF")) +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "#00809D") +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "#00809D") +
  geom_label_repel(data = top_genes, 
                  aes(label = rownames(top_genes)),
                  size = 3,
                  box.padding = 0.1,
                  point.padding = 0.1,segment.color = "black", # 连接线颜色
                  show.legend =F) +
  theme_minimal() +
  labs(title = "Volcano Plot of Differential Gene Expression",  ###can change refer to groups features
       x = "log2(Fold Change)",
       y = "-log10(Adjusted P-value)") +
  
  # 调整图例位置
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5))
