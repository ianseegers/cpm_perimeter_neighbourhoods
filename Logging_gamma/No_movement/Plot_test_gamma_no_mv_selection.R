suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(tidyr)
library(ggpubr)
library(grid)

df <- fread("data/gammalog_v350_no_mv.txt")
cols <- c("MCS", "Gamma")
colnames(df) <- cols

df <- df[df$MCS > 499,]
df$asp <- "4.22"

df1 <- fread("data/gammalog_v500_no_mv.txt")
cols <- c("MCS", "Gamma")
colnames(df1) <- cols

df1 <- df1[df1$MCS > 499,]
df1$asp <- "3.53"

df2 <- fread("data/gammalog_v650_no_mv.txt")
cols <- c("MCS", "Gamma")
colnames(df2) <- cols

df2 <- df2[df2$MCS > 499,]
df2$asp <- "3.10"

df3 <- fread("data/gammalog_v800_no_mv.txt")
cols <- c("MCS", "Gamma")
colnames(df3) <- cols

df3 <- df3[df3$MCS > 499,]
df3$asp <- "2.79"

df_f <- rbind(df,df1,df2,df3)
df_f$asp <- as.character(df_f$asp)
df_f$asp <- factor(df_f$asp, levels = c("4.22", "3.53", "3.10", "2.79"))

png(filename = "plots/Final_plot_gamma_no_mv_selection.png", width = 750, height = 750, res = 200)
ggplot(df_f, aes(as.factor(asp), (Gamma/(3/18)))) + 
  geom_violin(fill = "blue", alpha = 0.5) + 
  labs(y = expression(paste(gamma,"(t)/",gamma^"*")),
       x = "Aspherity") +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 16))
dev.off()