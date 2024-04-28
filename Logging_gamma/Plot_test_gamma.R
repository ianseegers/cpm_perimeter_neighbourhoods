suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(tidyr)
library(ggpubr)

df1 <- fread("data/gammalog_nomv_plot.txt")
cols <- c("MCS", "Gamma")
colnames(df1) <- cols

df1 <- df1[df1$MCS > 499,]
df1$m <- 0

df2 <- fread("data/gammalog_amoeb_plot.txt")
cols <- c("MCS", "Gamma")
colnames(df2) <- cols

df2 <- df2[df2$MCS > 499,]
df2$m <- 1

df3 <- fread("data/gammalog_intermediate_plot.txt")
cols <- c("MCS", "Gamma")
colnames(df3) <- cols

df3 <- df3[df3$MCS > 499,]
df3$m <- 2

df4 <- fread("data/gammalog_kera_plot.txt")
cols <- c("MCS", "Gamma")
colnames(df4) <- cols

df4 <- df4[df4$MCS > 499,]
df4$m <- 3

df_f <- rbind(df1, df2, df3, df4)

png(filename = "plots/Final_plot_gamma_migration.png", width = 900, height = 900, res = 220)
ggplot() + 
  geom_hline(yintercept = 1, color = "black", linetype = "solid", linewidth = 0.7) +
  geom_violin(data = df_f[df_f$m == 0,], aes(as.factor(m), (Gamma/(3/18))), 
              fill = "lightgrey", alpha = 1) + 
  geom_segment(aes(x = 0.75, xend = 1.25, 
                   y = median((df_f[df_f$m == 0,]$Gamma/(3/18))), 
                   yend = median((df_f[df_f$m == 0,]$Gamma/(3/18)))),
                   linewidth = 1.3, 
               color = "#00578e") +
  geom_violin(data = df_f[df_f$m == 1,], aes(as.factor(m), (Gamma/(3/18))), 
              fill = "lightgrey", alpha = 1) + 
  geom_segment(aes(x = 1.75, xend = 2.25, 
                   y = median((df_f[df_f$m == 1,]$Gamma/(3/18))), 
                   yend = median((df_f[df_f$m == 1,]$Gamma/(3/18)))),
                   linewidth = 1.3, 
               color = "#e8d0cc") +
  geom_violin(data = df_f[df_f$m == 2,], aes(as.factor(m), (Gamma/(3/18))), 
              fill = "lightgrey", alpha = 1) +
  geom_segment(aes(x = 2.75, xend = 3.25, 
                   y = median((df_f[df_f$m == 2,]$Gamma/(3/18))), 
                   yend = median((df_f[df_f$m == 2,]$Gamma/(3/18)))),
                   linewidth = 1.3, 
               color = "#eededc") +
  geom_violin(data = df_f[df_f$m == 3,], aes(as.factor(m), (Gamma/(3/18))), 
              fill = "lightgrey", alpha = 1) + 
  geom_segment(aes(x = 3.75, xend = 4.25, 
                   y = median((df_f[df_f$m == 3,]$Gamma/(3/18))), 
                   yend = median((df_f[df_f$m == 3,]$Gamma/(3/18)))),
                   linewidth = 1.3, 
               color = "#c16f59") +
  scale_x_discrete(labels = c("No movement", 
                              "Amoeboid", 
                              "Intermediate", 
                              "Keratocyte-like")) +
  labs(y = expression(paste(gamma,"(t)/",gamma^"*")),
       x = "Movement type") +
  theme_bw() +
  theme(axis.title.y = element_text(size = 15),
        axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "none")
dev.off()

