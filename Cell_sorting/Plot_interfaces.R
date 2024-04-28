suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(ggpubr)

ssize <- 150

df <- fread("data/res_log_interfaces.txt")
df$V2 <- df$V2 - 500
df1_full <- df[df$V1 == "C1",]
df2_full <- df[df$V1 == "C2",]
df3_full <- df[df$V1 == "C3",]
df <- df[df$V2 %% ssize == 0,]
df1 <- df[df$V1 == "C1",]
df2 <- df[df$V1 == "C2",]
df3 <- df[df$V1 == "C3",]

end1 <- matrix(c("C1", (max(df$V2) + ssize), 0),1,3)
end2 <- matrix(c("C2", (max(df$V2) + ssize), 0),1,3)
end3 <- matrix(c("C3", (max(df$V2) + ssize), 0),1,3)

df1 <- rbind(df1, end1)
df2 <- rbind(df2, end2)
df3 <- rbind(df3, end3)

df <- rbind(df1, df2, df3)
df$V2 <- as.numeric(df$V2)
df$V3 <- as.numeric(df$V3)

df1 <- df1[1:min(which(df1$V3 == 0)),]
df1$V2 <- as.numeric(df1$V2)
df1$V3 <- as.numeric(df1$V3)
df2 <- df2[1:min(which(df2$V3 == 0)),]
df2$V2 <- as.numeric(df2$V2)
df2$V3 <- as.numeric(df2$V3)
df3 <- df3[1:min(which(df3$V3 == 0)),]
df3$V2 <- as.numeric(df3$V2)
df3$V3 <- as.numeric(df3$V3)

png(filename = "plots/Final_plot_interfaces_time.png", width = 1200, height = 700, res = 180)
ggplot() +
  geom_line(data = df1, aes(V2, V3, color = V1)) +
  geom_line(data = df2, aes(V2, V3, color = V1)) +
  # geom_line(data = df3, aes(V2, V3, color = V1)) +
  geom_point(aes(x = df2_full[min(which(df2_full$V3 == 0)),V2], y = 0), 
             color = "black", shape = 18, size = 5, alpha = 1) +
  geom_point(aes(x = df1_full[min(which(df1_full$V3 == 0)),V2], y = 0), 
             color = "#e77e72", shape = 18, size = 5, alpha = 1) +
  # geom_point(aes(x = df3_full[min(which(df3_full$V3 == 0)),V2], y = 0), 
  #            color = "#6f9af8", shape = 18, size = 5, alpha = 1) +
  theme_bw() +
  labs(y = "Number of green-blue interfaces",
       x = "Time (MCSs)",
       color = "Neighborhood") +
  scale_color_manual(labels = c("N2",
                                  expression(paste("N6"^"*"))),
                      values = c("#e77e72", "black")) +
  theme(legend.text.align = 0)
dev.off()
