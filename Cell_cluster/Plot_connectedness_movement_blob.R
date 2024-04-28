suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(tidyr)
library(ggpubr)
library(grid)
library(ggbeeswarm)

df <- fread("data/res_one_ck.txt")
cols <- c("MCS", "C1", "C2", "C3", "C4", "C5", "C6", 
          "X1_1", "Y1_1", "X1_2", "Y1_2", "X1_3", "Y1_3", 
          "X2_1", "Y2_1", "X2_2", "Y2_2", "X2_3", "Y2_3", 
          "X3_1", "Y3_1", "X3_2", "Y3_2", "X3_3", "Y3_3", 
          "X4_1", "Y4_1", "X4_2", "Y4_2", "X4_3", "Y4_3", 
          "X5_1", "Y5_1", "X5_2", "Y5_2", "X5_3", "Y5_3",
          "X6_1", "Y6_1", "X6_2", "Y6_2", "X6_3", "Y6_3")
colnames(df) <- cols

df <- df[df$MCS > 499,]

df1 <- df[,c(1,2)]
colnames(df1) <- c("MCS", "C")
df1$NB <- "Second"
df1$P <- 70
df2 <- df[,c(1,3)]
colnames(df2) <- c("MCS", "C")
df2$NB <- "Sixth theory"
df2$P <- 70
df3 <- df[,c(1,4)]
colnames(df3) <- c("MCS", "C")
df3$NB <- "Sixth true"
df3$P <- 70
df4 <- df[,c(1,5)]
colnames(df4) <- c("MCS", "C")
df4$NB <- "Second"
df4$P <- 100
df5 <- df[,c(1,6)]
colnames(df5) <- c("MCS", "C")
df5$NB <- "Sixth theory"
df5$P <- 100
df6 <- df[,c(1,7)]
colnames(df6) <- c("MCS", "C")
df6$NB <- "Sixth true"
df6$P <- 100

df_C <- rbind(df1, df2, df3, df4, df5, df6)

##Without gamma eff
a <- ggplot() + geom_quasirandom(data = df_C[df_C$P == 70 & df_C$NB != "Sixth true",], 
                                 aes(NB, C), color = "black") + 
  theme_bw() +
  labs(title = expression(paste("P"[t], " = 70")),
       y = "Average connectedness",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")))) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0.4, 1))
  
b <- ggplot() + geom_quasirandom(data = df_C[df_C$P == 100 & df_C$NB != "Sixth true",], 
                                 aes(NB, C), color = "black") + 
  theme_bw() +
  labs(title = expression(paste("P"[t], " = 100")),
       y = "Average connectedness",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")))) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0.4, 1))

png(filename = "Plots_single_kind/Plot_connectedness_blob_no_gameff.png", width = 1700, height = 800, res = 220)
figure <- ggarrange(b + rremove("xlab"),
                    a + rremove("xlab") + rremove("ylab"), 
                    nrow = 1)
annotate_figure(figure, bottom = textGrob("Neighbourhood", gp = gpar(cex = 1)))
dev.off()

##Gamma eff added
a2 <- ggplot() + geom_quasirandom(data = df_C[df_C$P == 70 & df_C$NB != "Sixth true",], 
                                  aes(NB, C), color = "grey") + 
  geom_quasirandom(data = df_C[df_C$P == 70 & df_C$NB == "Sixth true",], 
                     aes(NB, C), color = "black") + 
  theme_bw() +
  labs(title = expression(paste("P"[t], " = 70")),
       y = "Average connectedness",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")),
                              expression(paste("N6"["eff"])))) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0.4, 1))

b2 <- ggplot() + geom_quasirandom(data = df_C[df_C$P == 100 & df_C$NB != "Sixth true",], 
                                  aes(NB, C), color = "grey") + 
  geom_quasirandom(data = df_C[df_C$P == 100 & df_C$NB == "Sixth true",], 
                   aes(NB, C), color = "black") + 
  theme_bw() +
  labs(title = expression(paste("P"[t], " = 100")),
       y = "Average connectedness",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")),
                              expression(paste("N6"["eff"])))) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0.4, 1))

png(filename = "Plots_single_kind/Plot_connectedness_blob.png", width = 1700, height = 800, res = 220)
figure <- ggarrange(b2 + rremove("xlab"),
                    a2 + rremove("xlab") + rremove("ylab"), 
                    nrow = 1)
annotate_figure(figure, bottom = textGrob("Neighbourhood", gp = gpar(cex = 1)))
dev.off()

rm(figure, df_C, a, b, a2, b2)

##Displacement----------------------------
df1 <- df[,c(1,8,9,10,11,12,13)]
colnames(df1) <- c("MCS", "X1", "Y1", "X2", "Y2", "X3", "Y3")
df1$NB <- "Second"
df1$P <- 70
df1 <- df1 %>% mutate(disp_x1 = X1 - lag(X1, default = first(X1)))
df1 <- df1 %>% mutate(disp_y1 = Y1 - lag(Y1, default = first(Y1)))
df1 <- df1 %>% mutate(disp1 = sqrt(disp_x1^2 + disp_y1^2))
df1 <- df1 %>% mutate(disp_x2 = X2 - lag(X2, default = first(X2)))
df1 <- df1 %>% mutate(disp_y2 = Y2 - lag(Y2, default = first(Y2)))
df1 <- df1 %>% mutate(disp2 = sqrt(disp_x2^2 + disp_y2^2))
df1 <- df1 %>% mutate(disp_x3 = X3 - lag(X3, default = first(X3)))
df1 <- df1 %>% mutate(disp_y3 = Y3 - lag(Y3, default = first(Y3)))
df1 <- df1 %>% mutate(disp3 = sqrt(disp_x3^2 + disp_y3^2))
df1 <- df1[,c(1,12,15,18,8,9)]
df1_temp1 <- df1[,c(2,5,6)]
colnames(df1_temp1) <- c("Disp", "NB", "P")
df1_temp2 <- df1[,c(3,5,6)]
colnames(df1_temp2) <- c("Disp", "NB", "P")
df1_temp3 <- df1[,c(4,5,6)]
colnames(df1_temp3) <- c("Disp", "NB", "P")
df1 <- rbind(df1_temp1, df1_temp2, df1_temp3)
rm(df1_temp1, df1_temp2, df1_temp3)

df2 <- df[,c(1,14,15,16,17,18,19)]
colnames(df2) <- c("MCS", "X1", "Y1", "X2", "Y2", "X3", "Y3")
df2$NB <- "Sixth theory"
df2$P <- 70
df2 <- df2 %>% mutate(disp_x1 = X1 - lag(X1, default = first(X1)))
df2 <- df2 %>% mutate(disp_y1 = Y1 - lag(Y1, default = first(Y1)))
df2 <- df2 %>% mutate(disp1 = sqrt(disp_x1^2 + disp_y1^2))
df2 <- df2 %>% mutate(disp_x2 = X2 - lag(X2, default = first(X2)))
df2 <- df2 %>% mutate(disp_y2 = Y2 - lag(Y2, default = first(Y2)))
df2 <- df2 %>% mutate(disp2 = sqrt(disp_x2^2 + disp_y2^2))
df2 <- df2 %>% mutate(disp_x3 = X3 - lag(X3, default = first(X3)))
df2 <- df2 %>% mutate(disp_y3 = Y3 - lag(Y3, default = first(Y3)))
df2 <- df2 %>% mutate(disp3 = sqrt(disp_x3^2 + disp_y3^2))
df2 <- df2[,c(1,12,15,18,8,9)]
df2_temp1 <- df2[,c(2,5,6)]
colnames(df2_temp1) <- c("Disp", "NB", "P")
df2_temp2 <- df2[,c(3,5,6)]
colnames(df2_temp2) <- c("Disp", "NB", "P")
df2_temp3 <- df2[,c(4,5,6)]
colnames(df2_temp3) <- c("Disp", "NB", "P")
df2 <- rbind(df2_temp1, df2_temp2, df2_temp3)
rm(df2_temp1, df2_temp2, df2_temp3)

df3 <- df[,c(1,20,21,22,23,24,25)]
colnames(df3) <- c("MCS", "X1", "Y1", "X2", "Y2", "X3", "Y3")
df3$NB <- "Sixth true"
df3$P <- 70
df3 <- df3 %>% mutate(disp_x1 = X1 - lag(X1, default = first(X1)))
df3 <- df3 %>% mutate(disp_y1 = Y1 - lag(Y1, default = first(Y1)))
df3 <- df3 %>% mutate(disp1 = sqrt(disp_x1^2 + disp_y1^2))
df3 <- df3 %>% mutate(disp_x2 = X2 - lag(X2, default = first(X2)))
df3 <- df3 %>% mutate(disp_y2 = Y2 - lag(Y2, default = first(Y2)))
df3 <- df3 %>% mutate(disp2 = sqrt(disp_x2^2 + disp_y2^2))
df3 <- df3 %>% mutate(disp_x3 = X3 - lag(X3, default = first(X3)))
df3 <- df3 %>% mutate(disp_y3 = Y3 - lag(Y3, default = first(Y3)))
df3 <- df3 %>% mutate(disp3 = sqrt(disp_x3^2 + disp_y3^2))
df3 <- df3[,c(1,12,15,18,8,9)]
df3_temp1 <- df3[,c(2,5,6)]
colnames(df3_temp1) <- c("Disp", "NB", "P")
df3_temp2 <- df3[,c(3,5,6)]
colnames(df3_temp2) <- c("Disp", "NB", "P")
df3_temp3 <- df3[,c(4,5,6)]
colnames(df3_temp3) <- c("Disp", "NB", "P")
df3 <- rbind(df3_temp1, df3_temp2, df3_temp3)
rm(df3_temp1, df3_temp2, df3_temp3)

df4 <- df[,c(1,26,27,28,29,30,31)]
colnames(df4) <- c("MCS", "X1", "Y1", "X2", "Y2", "X3", "Y3")
df4$NB <- "Second"
df4$P <- 100
df4 <- df4 %>% mutate(disp_x1 = X1 - lag(X1, default = first(X1)))
df4 <- df4 %>% mutate(disp_y1 = Y1 - lag(Y1, default = first(Y1)))
df4 <- df4 %>% mutate(disp1 = sqrt(disp_x1^2 + disp_y1^2))
df4 <- df4 %>% mutate(disp_x2 = X2 - lag(X2, default = first(X2)))
df4 <- df4 %>% mutate(disp_y2 = Y2 - lag(Y2, default = first(Y2)))
df4 <- df4 %>% mutate(disp2 = sqrt(disp_x2^2 + disp_y2^2))
df4 <- df4 %>% mutate(disp_x3 = X3 - lag(X3, default = first(X3)))
df4 <- df4 %>% mutate(disp_y3 = Y3 - lag(Y3, default = first(Y3)))
df4 <- df4 %>% mutate(disp3 = sqrt(disp_x3^2 + disp_y3^2))
df4 <- df4[,c(1,12,15,18,8,9)]
df4_temp1 <- df4[,c(2,5,6)]
colnames(df4_temp1) <- c("Disp", "NB", "P")
df4_temp2 <- df4[,c(3,5,6)]
colnames(df4_temp2) <- c("Disp", "NB", "P")
df4_temp3 <- df4[,c(4,5,6)]
colnames(df4_temp3) <- c("Disp", "NB", "P")
df4 <- rbind(df4_temp1, df4_temp2, df4_temp3)
rm(df4_temp1, df4_temp2, df4_temp3)

df5 <- df[,c(1,32,33,34,35,36,37)]
colnames(df5) <- c("MCS", "X1", "Y1", "X2", "Y2", "X3", "Y3")
df5$NB <- "Sixth theory"
df5$P <- 100
df5 <- df5 %>% mutate(disp_x1 = X1 - lag(X1, default = first(X1)))
df5 <- df5 %>% mutate(disp_y1 = Y1 - lag(Y1, default = first(Y1)))
df5 <- df5 %>% mutate(disp1 = sqrt(disp_x1^2 + disp_y1^2))
df5 <- df5 %>% mutate(disp_x2 = X2 - lag(X2, default = first(X2)))
df5 <- df5 %>% mutate(disp_y2 = Y2 - lag(Y2, default = first(Y2)))
df5 <- df5 %>% mutate(disp2 = sqrt(disp_x2^2 + disp_y2^2))
df5 <- df5 %>% mutate(disp_x3 = X3 - lag(X3, default = first(X3)))
df5 <- df5 %>% mutate(disp_y3 = Y3 - lag(Y3, default = first(Y3)))
df5 <- df5 %>% mutate(disp3 = sqrt(disp_x3^2 + disp_y3^2))
df5 <- df5[,c(1,12,15,18,8,9)]
df5_temp1 <- df5[,c(2,5,6)]
colnames(df5_temp1) <- c("Disp", "NB", "P")
df5_temp2 <- df5[,c(3,5,6)]
colnames(df5_temp2) <- c("Disp", "NB", "P")
df5_temp3 <- df5[,c(4,5,6)]
colnames(df5_temp3) <- c("Disp", "NB", "P")
df5 <- rbind(df5_temp1, df5_temp2, df5_temp3)
rm(df5_temp1, df5_temp2, df5_temp3)

df6 <- df[,c(1,38,39,40,41,42,43)]
colnames(df6) <- c("MCS", "X1", "Y1", "X2", "Y2", "X3", "Y3")
df6$NB <- "Sixth true"
df6$P <- 100
df6 <- df6 %>% mutate(disp_x1 = X1 - lag(X1, default = first(X1)))
df6 <- df6 %>% mutate(disp_y1 = Y1 - lag(Y1, default = first(Y1)))
df6 <- df6 %>% mutate(disp1 = sqrt(disp_x1^2 + disp_y1^2))
df6 <- df6 %>% mutate(disp_x2 = X2 - lag(X2, default = first(X2)))
df6 <- df6 %>% mutate(disp_y2 = Y2 - lag(Y2, default = first(Y2)))
df6 <- df6 %>% mutate(disp2 = sqrt(disp_x2^2 + disp_y2^2))
df6 <- df6 %>% mutate(disp_x3 = X3 - lag(X3, default = first(X3)))
df6 <- df6 %>% mutate(disp_y3 = Y3 - lag(Y3, default = first(Y3)))
df6 <- df6 %>% mutate(disp3 = sqrt(disp_x3^2 + disp_y3^2))
df6 <- df6[,c(1,12,15,18,8,9)]
df6_temp1 <- df6[,c(2,5,6)]
colnames(df6_temp1) <- c("Disp", "NB", "P")
df6_temp2 <- df6[,c(3,5,6)]
colnames(df6_temp2) <- c("Disp", "NB", "P")
df6_temp3 <- df6[,c(4,5,6)]
colnames(df6_temp3) <- c("Disp", "NB", "P")
df6 <- rbind(df6_temp1, df6_temp2, df6_temp3)
rm(df6_temp1, df6_temp2, df6_temp3)

df_mv <- rbind(df1, df2, df3, df4, df5, df6)

a <- ggplot() + 
  geom_violin(data = df_mv[df_mv$P == 70 & df_mv$NB != "Sixth true",], aes(NB, Disp), 
              fill = "red", alpha = 0.5) + theme_bw() +
  labs(title = expression(paste("P"[t], " = 70")),
       y = "Displacement per MCS (px)",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")))) +
  scale_y_continuous(limits = c(0,0.8)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10))

b <- ggplot() + 
  geom_violin(data = df_mv[df_mv$P == 100 & df_mv$NB != "Sixth true",], 
              aes(NB, Disp), fill = "red", alpha = 0.5) + 
  theme_bw() +
  labs(title = expression(paste("P"[t], " = 100")),
       y = "Displacement per MCS (px)",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")))) +
  scale_y_continuous(limits = c(0,3.2)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10))

png(filename = "Plots_single_kind/Plot_movement_blob_no_gameff.png", width = 1700, height = 800, res = 220)
figure <- ggarrange(b + rremove("xlab"),
                    a + rremove("ylab") + rremove("xlab"), 
                    nrow = 1)
annotate_figure(figure, bottom = textGrob("Neighbourhood", gp = gpar(cex = 1)))
dev.off()

##With gamma effective
a2 <- ggplot() + 
  geom_violin(data = df_mv[df_mv$P == 70 & df_mv$NB != "Sixth true",], aes(NB, Disp), 
              fill = "grey", alpha = 0.5) + theme_bw() +
  geom_violin(data = df_mv[df_mv$P == 70 & df_mv$NB == "Sixth true",], aes(NB, Disp), 
              fill = "red", alpha = 0.5) + theme_bw() +
  labs(title = expression(paste("P"[t], " = 70")),
       y = "Displacement per MCS (px)",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")),
                              expression(paste("N6"["eff"])))) +
  scale_y_continuous(limits = c(0,0.8)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10))

b2 <- ggplot() + 
  geom_violin(data = df_mv[df_mv$P == 100 & df_mv$NB != "Sixth true",], aes(NB, Disp), 
              fill = "grey", alpha = 0.5) + theme_bw() +
  geom_violin(data = df_mv[df_mv$P == 100 & df_mv$NB == "Sixth true",], aes(NB, Disp), 
              fill = "red", alpha = 0.5) + theme_bw() +
  labs(title = expression(paste("P"[t], " = 100")),
       y = "Displacement per MCS (px)",
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"^"*")),
                              expression(paste("N6"["eff"])))) +
  scale_y_continuous(limits = c(0,3.2)) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10))

png(filename = "Plots_single_kind/Plot_movement_blob.png", width = 1700, height = 800, res = 220)
figure <- ggarrange(b2 + rremove("xlab"),
                    a2 + rremove("ylab") + rremove("xlab"), 
                    nrow = 1)
annotate_figure(figure, bottom = textGrob("Neighbourhood", gp = gpar(cex = 1)))
dev.off()
