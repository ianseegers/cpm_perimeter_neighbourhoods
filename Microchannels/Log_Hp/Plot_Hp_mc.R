suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(grid)
library(LaplacesDemon)

cols <- c("MCS", "srci", "tgti", "constraint", "value")
cols_perim <- c("MCS", "srci", "tgti", "Perimeter")
cols_tot <- c("MCS", "srci", "tgti", "Total")
cols_pcop <- c("MCS", "srci", "tgti", "pcopy")

##P = 300-----------------------------------------------------------------------
df_m_p300 <- fread("data/hplog_p300_plot_moore.txt")
colnames(df_m_p300) <- cols

df_m_p300_perim <- df_m_p300[df_m_p300$constraint == "Perim",]
df_m_p300_perim <- df_m_p300_perim[,-4]
colnames(df_m_p300_perim) <- cols_perim

df_m_p300_tot <- df_m_p300[df_m_p300$constraint == "Total",]
df_m_p300_tot <- df_m_p300_tot[,-4]
colnames(df_m_p300_tot) <- cols_tot

df_m_p300_pcop <- df_m_p300[df_m_p300$constraint == "pcopy",]
df_m_p300_pcop <- df_m_p300_pcop[,-4]
colnames(df_m_p300_pcop) <- cols_pcop

df_m_p300 <- merge(df_m_p300_tot, df_m_p300_pcop, by = c("MCS", "srci", "tgti"))
df_m_p300 <- merge(df_m_p300_perim, df_m_p300, by = c("MCS", "srci", "tgti"))
df_m_p300 <- df_m_p300[!(duplicated(df_m_p300[,c(1:3)]))]

df_st_p300 <- fread("data/hplog_p300_plot_sixth_theory.txt")
colnames(df_st_p300) <- cols

df_st_p300_perim <- df_st_p300[df_st_p300$constraint == "Perim",]
df_st_p300_perim <- df_st_p300_perim[,-4]
colnames(df_st_p300_perim) <- cols_perim

df_st_p300_tot <- df_st_p300[df_st_p300$constraint == "Total",]
df_st_p300_tot <- df_st_p300_tot[,-4]
colnames(df_st_p300_tot) <- cols_tot

df_st_p300_pcop <- df_st_p300[df_st_p300$constraint == "pcopy",]
df_st_p300_pcop <- df_st_p300_pcop[,-4]
colnames(df_st_p300_pcop) <- cols_pcop

df_st_p300 <- merge(df_st_p300_tot, df_st_p300_pcop, by = c("MCS", "srci", "tgti"))
df_st_p300 <- merge(df_st_p300_perim, df_st_p300, by = c("MCS", "srci", "tgti"))
df_st_p300 <- df_st_p300[!(duplicated(df_st_p300[,c(1:3)]))]

df_sm_p300 <- fread("data/hplog_p300_plot_sixth_measured.txt")
colnames(df_sm_p300) <- cols

df_sm_p300_perim <- df_sm_p300[df_sm_p300$constraint == "Perim",]
df_sm_p300_perim <- df_sm_p300_perim[,-4]
colnames(df_sm_p300_perim) <- cols_perim

df_sm_p300_tot <- df_sm_p300[df_sm_p300$constraint == "Total",]
df_sm_p300_tot <- df_sm_p300_tot[,-4]
colnames(df_sm_p300_tot) <- cols_tot

df_sm_p300_pcop <- df_sm_p300[df_sm_p300$constraint == "pcopy",]
df_sm_p300_pcop <- df_sm_p300_pcop[,-4]
colnames(df_sm_p300_pcop) <- cols_pcop

df_sm_p300 <- merge(df_sm_p300_tot, df_sm_p300_pcop, by = c("MCS", "srci", "tgti"))
df_sm_p300 <- merge(df_sm_p300_perim, df_sm_p300, by = c("MCS", "srci", "tgti"))
df_sm_p300 <- df_sm_p300[!(duplicated(df_sm_p300[,c(1:3)]))]

rm(list=ls(pattern="_p300_"))

df_m_p300$NB <- "second"
df_st_p300$NB <- "sixth_theory"
df_sm_p300$NB <- "sixth_true"

##P = 280-----------------------------------------------------------------------
df_m_p280 <- fread("data/hplog_p280_plot_moore.txt")
colnames(df_m_p280) <- cols

df_m_p280_perim <- df_m_p280[df_m_p280$constraint == "Perim",]
df_m_p280_perim <- df_m_p280_perim[,-4]
colnames(df_m_p280_perim) <- cols_perim

df_m_p280_tot <- df_m_p280[df_m_p280$constraint == "Total",]
df_m_p280_tot <- df_m_p280_tot[,-4]
colnames(df_m_p280_tot) <- cols_tot

df_m_p280_pcop <- df_m_p280[df_m_p280$constraint == "pcopy",]
df_m_p280_pcop <- df_m_p280_pcop[,-4]
colnames(df_m_p280_pcop) <- cols_pcop

df_m_p280 <- merge(df_m_p280_tot, df_m_p280_pcop, by = c("MCS", "srci", "tgti"))
df_m_p280 <- merge(df_m_p280_perim, df_m_p280, by = c("MCS", "srci", "tgti"))
df_m_p280 <- df_m_p280[!(duplicated(df_m_p280[,c(1:3)]))]

df_st_p280 <- fread("data/hplog_p280_plot_sixth_theory.txt")
colnames(df_st_p280) <- cols

df_st_p280_perim <- df_st_p280[df_st_p280$constraint == "Perim",]
df_st_p280_perim <- df_st_p280_perim[,-4]
colnames(df_st_p280_perim) <- cols_perim

df_st_p280_tot <- df_st_p280[df_st_p280$constraint == "Total",]
df_st_p280_tot <- df_st_p280_tot[,-4]
colnames(df_st_p280_tot) <- cols_tot

df_st_p280_pcop <- df_st_p280[df_st_p280$constraint == "pcopy",]
df_st_p280_pcop <- df_st_p280_pcop[,-4]
colnames(df_st_p280_pcop) <- cols_pcop

df_st_p280 <- merge(df_st_p280_tot, df_st_p280_pcop, by = c("MCS", "srci", "tgti"))
df_st_p280 <- merge(df_st_p280_perim, df_st_p280, by = c("MCS", "srci", "tgti"))
df_st_p280 <- df_st_p280[!(duplicated(df_st_p280[,c(1:3)]))]

df_sm_p280 <- fread("data/hplog_p280_plot_sixth_measured.txt")
colnames(df_sm_p280) <- cols

df_sm_p280_perim <- df_sm_p280[df_sm_p280$constraint == "Perim",]
df_sm_p280_perim <- df_sm_p280_perim[,-4]
colnames(df_sm_p280_perim) <- cols_perim

df_sm_p280_tot <- df_sm_p280[df_sm_p280$constraint == "Total",]
df_sm_p280_tot <- df_sm_p280_tot[,-4]
colnames(df_sm_p280_tot) <- cols_tot

df_sm_p280_pcop <- df_sm_p280[df_sm_p280$constraint == "pcopy",]
df_sm_p280_pcop <- df_sm_p280_pcop[,-4]
colnames(df_sm_p280_pcop) <- cols_pcop

df_sm_p280 <- merge(df_sm_p280_tot, df_sm_p280_pcop, by = c("MCS", "srci", "tgti"))
df_sm_p280 <- merge(df_sm_p280_perim, df_sm_p280, by = c("MCS", "srci", "tgti"))
df_sm_p280 <- df_sm_p280[!(duplicated(df_sm_p280[,c(1:3)]))]

rm(list=ls(pattern="_p280_"))

df_m_p280$NB <- "second"
df_st_p280$NB <- "sixth_theory"
df_sm_p280$NB <- "sixth_true"

##P = 320-----------------------------------------------------------------------
df_m_p320 <- fread("data/hplog_p320_plot_moore.txt")
colnames(df_m_p320) <- cols

df_m_p320_perim <- df_m_p320[df_m_p320$constraint == "Perim",]
df_m_p320_perim <- df_m_p320_perim[,-4]
colnames(df_m_p320_perim) <- cols_perim

df_m_p320_tot <- df_m_p320[df_m_p320$constraint == "Total",]
df_m_p320_tot <- df_m_p320_tot[,-4]
colnames(df_m_p320_tot) <- cols_tot

df_m_p320_pcop <- df_m_p320[df_m_p320$constraint == "pcopy",]
df_m_p320_pcop <- df_m_p320_pcop[,-4]
colnames(df_m_p320_pcop) <- cols_pcop

df_m_p320 <- merge(df_m_p320_tot, df_m_p320_pcop, by = c("MCS", "srci", "tgti"))
df_m_p320 <- merge(df_m_p320_perim, df_m_p320, by = c("MCS", "srci", "tgti"))
df_m_p320 <- df_m_p320[!(duplicated(df_m_p320[,c(1:3)]))]

df_st_p320 <- fread("data/hplog_p320_plot_sixth_theory.txt")
colnames(df_st_p320) <- cols

df_st_p320_perim <- df_st_p320[df_st_p320$constraint == "Perim",]
df_st_p320_perim <- df_st_p320_perim[,-4]
colnames(df_st_p320_perim) <- cols_perim

df_st_p320_tot <- df_st_p320[df_st_p320$constraint == "Total",]
df_st_p320_tot <- df_st_p320_tot[,-4]
colnames(df_st_p320_tot) <- cols_tot

df_st_p320_pcop <- df_st_p320[df_st_p320$constraint == "pcopy",]
df_st_p320_pcop <- df_st_p320_pcop[,-4]
colnames(df_st_p320_pcop) <- cols_pcop

df_st_p320 <- merge(df_st_p320_tot, df_st_p320_pcop, by = c("MCS", "srci", "tgti"))
df_st_p320 <- merge(df_st_p320_perim, df_st_p320, by = c("MCS", "srci", "tgti"))
df_st_p320 <- df_st_p320[!(duplicated(df_st_p320[,c(1:3)]))]

df_sm_p320 <- fread("data/hplog_p320_plot_sixth_measured.txt")
colnames(df_sm_p320) <- cols

df_sm_p320_perim <- df_sm_p320[df_sm_p320$constraint == "Perim",]
df_sm_p320_perim <- df_sm_p320_perim[,-4]
colnames(df_sm_p320_perim) <- cols_perim

df_sm_p320_tot <- df_sm_p320[df_sm_p320$constraint == "Total",]
df_sm_p320_tot <- df_sm_p320_tot[,-4]
colnames(df_sm_p320_tot) <- cols_tot

df_sm_p320_pcop <- df_sm_p320[df_sm_p320$constraint == "pcopy",]
df_sm_p320_pcop <- df_sm_p320_pcop[,-4]
colnames(df_sm_p320_pcop) <- cols_pcop

df_sm_p320 <- merge(df_sm_p320_tot, df_sm_p320_pcop, by = c("MCS", "srci", "tgti"))
df_sm_p320 <- merge(df_sm_p320_perim, df_sm_p320, by = c("MCS", "srci", "tgti"))
df_sm_p320 <- df_sm_p320[!(duplicated(df_sm_p320[,c(1:3)]))]

rm(list=ls(pattern="_p320_"))

df_m_p320$NB <- "second"
df_st_p320$NB <- "sixth_theory"
df_sm_p320$NB <- "sixth_true"

##P = 340-----------------------------------------------------------------------
df_m_p340 <- fread("data/hplog_p340_plot_moore.txt")
colnames(df_m_p340) <- cols

df_m_p340_perim <- df_m_p340[df_m_p340$constraint == "Perim",]
df_m_p340_perim <- df_m_p340_perim[,-4]
colnames(df_m_p340_perim) <- cols_perim

df_m_p340_tot <- df_m_p340[df_m_p340$constraint == "Total",]
df_m_p340_tot <- df_m_p340_tot[,-4]
colnames(df_m_p340_tot) <- cols_tot

df_m_p340_pcop <- df_m_p340[df_m_p340$constraint == "pcopy",]
df_m_p340_pcop <- df_m_p340_pcop[,-4]
colnames(df_m_p340_pcop) <- cols_pcop

df_m_p340 <- merge(df_m_p340_tot, df_m_p340_pcop, by = c("MCS", "srci", "tgti"))
df_m_p340 <- merge(df_m_p340_perim, df_m_p340, by = c("MCS", "srci", "tgti"))
df_m_p340 <- df_m_p340[!(duplicated(df_m_p340[,c(1:3)]))]

df_st_p340 <- fread("data/hplog_p340_plot_sixth_theory.txt")
colnames(df_st_p340) <- cols

df_st_p340_perim <- df_st_p340[df_st_p340$constraint == "Perim",]
df_st_p340_perim <- df_st_p340_perim[,-4]
colnames(df_st_p340_perim) <- cols_perim

df_st_p340_tot <- df_st_p340[df_st_p340$constraint == "Total",]
df_st_p340_tot <- df_st_p340_tot[,-4]
colnames(df_st_p340_tot) <- cols_tot

df_st_p340_pcop <- df_st_p340[df_st_p340$constraint == "pcopy",]
df_st_p340_pcop <- df_st_p340_pcop[,-4]
colnames(df_st_p340_pcop) <- cols_pcop

df_st_p340 <- merge(df_st_p340_tot, df_st_p340_pcop, by = c("MCS", "srci", "tgti"))
df_st_p340 <- merge(df_st_p340_perim, df_st_p340, by = c("MCS", "srci", "tgti"))
df_st_p340 <- df_st_p340[!(duplicated(df_st_p340[,c(1:3)]))]

df_sm_p340 <- fread("data/hplog_p340_plot_sixth_measured.txt")
colnames(df_sm_p340) <- cols

df_sm_p340_perim <- df_sm_p340[df_sm_p340$constraint == "Perim",]
df_sm_p340_perim <- df_sm_p340_perim[,-4]
colnames(df_sm_p340_perim) <- cols_perim

df_sm_p340_tot <- df_sm_p340[df_sm_p340$constraint == "Total",]
df_sm_p340_tot <- df_sm_p340_tot[,-4]
colnames(df_sm_p340_tot) <- cols_tot

df_sm_p340_pcop <- df_sm_p340[df_sm_p340$constraint == "pcopy",]
df_sm_p340_pcop <- df_sm_p340_pcop[,-4]
colnames(df_sm_p340_pcop) <- cols_pcop

df_sm_p340 <- merge(df_sm_p340_tot, df_sm_p340_pcop, by = c("MCS", "srci", "tgti"))
df_sm_p340 <- merge(df_sm_p340_perim, df_sm_p340, by = c("MCS", "srci", "tgti"))
df_sm_p340 <- df_sm_p340[!(duplicated(df_sm_p340[,c(1:3)]))]

rm(list=ls(pattern="_p340_"))

df_m_p340$NB <- "second"
df_st_p340$NB <- "sixth_theory"
df_sm_p340$NB <- "sixth_true"



##Combining data----------------------------------------------------------------
df_merge_p280 <- rbind(df_m_p280, df_st_p280, df_sm_p280)
df_merge_p300 <- rbind(df_m_p300, df_st_p300, df_sm_p300)
df_merge_p320 <- rbind(df_m_p320, df_st_p320, df_sm_p320)
df_merge_p340 <- rbind(df_m_p340, df_st_p340, df_sm_p340)

##Plotting----------------------------------------------------------------------
a2 <- ggplot() + 
  geom_hline(yintercept = 0, color = "black") +
  geom_quasirandom(data = df_merge_p280[df_merge_p280$NB != "sixth_theory",], 
                   aes(x = as.factor(NB), y = Perimeter, color = pcopy),
                   size = 0.1, alpha = 0.25) + 
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"["eff"])))) +
  scale_y_continuous(limits = c(0, 
                                1750),
                     breaks = seq(0, 
                                  1750,
                                  by = 250)) +
  labs(title = expression(paste(P[t], " = 280")),
       color = expression(paste("p"[copy]))) +
  scale_color_gradientn(colours = c("grey", "#FD5353", "#FD5353"),
                        values = c(0,0.5,1))
b2 <- ggplot() + 
  geom_hline(yintercept = 0, color = "black") +
  geom_quasirandom(data = df_merge_p300[df_merge_p300$NB != "sixth_theory",], 
                   aes(x = as.factor(NB), y = Perimeter, color = pcopy),
                   size = 0.1, alpha = 0.25) + 
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"["eff"])))) +
  scale_y_continuous(limits = c(0, 
                                1250),
                     breaks = seq(0, 
                                  1250,
                                  by = 250)) +
  labs(title = expression(paste(P[t], " = 300")),
       color = expression(paste("p"[copy]))) +
  scale_color_gradientn(colours = c("grey", "#FD5353", "#FD5353"),
                        values = c(0,0.5,1))
c2 <- ggplot() + 
  geom_hline(yintercept = 0, color = "black") +
  geom_quasirandom(data = df_merge_p320[df_merge_p320$NB != "sixth_theory",], 
                   aes(x = as.factor(NB), y = Perimeter, color = pcopy),
                   size = 0.1, alpha = 0.25) + 
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"["eff"])))) +
  scale_y_continuous(limits = c(-400, 
                                800),
                     breaks = seq(-250, 
                                  750,
                                  by = 250)) +
  labs(title = expression(paste(P[t], " = 320")),
       color = expression(paste("p"[copy]))) +
  scale_color_gradientn(colours = c("grey", "#FD5353", "#FD5353"),
                        values = c(0,0.5,1))
d2 <- ggplot() + 
  geom_hline(yintercept = 0, color = "black") +
  geom_quasirandom(data = df_merge_p340[df_merge_p340$NB != "sixth_theory",], 
                   aes(x = as.factor(NB), y = Perimeter, color = pcopy),
                   size = 0.1, alpha = 0.25) + 
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  scale_x_discrete(labels = c("N2", 
                              expression(paste("N6"["eff"])))) +
  scale_y_continuous(limits = c(-300, 
                                750),
                     breaks = seq(-250, 
                                  750,
                                  by = 250)) +
  labs(title = expression(paste(P[t], " = 340")),
       color = expression(paste("p"[copy]))) +
  scale_color_gradientn(colours = c("grey", "#FD5353", "#FD5353"),
                        values = c(0,0.5,1))

png(filename = "plots/Plot_deltaHP_mc_selection.png", width = 4800, height = 1200, res = 360)
figure <- ggarrange(a2,b2,c2,d2, nrow = 1, ncol = 4, common.legend = TRUE, legend = "right")
annotate_figure(figure, 
                left = textGrob(expression(paste(Delta, "H"[P],)), 
                                gp = gpar(cex = 1),
                                rot = 90),
                bottom = textGrob("Neighbourhood", 
                                  gp = gpar(cex = 1)))
dev.off()
