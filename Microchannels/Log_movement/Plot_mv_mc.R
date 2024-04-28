suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(grid)

cols <- c("MCS", "NB", "X_pos", "Y_pos", "C")

## Connectedness----------------------------------------------------------------
df_m_p300 <- fread("data/mvlog_p300_plot_moore.txt")
colnames(df_m_p300) <- cols
df_m_p300 <- df_m_p300[df_m_p300$MCS > 499,]

df_sm_p300 <- fread("data/mvlog_p300_plot_sixth_measured.txt")
colnames(df_sm_p300) <- cols
df_sm_p300$NB <- "sixth_measured"
df_sm_p300 <- df_sm_p300[df_sm_p300$MCS > 499,]

df_m_p280 <- fread("data/mvlog_p280_plot_moore.txt")
colnames(df_m_p280) <- cols
df_m_p280 <- df_m_p280[df_m_p280$MCS > 499,]

df_sm_p280 <- fread("data/mvlog_p280_plot_sixth_measured.txt")
colnames(df_sm_p280) <- cols
df_sm_p280$NB <- "sixth_measured"
df_sm_p280 <- df_sm_p280[df_sm_p280$MCS > 499,]

df_m_p320 <- fread("data/mvlog_p320_plot_moore.txt")
colnames(df_m_p320) <- cols
df_m_p320 <- df_m_p320[df_m_p320$MCS > 499,]

df_sm_p320 <- fread("data/mvlog_p320_plot_sixth_measured.txt")
colnames(df_sm_p320) <- cols
df_sm_p320$NB <- "sixth_measured"
df_sm_p320 <- df_sm_p320[df_sm_p320$MCS > 499,]

df_m_p340 <- fread("data/mvlog_p340_plot_moore.txt")
colnames(df_m_p340) <- cols
df_m_p340 <- df_m_p340[df_m_p340$MCS > 499,]

df_sm_p340 <- fread("data/mvlog_p340_plot_sixth_measured.txt")
colnames(df_sm_p340) <- cols
df_sm_p340$NB <- "sixth_measured"
df_sm_p340 <- df_sm_p340[df_sm_p340$MCS > 499,]

df_m_p360 <- fread("data/mvlog_p360_plot_moore.txt")
colnames(df_m_p360) <- cols
df_m_p360 <- df_m_p360[df_m_p360$MCS > 499,]

df_sm_p360 <- fread("data/mvlog_p360_plot_sixth_measured.txt")
colnames(df_sm_p360) <- cols
df_sm_p360$NB <- "sixth_measured"
df_sm_p360 <- df_sm_p360[df_sm_p360$MCS > 499,]

df_m_p380 <- fread("data/mvlog_p380_plot_moore.txt")
colnames(df_m_p380) <- cols
df_m_p380 <- df_m_p380[df_m_p380$MCS > 499,]

df_sm_p380 <- fread("data/mvlog_p380_plot_sixth_measured.txt")
colnames(df_sm_p380) <- cols
df_sm_p380$NB <- "sixth_measured"
df_sm_p380 <- df_sm_p380[df_sm_p380$MCS > 499,]

df_m_p400 <- fread("data/mvlog_p400_plot_moore.txt")
colnames(df_m_p400) <- cols
df_m_p400 <- df_m_p400[df_m_p400$MCS > 499,]

df_sm_p400 <- fread("data/mvlog_p400_plot_sixth_measured.txt")
colnames(df_sm_p400) <- cols
df_sm_p400$NB <- "sixth_measured"
df_sm_p400 <- df_sm_p400[df_sm_p400$MCS > 499,]

df_merge_p280 <- rbind(df_m_p280, df_sm_p280)
df_merge_p300 <- rbind(df_m_p300, df_sm_p300)
df_merge_p320 <- rbind(df_m_p320, df_sm_p320)
df_merge_p340 <- rbind(df_m_p340, df_sm_p340)
df_merge_p360 <- rbind(df_m_p360, df_sm_p360)
df_merge_p380 <- rbind(df_m_p380, df_sm_p380)
df_merge_p400 <- rbind(df_m_p400, df_sm_p400)
rm(list=ls(pattern="m_p"))

## Movement---------------------------------------------------------------------
df_merge_p280 <- df_merge_p280 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p280[df_merge_p280$disp_x > 240,]$disp_x <- df_merge_p280[df_merge_p280$disp_x > 240,]$disp_x - 250
df_merge_p280[df_merge_p280$disp_x < -240,]$disp_x <- df_merge_p280[df_merge_p280$disp_x < -240,]$disp_x + 250
df_merge_p280 <- df_merge_p280 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p280 <- df_merge_p280 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p280 <- df_merge_p280 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p280 <- df_merge_p280 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p280 <- df_merge_p280[!(df_merge_p280$MCS == 500),]
df_merge_p280 <- df_merge_p280[,c(1,2,5,10)]

df_merge_p300 <- df_merge_p300 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p300[df_merge_p300$disp_x > 240,]$disp_x <- df_merge_p300[df_merge_p300$disp_x > 240,]$disp_x - 250
df_merge_p300[df_merge_p300$disp_x < -240,]$disp_x <- df_merge_p300[df_merge_p300$disp_x < -240,]$disp_x + 250
df_merge_p300 <- df_merge_p300 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p300 <- df_merge_p300 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p300 <- df_merge_p300 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p300 <- df_merge_p300 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p300 <- df_merge_p300[!(df_merge_p300$MCS == 500),]
df_merge_p300 <- df_merge_p300[,c(1,2,5,10)]

df_merge_p320 <- df_merge_p320 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p320[df_merge_p320$disp_x > 240,]$disp_x <- df_merge_p320[df_merge_p320$disp_x > 240,]$disp_x - 250
df_merge_p320[df_merge_p320$disp_x < -240,]$disp_x <- df_merge_p320[df_merge_p320$disp_x < -240,]$disp_x + 250
df_merge_p320 <- df_merge_p320 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p320 <- df_merge_p320 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p320 <- df_merge_p320 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p320 <- df_merge_p320 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p320 <- df_merge_p320[!(df_merge_p320$MCS == 500),]
df_merge_p320 <- df_merge_p320[,c(1,2,5,10)]

df_merge_p340 <- df_merge_p340 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p340[df_merge_p340$disp_x > 240,]$disp_x <- df_merge_p340[df_merge_p340$disp_x > 240,]$disp_x - 250
df_merge_p340[df_merge_p340$disp_x < -240,]$disp_x <- df_merge_p340[df_merge_p340$disp_x < -240,]$disp_x + 250
df_merge_p340 <- df_merge_p340 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p340 <- df_merge_p340 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p340 <- df_merge_p340 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p340 <- df_merge_p340 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p340 <- df_merge_p340[!(df_merge_p340$MCS == 500),]
df_merge_p340 <- df_merge_p340[,c(1,2,5,10)]

df_merge_p360 <- df_merge_p360 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p360[df_merge_p360$disp_x > 240,]$disp_x <- df_merge_p360[df_merge_p360$disp_x > 240,]$disp_x - 250
df_merge_p360[df_merge_p360$disp_x < -240,]$disp_x <- df_merge_p360[df_merge_p360$disp_x < -240,]$disp_x + 250
df_merge_p360 <- df_merge_p360 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p360 <- df_merge_p360 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p360 <- df_merge_p360 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p360 <- df_merge_p360 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p360 <- df_merge_p360[!(df_merge_p360$MCS == 500),]
df_merge_p360 <- df_merge_p360[,c(1,2,5,10)]

df_merge_p380 <- df_merge_p380 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p380[df_merge_p380$disp_x > 240,]$disp_x <- df_merge_p380[df_merge_p380$disp_x > 240,]$disp_x - 250
df_merge_p380[df_merge_p380$disp_x < -240,]$disp_x <- df_merge_p380[df_merge_p380$disp_x < -240,]$disp_x + 250
df_merge_p380 <- df_merge_p380 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p380 <- df_merge_p380 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p380 <- df_merge_p380 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p380 <- df_merge_p380 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p380 <- df_merge_p380[!(df_merge_p380$MCS == 500),]
df_merge_p380 <- df_merge_p380[,c(1,2,5,10)]

df_merge_p400 <- df_merge_p400 %>% mutate(disp_x = X_pos - lag(X_pos, default = first(X_pos)))
df_merge_p400[df_merge_p400$disp_x > 240,]$disp_x <- df_merge_p400[df_merge_p400$disp_x > 240,]$disp_x - 250
df_merge_p400[df_merge_p400$disp_x < -240,]$disp_x <- df_merge_p400[df_merge_p400$disp_x < -240,]$disp_x + 250
df_merge_p400 <- df_merge_p400 %>% mutate(disp_x_2 = (disp_x)^2)
df_merge_p400 <- df_merge_p400 %>% mutate(disp_y = Y_pos - lag(Y_pos, default = first(Y_pos)))
df_merge_p400 <- df_merge_p400 %>% mutate(disp_y_2 = (disp_y)^2)
df_merge_p400 <- df_merge_p400 %>% mutate(disp = sqrt(disp_x_2 + disp_y_2))

df_merge_p400 <- df_merge_p400[!(df_merge_p400$MCS == 500),]
df_merge_p400 <- df_merge_p400[,c(1,2,5,10)]

df_merge_p280_summary <- df_merge_p280 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 280)

for(i in unique(df_merge_p280$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p280[df_merge_p280$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p280[df_merge_p280$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p280_summary[df_merge_p280_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p280_summary[df_merge_p280_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_p300_summary <- df_merge_p300 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 300)

for(i in unique(df_merge_p300$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p300[df_merge_p300$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p300[df_merge_p300$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p300_summary[df_merge_p300_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p300_summary[df_merge_p300_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_p320_summary <- df_merge_p320 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 320)

for(i in unique(df_merge_p320$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p320[df_merge_p320$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p320[df_merge_p320$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p320_summary[df_merge_p320_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p320_summary[df_merge_p320_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_p340_summary <- df_merge_p340 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 340)

for(i in unique(df_merge_p340$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p340[df_merge_p340$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p340[df_merge_p340$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p340_summary[df_merge_p340_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p340_summary[df_merge_p340_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_p360_summary <- df_merge_p360 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 360)

for(i in unique(df_merge_p360$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p360[df_merge_p360$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p360[df_merge_p360$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p360_summary[df_merge_p360_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p360_summary[df_merge_p360_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_p380_summary <- df_merge_p380 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 380)

for(i in unique(df_merge_p380$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p380[df_merge_p380$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p380[df_merge_p380$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p380_summary[df_merge_p380_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p380_summary[df_merge_p380_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_p400_summary <- df_merge_p400 %>% 
  group_by(NB) %>% 
  summarise(med_disp = median(disp),
            low_quant_disp = quantile(disp, 0.25),
            high_quant_disp = quantile(disp, 0.75),
            C = "None",
            Pt = 400)

for(i in unique(df_merge_p400$NB)){
  n_connect = 0
  n_connect = sum(df_merge_p400[df_merge_p400$NB == i,]$C > 0.95)
  frac_connect = n_connect/nrow(df_merge_p400[df_merge_p400$NB == i,])
  if(frac_connect > 0.95){
    df_merge_p400_summary[df_merge_p400_summary$NB == i,]$C <- "Connected"
  } else {
    df_merge_p400_summary[df_merge_p400_summary$NB == i,]$C <- "Ruptured"
  }
}

df_merge_summary <- rbind(df_merge_p280_summary, df_merge_p300_summary,
                          df_merge_p320_summary, df_merge_p340_summary,
                          df_merge_p360_summary, df_merge_p380_summary,
                          df_merge_p400_summary)

e <- ggplot(df_merge_summary, aes(x = as.factor(Pt), 
                             y = med_disp, 
                             group = NB, 
                             color = NB)) + 
  geom_line() +
  geom_point(aes(shape = C), size = 2.5) + 
  theme_bw() +
  scale_shape_manual(values = c(16,4)) +
  scale_color_manual(labels = c("N2",
                              expression(paste("N6"["eff"]))),
                       values = c("#e77e72", "#6f9af8")) +
  theme(legend.text.align = 0) +
  labs(y = "Median displacement per 5 MCSs (px)",
       x = "Target perimeter",
       shape = "Connected?",
       color = "Neighbourhood") +
  geom_errorbar(data = df_merge_summary[df_merge_summary$NB != "sixth",], 
                aes(ymin = low_quant_disp, ymax = high_quant_disp),
                width = 0.02)


png(filename = "plot/Plot_mv_mc.png", width = 1200, height = 800, res = 180)
e
dev.off()
