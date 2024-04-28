suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(grid)

cols <- c("MCS", "perim")

df_280 <- fread("data/time_success_p280.txt")
colnames(df_280) <- cols

df_280_sec <- df_280[df_280$perim == 280,]
df_280_six <- df_280[df_280$perim != 280,]

df_280_sec <- df_280_sec %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_280_sec <- df_280_sec[-1,]
df_280_sec <- df_280_sec[df_280_sec$duration >= 0,]

df_280_six <- df_280_six %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_280_six <- df_280_six[-1,]
df_280_six <- df_280_six[df_280_six$duration >= 0,]

df_300 <- fread("data/time_success_p300.txt")
colnames(df_300) <- cols

df_300_sec <- df_300[df_300$perim == 300,]
df_300_six <- df_300[df_300$perim != 300,]

df_300_sec <- df_300_sec %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_300_sec <- df_300_sec[-1,]
df_300_sec <- df_300_sec[df_300_sec$duration >= 0,]

df_300_six <- df_300_six %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_300_six <- df_300_six[-1,]
df_300_six <- df_300_six[df_300_six$duration >= 0,]

df_320 <- fread("data/time_success_p320.txt")
colnames(df_320) <- cols

df_320_sec <- df_320[df_320$perim == 320,]
df_320_six <- df_320[df_320$perim != 320,]

df_320_sec <- df_320_sec %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_320_sec <- df_320_sec[-1,]
df_320_sec <- df_320_sec[df_320_sec$duration >= 0,]

df_320_six <- df_320_six %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_320_six <- df_320_six[-1,]
df_320_six <- df_320_six[df_320_six$duration >= 0,]

df_340 <- fread("data/time_success_p340.txt")
colnames(df_340) <- cols

df_340_sec <- df_340[df_340$perim == 340,]
df_340_six <- df_340[df_340$perim != 340,]

df_340_sec <- df_340_sec %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_340_sec <- df_340_sec[-1,]
df_340_sec <- df_340_sec[df_340_sec$duration >= 0,]

df_340_six <- df_340_six %>% mutate(duration = MCS - lag(MCS, default = first(MCS)))
df_340_six <- df_340_six[-1,]
df_340_six <- df_340_six[df_340_six$duration >= 0,]

df_sec <- rbind(df_280_sec, df_300_sec, df_320_sec, df_340_sec)
df_six <- rbind(df_280_six, df_300_six, df_320_six, df_340_six)

df_sec_plot <- df_sec %>% group_by(perim) %>% summarise(median_dur = median(duration),
                                                        low_quant = quantile(duration, 0.25),
                                                        high_quant = quantile(duration, 0.75))
df_sec_plot$nsam <- 0

for(i in unique(df_sec_plot$perim)){
  df_sec_plot[df_sec_plot$perim == i,]$nsam <- nrow(df_sec[df_sec$perim == i,])
}

df_six_plot <- df_six %>% group_by(perim) %>% summarise(median_dur = median(duration),
                                                        low_quant = quantile(duration, 0.25),
                                                        high_quant = quantile(duration, 0.75))
df_six_plot$nsam <- 0

for(i in unique(df_six_plot$perim)){
  df_six_plot[df_six_plot$perim == i,]$nsam <- nrow(df_six[df_six$perim == i,])
}

df_six_plot$perim <- df_sec_plot$perim

a <- ggplot() + 
  geom_line(data = df_sec_plot, aes(perim, median_dur), color = "#e77e72") + 
  geom_errorbar(data = df_sec_plot, aes(x = perim, 
                                        ymin = low_quant, 
                                        ymax = high_quant),
                width = 0.8,
                color = "#e77e72") +
  geom_point(data = df_sec_plot, aes(perim, median_dur), color = "#e77e72",
             size = 2) +
  geom_line(data = df_six_plot, aes(perim, median_dur), color = "#6f9af8") + 
  geom_errorbar(data = df_six_plot, aes(x = perim, 
                                        ymin = low_quant, 
                                        ymax = high_quant),
                width = 0.8,
                color = "#6f9af8") +
  geom_point(data = df_six_plot, aes(perim, median_dur), color = "#6f9af8",
             size = 2) +
  theme_bw() +
  labs(y = "Median duration between \nsuccessful protrusions (MCSs)",
       x = "Target perimeter") +
  theme(text = element_text(size = 16))

b <- ggplot() + 
  geom_line(data = df_six_plot, aes(perim, median_dur), color = "#6f9af8") + 
  geom_errorbar(data = df_six_plot, aes(x = perim, 
                                        ymin = low_quant, 
                                        ymax = high_quant),
                width = 0.8,
                color = "#6f9af8") +
  geom_point(data = df_six_plot, aes(perim, median_dur), color = "#6f9af8",
             size = 2) +
  theme_bw() +
  labs(y = "Median duration between successful protrusions (MCSs)",
       x = "Target perimeter")

png(filename = "plots/Final_plot_time_protrusions.png", width = 1200, height = 900, res = 180)
a
dev.off()

# png(filename = "plots/Final_plot_time_protrusions_zoom.png", width = 1200, height = 900, res = 180)
# b
# dev.off()
# 
# c <- ggplot() + 
#   geom_line(data = df_sec_plot, aes(perim, nsam), color = "#e77e72") + 
#   geom_point(data = df_sec_plot, aes(perim, nsam), color = "#e77e72",
#              size = 2) +
#   geom_line(data = df_six_plot, aes(perim, nsam), color = "#6f9af8") + 
#   geom_point(data = df_six_plot, aes(perim, nsam), color = "#6f9af8",
#              size = 2) +
#   theme_bw() +
#   labs(y = "Total number of successful protrusions",
#        x = "Target perimeter")
# 
# png(filename = "plots/Final_plot_nsuccessful.png", width = 1200, height = 800, res = 180)
# c
# dev.off()
