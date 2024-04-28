suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(data.table))
library(ggplot2)
library(ggbeeswarm)
library(ggpubr)
library(grid)

df1 <- fread("data/all-res-p70.txt")
colnames(df1) <- c("CPM", "t")

a <- ggplot(df1[df1$CPM != "C3",], aes(CPM, log10(t))) + geom_quasirandom(fill = "black") +
  theme_bw() +
  labs(y = expression(paste("log10(time until sorted state (MCS))")),
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2",
                              expression(paste("N6"^"*")))) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10)) +
  geom_segment(aes(x = 0.75, xend = 1.25, 
                   y = median(log10(df1[df1$CPM == "C1",t])), 
                   yend = median(log10(df1[df1$CPM == "C1",t]))), 
                  color = "red") +
  geom_segment(aes(x = 1.75, xend = 2.25, 
                   y = median(log10(df1[df1$CPM == "C2",t])), 
                   yend = median(log10(df1[df1$CPM == "C2",t]))), 
               color = "red")

# df2 <- fread("data/all-res-p100.txt")
# colnames(df2) <- c("CPM", "t")

# b <- ggplot(df2, aes(CPM, log10(t))) + geom_quasirandom(fill = "black") + theme_bw() + 
#   labs(y = expression(paste("log10(time until sorted state (MCS))")),
#        x = "Neighborhood",
#        title = expression(paste(P[t], " = 100"))) +
#   scale_x_discrete(labels = c("Second",
#                               expression(paste("Sixth + ", xi["theory"])),
#                               expression(paste("Sixth + ", xi["measured"])))) +
#   scale_y_continuous(breaks = seq(2.5,4.5,0.5), limits = c(2.5,4.5)) +
#   theme(axis.text.x = element_text(size = 11),
#         axis.text.y = element_text(size = 10)) +
#   geom_segment(aes(x = 0.75, xend = 1.25, 
#                    y = median(log10(df2[df2$CPM == "C1",t])), 
#                    yend = median(log10(df2[df2$CPM == "C1",t]))), 
#                color = "red") +
#   geom_segment(aes(x = 1.75, xend = 2.25, 
#                    y = median(log10(df2[df2$CPM == "C2",t])), 
#                    yend = median(log10(df2[df2$CPM == "C2",t]))), 
#                color = "red") +
#   geom_segment(aes(x = 2.75, xend = 3.25, 
#                    y = median(log10(df2[df2$CPM == "C3",t])), 
#                    yend = median(log10(df2[df2$CPM == "C3",t]))), 
#                color = "red")

png(filename = "plots/Plot_sorting_finishtime_no_gameff.png", width = 800, height = 750, res = 180)
a
dev.off()

a2 <- ggplot() + 
  geom_quasirandom(data = df1[df1$CPM != "C3",], aes(CPM, log10(t)), color = "grey") +
  geom_quasirandom(data = df1[df1$CPM == "C3",], aes(CPM, log10(t)), color = "black") +
  theme_bw() +
  labs(y = expression(paste("log10(time until sorted state (MCS))")),
       x = "Neighbourhood") +
  scale_x_discrete(labels = c("N2",
                              expression(paste("N6"^"*")),
                              expression(paste("N6"["eff"])))) +
  theme(axis.text.x = element_text(size = 13),
        axis.text.y = element_text(size = 10)) +
  geom_segment(aes(x = 0.75, xend = 1.25, 
                   y = median(log10(df1[df1$CPM == "C1",t])), 
                   yend = median(log10(df1[df1$CPM == "C1",t]))), 
               color = "black") +
  geom_segment(aes(x = 1.75, xend = 2.25, 
                   y = median(log10(df1[df1$CPM == "C2",t])), 
                   yend = median(log10(df1[df1$CPM == "C2",t]))), 
               color = "black") +
  geom_segment(aes(x = 2.75, xend = 3.25, 
                   y = median(log10(df1[df1$CPM == "C3",t])), 
                   yend = median(log10(df1[df1$CPM == "C3",t]))), 
               color = "red")

png(filename = "plots/Plot_sorting_finishtime.png", width = 800, height = 750, res = 180)
a2
dev.off()
