# repcocourse_boxplots_all_data_points
# Anita Werensteijn-Honingh, started 2019-11-14, continued for reproducible code 2020-01-27.

library(tidyverse)
library(RColorBrewer)
library(magick)
library(ggsignif)
library(stringr)

# keep axes the same across GTV and Bony boxplots of the same direction, as they will be combined
# add 0.5 mm to positive y-axis limit to allow for significance indication

## first make boxplot figure for translations, then repeat for absolute translations
# GTV_LR
tiff("../results/figures/Fake_boxplot_GTV_LR_ind_1.tiff", units="cm", width=10, height=10, res=300)
LR_GTV_boxplot <- ggplot(fake_Combined_timepoint_per_fraction, aes(timepoint, GTV_LR_transl)) + geom_boxplot(aes(fill=mattress)) + 
  scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
  scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
  scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                   labels = c("pre-PV", "pre-post", "PV-post")) +
  ggtitle("A)  GTV intrafraction motion Left-Right") +
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(size = 10, face="bold"),
        axis.text = element_text(size = 10),
        legend.position = "none")
LR_GTV_boxplot
dev.off()
LR_GTV_boxplot

# Bony_LR
tiff("../results/figures/Fake_boxplot_Bony_LR_ind_1.tiff", units="cm", width=10, height=10, res=300)
LR_Bony_boxplot <- ggplot(fake_Combined_timepoint_per_fraction, aes(timepoint, Bony_LR_transl)) + geom_boxplot(aes(fill=mattress)) + 
  scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
  scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
  scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                   labels = c("pre-PV", "pre-post", "PV-post")) +
  ggtitle("B)  Bony intrafraction motion Left-Right") +
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(size = 10, face="bold"),
        axis.text = element_text(size = 10),
        legend.position = "none")
LR_Bony_boxplot
dev.off()
LR_Bony_boxplot

# GTV_AP
tiff("../results/figures/Fake_boxplot_GTV_AP_ind_1.tiff", units="cm", width=10, height=10, res=300)
AP_GTV_boxplot <- ggplot(fake_Combined_timepoint_per_fraction, aes(timepoint, GTV_AP_transl)) + geom_boxplot(aes(fill=mattress)) + 
  scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
  scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
  scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                   labels = c("pre-PV", "pre-post", "PV-post")) +
  ggtitle("C)  GTV intrafraction motion Anterior-Posterior") +
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(size = 10, face="bold"),
        axis.text = element_text(size = 10),
        legend.position = "none")
AP_GTV_boxplot
dev.off()
AP_GTV_boxplot

## todo: test significance again before plotting it and ideally automatically incorporate this in the graphs

# Bony_AP
tiff("../results/figures/Fake_boxplot_Bony_AP_ind_1.tiff", units="cm", width=12.55, height=10, res=300)
AP_Bony_boxplot <- ggplot(fake_Combined_timepoint_per_fraction, aes(timepoint, Bony_AP_transl)) + geom_boxplot(aes(fill=mattress)) + 
  scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
  scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
  scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                   labels = c("pre-PV", "pre-post", "PV-post")) +
  ggtitle("D)  Bony intrafraction motion Anterior-Posterior") +
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(size = 10, face="bold"),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 12, face="bold"),
        legend.text = element_text(size = 12)) +
  labs(fill = "Evacuated\ncushion") +
  geom_signif(stat="identity", data=data.frame(x = 0.875, xend = 1.125, y = 7.12, annotation = "*"), 
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation, textsize = 8, vjust = 0.4)) +
  geom_signif(stat="identity", data=data.frame(x = 1.875, xend = 2.125, y = 7.12, annotation = "*"), 
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation, textsize = 8, vjust = 0.4))
AP_Bony_boxplot
dev.off()
AP_Bony_boxplot

# GTV_CC
tiff("../results/figures/Fake_boxplot_GTV_CC_ind_1.tiff", units="cm", width=10, height=10, res=300)
CC_GTV_boxplot <- ggplot(fake_Combined_timepoint_per_fraction, aes(timepoint, GTV_CC_transl)) + geom_boxplot(aes(fill=mattress)) + 
  scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
  scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
  scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                   labels = c("pre-PV", "pre-post", "PV-post")) +
  ggtitle("E)  GTV intrafraction motion Cranial-Caudal") +
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(size = 10, face="bold"),
        axis.text = element_text(size = 10),
        legend.position = "none")
CC_GTV_boxplot
dev.off()
CC_GTV_boxplot

# Bony_CC
tiff("../results/figures/Fake_boxplot_Bony_CC_ind_1.tiff", units="cm", width=10, height=10, res=300)
CC_Bony_boxplot <- ggplot(fake_Combined_timepoint_per_fraction, aes(timepoint, Bony_CC_transl)) + geom_boxplot(aes(fill=mattress)) + 
  scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
  scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
  scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                   labels = c("pre-PV", "pre-post", "PV-post")) +
  ggtitle("F)  Bony intrafraction motion Cranial-Caudal") +
  theme(plot.title = element_text(size = 10, family = "Tahoma", face = "bold"),
        text = element_text(size = 10, family = "Tahoma"),
        axis.title = element_text(size = 10, face="bold"),
        axis.text = element_text(size = 10),
        legend.position = "none")
CC_Bony_boxplot
dev.off()
CC_Bony_boxplot


tiff("../results/figures/2305-3100mm_empty.tiff", units="cm", width=23.05, height=31, res=300)
ggplot() + theme_void()
dev.off()

# combine all six into single image
empty_2305_3100mm_figure <- image_read("../results/figures/2305-3100mm_empty.tiff")
image_A <- image_read("../results/figures/Fake_boxplot_GTV_LR_ind_1.tiff")
image_B <- image_read("../results/figures/Fake_boxplot_Bony_LR_ind_1.tiff")
image_C <- image_read("../results/figures/Fake_boxplot_GTV_AP_ind_1.tiff")
image_D <- image_read("../results/figures/Fake_boxplot_Bony_AP_ind_1.tiff")
image_E <- image_read("../results/figures/Fake_boxplot_GTV_CC_ind_1.tiff")
image_F <- image_read("../results/figures/Fake_boxplot_Bony_CC_ind_1.tiff")
combi_image_1 <- image_composite(empty_2305_3100mm_figure, image_A)
combi_image_2 <- image_composite(combi_image_1, image_B, offset = "+1240")
combi_image_3 <- image_composite(combi_image_2, image_C, offset = "+0+1240")
combi_image_4 <- image_composite(combi_image_3, image_D, offset = "+1240+1240")
combi_image_5 <- image_composite(combi_image_4, image_E, offset = "+0+2480")
combi_image_6 <- image_composite(combi_image_5, image_F, offset = "+1240+2480")
image_write(combi_image_6, path = "../results/figures/Fake_combi_boxplots_mult_timepoints_ind.tiff", format = "tiff")