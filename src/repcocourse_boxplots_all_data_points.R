# repcocourse_boxplots_all_data_points
# Anita Werensteijn-Honingh, started 2019-11-14, continued for reproducible code 2020-01-27.

library(tidyverse)
library(RColorBrewer)
library(magick)
library(ggsignif)
library(stringr)

# axes limits are now still hard-coded
# axes limits are the same across GTV and Bony boxplots of all three directions, as they will be combined
# add 0.5 mm to positive y-axis limit to allow for significance indication (always take the largest limit of 3 directions)
# and then round off the y-axis limits to integers or if wanted multiplications of 2 (if you use breaks at each 2)


create_timepoint_boxplot <- function(dataset, transl_parameter_name, subfigure_letter, title, text_size, 
                                     plot_file_name, legend=FALSE){
  ## takes in a dataset, the transl_parameter_name (column name) that you want to make the sub-boxplot for, 
  ## the subfigure_letter (a capital letter that indicates the subfigure), the subfigure title, the text_size,
  ## the plot_file_name (the name with which you want to save the subfigure (usually .tiff)) and the legend (TRUE/FALSE)
  ## indicates whether a legend needs to be plotted for this subfigure
  
  # standard settings (will be used for legend == FALSE), does not plot the legends (position <- none means don't plot legend)
  plot_width <- 10
  legend_position <- "none"

  # if legend == TRUE, modify plot width to allow space for the legend and plot the legend
  if(legend){
    plot_width <- 12.55
    legend_position <- "right"
  }
  
  ## plot the boxplot using ggplot. it entails boxplots at 3 timepoints and is splitted for with/without mattress
  sub_boxplot <- ggplot(dataset, aes(timepoint, dataset[, transl_parameter_name])) + geom_boxplot(aes(fill=mattress)) + 
    # set colors for with/without mattress
    scale_fill_manual(values = c("gray44", "cornsilk"), labels = c("No", "Yes")) + 
    scale_y_continuous(name = "Translation (mm)", breaks = seq(-6, 8, 2), limits=c(-6, 8)) + 
    scale_x_discrete(name = "Interval between scans", limits = c("pv","post", "change"), 
                     labels = c("pre-PV", "pre-post", "PV-post")) +
    ggtitle(sprintf("%s)  %s", subfigure_letter, title)) +
    theme(plot.title = element_text(size = text_size, family = "Tahoma", face = "bold"),
          text = element_text(size = text_size, family = "Tahoma"),
          axis.title = element_text(size = text_size, face="bold"),
          axis.text = element_text(size = text_size),
          legend.title = element_text(size = (text_size), face="bold"),
          legend.text = element_text(size = (text_size)),
          legend.position = legend_position)
  if(legend){
    sub_boxplot <- sub_boxplot + labs(fill = "Evacuated\ncushion")
  }  
  ggsave(plot_file_name, sub_boxplot, width=plot_width, height=10, dpi=300, units="cm")
}

counter <- 0
capital_letters <- c("A", "B", "C", "D", "E", "F")
titles <- c("GTV intrafraction motion Left-Right", "Bony intrafraction motion Left-Right",
            "GTV intrafraction motion Anterior-Posterior", "Bony intrafraction motion Anterior-Posterior",
            "GTV intrafraction motion Cranial-Caudal", "Bony intrafraction motion Cranial-Caudal")
for(transl_parameter_name in c("GTV_LR_transl", "Bony_LR_transl",
                   "GTV_AP_transl", "Bony_AP_transl",
                   "GTV_CC_transl", "Bony_CC_transl")){
  counter <- counter + 1
  print(counter)
  subfigure_letter <- capital_letters[counter]
  print(subfigure_letter)
  title <- titles[counter]
  print(title)
  plot_file_name <- sprintf("../results/figures/Fake_boxplot_%s_ind_1.tiff", transl_parameter_name)
  print(plot_file_name)
  if(transl_parameter_name == "Bony_AP_transl"){
    legend <- TRUE
  }
  else{
    legend <- FALSE
  }
  create_timepoint_boxplot(fake_Combined_timepoint_per_fraction, transl_parameter_name, subfigure_letter, title, 10,
                           plot_file_name, legend)
  print("completed")
}


tiff("../results/figures/2305-3100mm_empty.tiff", units="cm", width=23.05, height=31, res=300)
ggplot() + theme_void()
dev.off()

# combine all six into single image
empty_2305_3100mm_figure <- image_read("../results/figures/2305-3100mm_empty.tiff")
image_A <- image_read("../results/figures/Fake_boxplot_GTV_LR_transl_ind_1.tiff")
image_B <- image_read("../results/figures/Fake_boxplot_Bony_LR_transl_ind_1.tiff")
image_C <- image_read("../results/figures/Fake_boxplot_GTV_AP_transl_ind_1.tiff")
image_D <- image_read("../results/figures/Fake_boxplot_Bony_AP_transl_ind_1.tiff")
image_E <- image_read("../results/figures/Fake_boxplot_GTV_CC_transl_ind_1.tiff")
image_F <- image_read("../results/figures/Fake_boxplot_Bony_CC_transl_ind_1.tiff")
combi_image_1 <- image_composite(empty_2305_3100mm_figure, image_A)
combi_image_2 <- image_composite(combi_image_1, image_B, offset = "+1240")
combi_image_3 <- image_composite(combi_image_2, image_C, offset = "+0+1240")
combi_image_4 <- image_composite(combi_image_3, image_D, offset = "+1240+1240")
combi_image_5 <- image_composite(combi_image_4, image_E, offset = "+0+2480")
combi_image_6 <- image_composite(combi_image_5, image_F, offset = "+1240+2480")
image_write(combi_image_6, path = "../results/figures/Fake_combi_boxplots_mult_timepoints_ind2.tiff", format = "tiff")