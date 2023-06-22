library(hexSticker)
library(magick)
library(sysfonts)
library(tidyverse)


sticker(subplot = hex_image,
        package = "ExamineR",
        s_width = 1.25,
        s_height = 1.25,
        s_x = 1.1,
        s_y = 1.1,
        p_size = 12,
        p_x = 0.8,
        p_y = 0.7,
        p_family = "Aller_Rg",
        p_color = "black",
        h_fill = "grey",
        h_color = "black",
        url = "https://github.com/tjmooney1/ExamineR",
        u_size = 3.5
        ) %>% print()

hex_image <- image_read('~/Downloads/magnifying_glass.png')
