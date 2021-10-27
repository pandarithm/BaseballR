knitr::opts_chunk$set(tidy = FALSE, highlight = TRUE, comment = NA, 
                      prompt = FALSE, fig.width = 6, fig.height = 3.5,
                      message = FALSE)
options(digits = 3)
options(width = 64)
options(continue = " ")
library(tidyverse)
#crcblue <- c(0.7453, 0.9689, 0, 0.3686)
# Pantone blue 072
crcblue <- "#2905a1"

# continuous color palette
# https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2
crc_pal <- colorRampPalette(c(crcblue, "white"))

crc_2 <- crc_pal(3)[1:2]
crc_3 <- crc_pal(4)[1:3]

crc_2 <- c(crcblue, "gray70")
crc_3 <- c(crcblue, "gray70", "gray10")

# https://mycolor.space/?hex=%232905A1&sub=1
# discrete palette
crc_4 <- c(crcblue, "#9b89b4", "#fdf7ff", "#362b48")

crc_d <- c(crcblue, "gray94", "#9b89b4", "gray50")

#ggplot(mtcars, aes(x = disp, y = mpg, color = factor(gear))) +
#  geom_point() + 
#  scale_color_manual(values = crc_d)
