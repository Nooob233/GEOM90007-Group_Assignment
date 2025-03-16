library(magick)

image_write(image_colorize(image_read("www/public/location.png"), opacity = 100, color = "#18181B"), path = "www/public/location_Zinc.png", format = "png")

image_write(image_colorize(image_read("www/public/location.png"), opacity = 100, color = "#783FEB"), path = "www/public/location_Violet.png", format = "png")
