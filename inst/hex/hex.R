## 
## Euler, L. (1736)
## Solutio problematis ad geometriam situs pertinentis
## Commentarii Academiae Scientiarum Imperialis Petropolitanae,
## Vol. 8, pp. 128-140, 1736

library(hexSticker)
sticker("./inst/hex/euler1736_part.png", package="cogmapr", 
        p_size=19, s_x=1, s_y=.8, s_width=.6, p_y=1.55,
        spotlight = TRUE, h_size = 3,
        l_x = 0.75, l_y = 1.15,
        h_fill="#009B95", h_color="#4B0055", p_color="#4B0055",
        filename="./inst/hex/hex_cogmapr.png")

file.copy("./inst/hex/hex_cogmapr.png","../cogmapvisualizr/inst/shiny/cogmapvisualizr/www/")
