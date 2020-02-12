library(hexSticker)
library(here)

sticker(here("man/figures",
             "sem.png"),
        filename = here("man/figures", "logo.png"),
        package = "semoutput",
        h_fill = "#FFFFFF",
        h_color = "#D45E58",
        h_size = 1.8,
        p_color = "#D45E58",
        p_size = 6,
        s_width = .75,
        s_x = 1.0,
        s_y = .70)
