theme_hh <- function() {
    #based on
    theme_linedraw(base_size=12) %+replace% 
    theme(
        # change stuff here
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 16, face = "italic"),
        axis.text.y = element_text(color = "black", size = 16, face = "italic"),
        axis.title.x = element_text(color = "black", size = 20, face = "bold",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color = "black", size = 20, face = "bold", angle = 90,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0))
        )
}

theme_hh_smaller_text <- function() {
    #based on
    theme_linedraw(base_size=12) %+replace% 
    theme(
        # change stuff here
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(color = "black", size = 8, face = "italic"),
        axis.text.y = element_text(color = "black", size = 8, face = "italic"),
        axis.title.x = element_text(color = "black", size = 10, face = "bold",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color = "black", size = 10, face = "bold", angle = 90,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0))
        )
}

theme_hh_facet <- function() {
    #based on 
    theme_hh() %+replace% 
    theme(
        #change stuff here
        axis.text.x = element_text(color = "black", size = 6, face = "italic", angle=50),
        axis.text.y = element_text(color = "black", size = 6, face = "italic"),
        strip.text.x = element_text(size = 8, color = "black", face = "bold"), 
        strip.background = element_rect(color="black", fill="white", size=1, linetype="solid"),
        axis.title.x = element_text(color = "black", size = 16, face = "bold",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color = "black", size = 16, face = "bold", angle = 90,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0))
    )
}

theme_hh_poster <- function() {
    #based on
    theme_linedraw(base_size=35) %+replace% 
    theme(
        # change stuff here
        text = element_text(size=35,  family="monti"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', color = "gray"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(face = "italic", color = "black", size = 40),
        axis.text.x = element_text( face = "italic", color = "black", size = 40),
        axis.text.y = element_text(face = "italic", color = "black", size = 40),
        axis.title.x = element_text(color = "black", size = 50, face = "bold.italic",
                                    margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(color = "black", size = 50, face = "bold.italic", angle = 90,
                                    margin = margin(t = 0, r = 10, b = 0, l = 0))
        )
}