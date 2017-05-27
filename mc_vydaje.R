## Vydaje podle mestskych casti

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zoo)
library(ggiraph)
library(gganimate)
library(plotly)

# options(scipen = 666)
rozpocet <- read.csv("rozpocet.csv")
vydaje_mc2016 <- subset(rozpocet, rok == 2016 & k_za_ucetni_jedn == "Ne" & rozdeleni == "Výdaje")
                    # & nakladove_stredisko != "Nákladové středisko Magistrát města Brna") 
vydaje_mc2016 <- vydaje_mc2016[!grepl("VHČ", vydaje_mc2016$nakladove_stredisko), ]
vydaje_mc2016 %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
vydaje_mc2016$nakladove_stredisko %<>% gsub(pattern = "Nákladové středisko ", replacement = "")

## Aggregace podle skupiny
sum_vydaje <- ddply(vydaje_mc2016, .(nakladove_stredisko, skupina), summarise, 
                    vydaje = abs(sum(cerpano)), 
                    schvaleno = abs(sum(rozpocet_schvaleny)), 
                    upraveno = abs(sum(rozpocet_upraveny)))

#sum_vydaje %<>% subset(skupina2 != "Bez zařazení")
sum_vydaje$skupina %<>% factor
sum_vydaje$nakladove_stredisko %<>% factor

## doplnit chybejici mestske casti, ktere meli vydaje 0 v dane oblasti
imput <- data.frame(name = levels(sum_vydaje$nakladove_stredisko), 
                    whatever = NA)
sum_vydaje <- ddply(sum_vydaje, .(skupina), function(x) dplyr::full_join(x, imput, 
                                                            by = c("nakladove_stredisko"="name")))
sum_vydaje$skupina %<>% na.locf
sum_vydaje$vydaje[is.na(sum_vydaje$vydaje)] <- 0

## Celkove vydaje na skupina podle mestskych casti
# lapply(levels(sum_vydaje$skupina), 
#        function(x) ggplot(sum_vydaje[sum_vydaje$skupina == x, ], 
#                           aes(x = reorder(factor(nakladove_stredisko), vydaje), y = vydaje)) + 
#            geom_bar(stat = "identity") + xlab(x) + coord_flip())

## plotly chart - absolutni vydaje podle mestskych casti
hide_legend(ggplotly(
    ggplot(sum_vydaje[sum_vydaje$nakladove_stredisko != "Magistrát města Brna", ], 
           aes(x = nakladove_stredisko, 
                       y = vydaje, fill = factor(skupina))) + geom_bar(stat = "identity") + 
    coord_flip()))

## Vydaje podle poctu obyvatel

mc_data <- read.csv("citizen-counts-brno2.csv", stringsAsFactors = FALSE)
colnames(mc_data) <- c("nazev", "pocet_obyv", "katastr", "pocet_zsj")
mc_data$katastr <- NULL
mc_data$nazev %<>% gsub(pattern = "Městská část", replacement ="MČ") %>% factor
all_brno <- data.frame(nazev = "Magistrát města Brna", 
                       pocet_obyv = sum(mc_data$pocet_obyv), 
                       pocet_zsj = sum(mc_data$pocet_zsj))
mc_data <- rbind(mc_data, all_brno)
mc_data$nazev %<>% as.character %>% factor
sum_vydaje$nakladove_stredisko %<>% as.character %>% factor
sum_vydaje$whatever <- NULL

#identical(levels(mc_data$nazev), levels(sum_vydaje$nakladove_stredisko))

sum_vydaje_pp <- inner_join(sum_vydaje, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_vydaje_pp$vydaje_pp <- with(sum_vydaje_pp, vydaje / pocet_obyv)

# lapply(levels(sum_vydaje_pp$skupina), 
#        function(x) ggplot(sum_vydaje_pp[sum_vydaje_pp$skupina == x, ], 
#                           aes(x = reorder(factor(nakladove_stredisko), vydaje_pp), y = vydaje_pp)) + 
#            geom_bar(stat = "identity") + xlab(x) + coord_flip())

sum_vydaje_pp_wide <- tidyr::spread(sum_vydaje_pp[, c("nakladove_stredisko", "skupina", "vydaje_pp")], 
                                    skupina, vydaje_pp)
sum_vydaje_pp_wide$nakladove_stredisko <- reorder(sum_vydaje_pp_wide$nakladove_stredisko, 
                                                  rowSums(sum_vydaje_pp_wide[, 2:14]))
sum_vydaje_pp2 <- tidyr::gather(sum_vydaje_pp_wide, "skupina", "vydaje_pp", 2:14)

sum_vydaje_pp2$label_col <- ifelse(sum_vydaje_pp2$nakladove_stredisko == "Magistrát města Brna", "red", "black")

# hide_legend(ggplotly(
# ggplot(sum_vydaje_pp, 
#        aes(x = nakladove_stredisko, y = vydaje_pp, fill = factor(skupina))) + 
#     geom_bar(stat = "identity") + labs(x = "Městská část", y = "Výdaje na obyvatele") + 
#     coord_flip() + theme_minimal() + theme(axis.text.y = element_text(colour = sum_vydaje_pp$label_col))
# ))

sum_vydaje_pp2$vydaje_pp %<>% round(2)
sum_vydaje_pp2$note <- paste0(sum_vydaje_pp$skupina, ": ", sum_vydaje_pp2$vydaje_pp, " Kč")
vydaje_pp_plot <- 
    ggplot(sum_vydaje_pp2,
       aes(x = nakladove_stredisko, y = vydaje_pp, fill = factor(skupina),
       tooltip = note)) +
        geom_bar_interactive(stat = "identity") + labs(x = "Městská část", y = "Výdaje na obyvatele") +
        coord_flip() + theme_minimal() + 
    theme(legend.position = "right", legend.title = element_text("none"), 
          axis.text.y = element_text(colour = ifelse(levels(sum_vydaje_pp2$nakladove_stredisko) == "Magistrát města Brna", "red", "black")))
    
ggiraph(code = {print(vydaje_pp_plot)})


## Prijmy
# options(scipen = 666)
prijmy_mc2016 <- subset(rozpocet, rok == 2016 & k_za_ucetni_jedn == "Ne" & rozdeleni == "Příjmy")
# & nakladove_stredisko != "Nákladové středisko Magistrát města Brna") 
prijmy_mc2016 <- prijmy_mc2016[!grepl("VHČ", prijmy_mc2016$nakladove_stredisko), ]
prijmy_mc2016 %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
prijmy_mc2016$nakladove_stredisko %<>% gsub(pattern = "Nákladové středisko ", replacement = "")

## Aggregace podle skupiny
sum_prijmy <- ddply(prijmy_mc2016, .(nakladove_stredisko, skupina), summarise, 
                    prijmy = abs(sum(cerpano)), 
                    schvaleno = abs(sum(rozpocet_schvaleny)), 
                    upraveno = abs(sum(rozpocet_upraveny)))

#sum_prijmy %<>% subset(skupina2 != "Bez zařazení")
sum_prijmy$skupina %<>% factor
sum_prijmy$nakladove_stredisko %<>% factor

## doplnit chybejici mestske casti, ktere meli prijmy 0 v dane oblasti
imput <- data.frame(name = levels(sum_prijmy$nakladove_stredisko), 
                    whatever = NA)
sum_prijmy <- ddply(sum_prijmy, .(skupina), function(x) dplyr::full_join(x, imput, 
                                                                         by = c("nakladove_stredisko"="name")))
sum_prijmy$skupina %<>% na.locf
sum_prijmy$prijmy[is.na(sum_prijmy$prijmy)] <- 0

## Celkove prijmy na skupina podle mestskych casti
# lapply(levels(sum_prijmy$skupina), 
#        function(x) ggplot(sum_prijmy[sum_prijmy$skupina == x, ], 
#                           aes(x = reorder(factor(nakladove_stredisko), prijmy), y = prijmy)) + 
#            geom_bar(stat = "identity") + xlab(x) + coord_flip())

## plotly chart - absolutni vydaje podle mestskych casti
# hide_legend(ggplotly(
#     ggplot(sum_vydaje[sum_vydaje$nakladove_stredisko != "Magistrát města Brna", ], 
#            aes(x = nakladove_stredisko, 
#                y = vydaje, fill = factor(skupina))) + geom_bar(stat = "identity") + 
#         coord_flip()))


# PRIJMY ------------------------------------------------------------------
## prijmy podle poctu obyvatel

mc_data <- read.csv("citizen-counts-brno2.csv", stringsAsFactors = FALSE)
colnames(mc_data) <- c("nazev", "pocet_obyv", "katastr", "pocet_zsj")
mc_data$katastr <- NULL
mc_data$nazev %<>% gsub(pattern = "Městská část", replacement ="MČ") %>% factor
all_brno <- data.frame(nazev = "Magistrát města Brna", 
                       pocet_obyv = sum(mc_data$pocet_obyv), 
                       pocet_zsj = sum(mc_data$pocet_zsj))
mc_data <- rbind(mc_data, all_brno)
mc_data$nazev %<>% as.character %>% factor
sum_prijmy$nakladove_stredisko %<>% as.character %>% factor
sum_prijmy$whatever <- NULL

#identical(levels(mc_data$nazev), levels(sum_prijmy$nakladove_stredisko))

sum_prijmy_pp <- inner_join(sum_prijmy, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_prijmy_pp$prijmy_pp <- with(sum_prijmy_pp, prijmy / pocet_obyv)

# lapply(levels(sum_prijmy_pp$skupina), 
#        function(x) ggplot(sum_prijmy_pp[sum_prijmy_pp$skupina == x, ], 
#                           aes(x = reorder(factor(nakladove_stredisko), prijmy_pp), y = prijmy_pp)) + 
#            geom_bar(stat = "identity") + xlab(x) + coord_flip())

sum_prijmy_pp_wide <- tidyr::spread(sum_prijmy_pp[, c("nakladove_stredisko", "skupina", "prijmy_pp")], 
                                    skupina, prijmy_pp)
sum_prijmy_pp_wide$nakladove_stredisko <- reorder(sum_prijmy_pp_wide$nakladove_stredisko, 
                                                  rowSums(sum_prijmy_pp_wide[, 2:12]))
sum_prijmy_pp2 <- tidyr::gather(sum_prijmy_pp_wide, "skupina", "prijmy_pp", 2:12)

sum_prijmy_pp2$label_col <- ifelse(sum_prijmy_pp2$nakladove_stredisko == "Magistrát města Brna", "red", "black")

# hide_legend(ggplotly(
# ggplot(sum_prijmy_pp, 
#        aes(x = nakladove_stredisko, y = prijmy_pp, fill = factor(skupina))) + 
#     geom_bar(stat = "identity") + labs(x = "Městská část", y = "Výdaje na obyvatele") + 
#     coord_flip() + theme_minimal() + theme(axis.text.y = element_text(colour = sum_prijmy_pp$label_col))
# ))

sum_prijmy_pp2$prijmy_pp %<>% round(2)
sum_prijmy_pp2$note <- paste0(sum_prijmy_pp$skupina, ": ", sum_prijmy_pp2$prijmy_pp, " Kč")
sum_prijmy_pp2$skupina %<>% gsub(pattern = "[0-9]*\\s-\\s", replacement = "")
sum_prijmy_pp2$skupina[sum_prijmy_pp2$skupina == "Příjmy z prodeje dlouhodobého majetku a ostatní kapitálové příjmy"] <- 
    "Příjmy z prodeje dlouhodobého majetku\n a ostatní kapitálové příjmy"
sum_prijmy_pp2$skupina[sum_prijmy_pp2$skupina == "Příjmy z prodeje nekapitálového majetku a ostatní nedaňové příjmy"] <- 
    "Příjmy z prodeje nekapitálového majetku\n a ostatní nedaňové příjmy"
sum_prijmy_pp2$skupina[sum_prijmy_pp2$skupina == "Příjmy z vlastní činnosti a odvody přebytků organizací s přímým vztahem"] <- 
    "Příjmy z vlastní činnosti a odvody přebytků\n organizací s přímým vztahem"

# sum_prijmy_pp2$skupina %<>% 
prijmy_pp_plot <- 
    ggplot(sum_prijmy_pp2,
           aes(x = nakladove_stredisko, y = prijmy_pp, fill = factor(skupina),
               tooltip = note)) +
    geom_bar_interactive(stat = "identity") + labs(x = "Městská část", y = "Příjmy na obyvatele") +
    coord_flip() + theme_minimal() + 
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.box = "horizontal", 
          legend.text = element_text(size = 8),
          legend.text.align = 0, 
          axis.text.y = element_text(colour = ifelse(levels(sum_prijmy_pp2$nakladove_stredisko) == "Magistrát města Brna", "red", "black"))) + 
    guides(fill=guide_legend(ncol = 2))


ggiraph(code = {print(prijmy_pp_plot)})


# POMER SCHVALENO / UPRAVENO ----------------------------------------------

## schvaleno / upraveno

vydaje_mc <- ddply(sum_vydaje, "nakladove_stredisko", summarise, 
         schvaleno = sum(schvaleno, na.rm = TRUE), 
         upraveno = sum(upraveno, na.rm = TRUE), 
         vydaje = sum(vydaje, na.rm = TRUE))

vydaje_mc$vydaj_schvaleno <- with(vydaje_mc, vydaje / schvaleno)
vydaje_mc$vydaj_upraveno <- with(vydaje_mc, vydaje / upraveno)

ggplot(vydaje_mc, aes(x = reorder(nakladove_stredisko, vydaj_schvaleno), 
                      y = vydaj_schvaleno)) + geom_point() + 
    coord_flip() + labs(x = "Městská část", y = "Skutečné výdaje / schválený rozpočet")

options(scipen = 10000000)
vydaje_dodrzeni <- ggplot(vydaje_mc[vydaje_mc$nakladove_stredisko != "Magistrát města Brna", ], 
       aes(x = schvaleno, y = vydaje, tooltip = nakladove_stredisko)) + geom_point_interactive() + 
    geom_abline(intercept = 0, slope = 1, colour = "blue") + 
    labs(x = "Schválený rozpočet", y = "Skutečné výdaje")
ggiraph(code = {print(vydaje_dodrzeni)})


# ggplot(vydaje_mc, aes(x = reorder(nakladove_stredisko, vydaj_upraveno), 
#                       y = vydaj_upraveno)) + geom_point() + 
#     coord_flip()
# ggplot(vydaje_mc, aes(x = schvaleno, y = upraveno)) + geom_point()

## bubble chart

prijmy_bubble <- ddply(sum_prijmy_pp, .(nakladove_stredisko, pocet_obyv), summarise, 
                       prijmy = sum(prijmy), prijmy_pp = sum(prijmy_pp))
vydaje_bubble <- ddply(sum_vydaje_pp, .(nakladove_stredisko, pocet_obyv), summarise, 
                       vydaje = sum(vydaje), vydaje_pp = sum(vydaje_pp))

bubble_data <- full_join(vydaje_bubble, prijmy_bubble[, c("nakladove_stredisko", 
                                                          "prijmy_pp")], 
                         by = "nakladove_stredisko")


bubble <- ggplot(bubble_data[bubble_data$nakladove_stredisko != "Magistrát města Brna", ], 
                 aes(x = vydaje_pp, y = prijmy_pp, colour = nakladove_stredisko, 
                                  tooltip = nakladove_stredisko)) + 
    geom_point_interactive(aes(size = pocet_obyv)) + guides(colour = "none") + 
    labs(x = "Výdaje na obyvatele", y = "Příjmy na obyvatele") + 
    xlim(c(0, 42000)) + ylim(c(0, 42000))
    
ggiraph(code = {print(bubble)})

hide_legend(ggplotly(ggplot(bubble_data[bubble_data$nakladove_stredisko != "Magistrát města Brna", ], 
                 aes(x = vydaje_pp, y = prijmy_pp, colour = nakladove_stredisko)) + 
    geom_point(aes(size = pocet_obyv)) + guides(colour = "none") + 
    labs(x = "Výdaje na obyvatele", y = "Příjmy na obyvatele") + 
    xlim(c(0, 42000)) + ylim(c(0, 42000)) + theme_minimal()))



# ANIMATE -----------------------------------------------------------------

animate_data <- ddply(vydaje_mc2016, "mesic", summarise, 
                      cerpano = sum(-cerpano), 
                      schvaleno = sum(-rozpocet_schvaleny), 
                      upraveno = sum(-rozpocet_upraveny))

animate_data$cerpano %<>% cumsum
animate_data$schvaleno %<>% cumsum
animate_data$upraveno %<>% cumsum

# devtools::install_github("dgrtwo/gganimate")
an_plot <- ggplot(animate_data, aes(x = mesic, y = cerpano, frame = mesic, cumulative = TRUE)) + 
    geom_point() + geom_line() + geom_hline(yintercept = animate_data$schvaleno[1])

an_plot <- ggplot(animate_data, aes(x = mesic, y = cerpano, frame = mesic, cumulative = TRUE)) + 
    geom_point() + geom_line() + geom_hline(yintercept = animate_data$schvaleno[1]) + 
    geom_point(aes(x = mesic, y = upraveno), colour = "red") + 
    geom_line(aes(x = mesic, y = upraveno), colour = "red") 


gganimate(an_plot, interval = 1.0, file = "animate_plot.gif")


animate_data2 <- ddply(vydaje_mc2016, .(mesic, nakladove_stredisko), summarise, 
                      cerpano = sum(-cerpano), 
                      schvaleno = sum(-rozpocet_schvaleny), 
                      upraveno = sum(-rozpocet_upraveny))
animate_data2 %<>% subset(nakladove_stredisko != "Magistrát města Brna")
# animate_data2 <- ddply(animate_data2, .(nakladove_stredisko), transform, 
#                        schvaleno = schvaleno[1])

animate_data2 <- ddply(animate_data2, .(nakladove_stredisko), transform,
                       cerpano = cumsum(cerpano),
                       schvaleno = cumsum(schvaleno),
                       upraveno = cumsum(upraveno))
lvls <- arrange(animate_data2[animate_data2$mesic == 12, ], cerpano)$nakladove_stredisko

## doplnit schvaleny a upraveny rozpocet
mc_animate <- ggplot(animate_data2, aes(x = factor(nakladove_stredisko, levels = lvls), y = cerpano, frame = mesic)) + 
    geom_bar(stat = "identity", position = "identity") + 
    geom_point(aes(x = nakladove_stredisko, y = schvaleno), pch = 124, colour = "black") + 
    geom_point(aes(x = nakladove_stredisko, y = upraveno), pch = 124, colour = "red") + 
    scale_y_continuous(labels = c("200 mil. Kč", "400 mil. Kč", "600 mil. Kč"), 
                       breaks = c(200000000, 400000000, 600000000)) + 
    coord_flip() + labs(x = "Městská část", y = "Čerpání rozpočtu")

gganimate(mc_animate, interval = 1, file = "mc_animate.gif")
# gganimate(mc_animate, interval = 1, file = "mc_animate.mp4")

## Kontrola
sum_mc <- ddply(vydaje_mc2016, "nakladove_stredisko", summarise, 
                sum(cerpano), sum(rozpocet_schvaleny), sum(rozpocet_upraveny))

## Copy-pasta
# oddil -------------------------------------------------------------------
## Agregace podle oddilu
sum_vydaje <- ddply(vydaje_mc2016, .(nakladove_stredisko, oddil), summarise, 
                    vydaje = abs(sum(cerpano)))
#sum_vydaje %<>% subset(skupina2 != "Bez zařazení")
sum_vydaje$oddil %<>% factor
sum_vydaje$nakladove_stredisko %<>% factor

## doplnit chybejici mestske casti, ktere meli vydaje 0 v dane oblasti
imput <- data.frame(name = levels(sum_vydaje$nakladove_stredisko), 
                    whatever = NA)
sum_vydaje <- ddply(sum_vydaje, .(oddil), function(x) dplyr::full_join(x, imput, 
                                                                         by = c("nakladove_stredisko"="name")))
sum_vydaje$oddil %<>% na.locf
sum_vydaje$vydaje[is.na(sum_vydaje$vydaje)] <- 0

## Celkove vydaje na oddilu podle mestskych casti
lapply(levels(sum_vydaje$oddil), 
       function(x) ggplot(sum_vydaje[sum_vydaje$oddil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje), y = vydaje)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

hide_legend(ggplotly(
    ggplot(sum_vydaje, 
           aes(x = nakladove_stredisko, y = vydaje, fill = factor(oddil))) + 
        geom_bar(stat = "identity") + coord_flip()
))


## Vydaje podle poctu obyvatel
sum_vydaje_pp <- dplyr::full_join(sum_vydaje, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_vydaje_pp$vydaje_pp <- with(sum_vydaje_pp, vydaje / pocet_obyv)

lapply(levels(sum_vydaje_pp$oddil), 
       function(x) ggplot(sum_vydaje_pp[sum_vydaje_pp$oddil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje_pp), y = vydaje_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

hide_legend(ggplotly(
    ggplot(sum_vydaje_pp, aes(x = nakladove_stredisko, 
                           y = vydaje_pp, fill = factor(oddil))) + geom_bar(stat = "identity") + 
        coord_flip() + labs(x = "Městská část", y = "Výdaje na 1 obyvatele")))

# pododdil -------------------------------------------------------------------
## Agregace podle pododdilu
sum_vydaje <- ddply(vydaje_mc2016, .(nakladove_stredisko, pododdil), summarise, 
                    vydaje = abs(sum(cerpano)))
#sum_vydaje %<>% subset(skupina2 != "Bez zařazení")
sum_vydaje %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
sum_vydaje$pododdil %<>% factor
sum_vydaje$nakladove_stredisko %<>% factor

## doplnit chybejici mestske casti, ktere meli vydaje 0 v dane oblasti
imput <- data.frame(name = levels(sum_vydaje$nakladove_stredisko), 
                    whatever = NA)
sum_vydaje <- ddply(sum_vydaje, .(pododdil), function(x) dplyr::full_join(x, imput, 
                                                                       by = c("nakladove_stredisko"="name")))
sum_vydaje$pododdil %<>% na.locf
sum_vydaje$vydaje[is.na(sum_vydaje$vydaje)] <- 0

## Celkove vydaje na oddilu podle mestskych casti
lapply(levels(sum_vydaje$pododdil), 
       function(x) ggplot(sum_vydaje[sum_vydaje$pododdil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje), y = vydaje)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

## Vydaje podle poctu obyvatel
sum_vydaje_pp <- dplyr::full_join(sum_vydaje, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_vydaje_pp$vydaje_pp <- with(sum_vydaje_pp, vydaje / pocet_obyv)

lapply(levels(sum_vydaje_pp$pododdil), 
       function(x) ggplot(sum_vydaje_pp[sum_vydaje_pp$pododdil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje_pp), y = vydaje_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

