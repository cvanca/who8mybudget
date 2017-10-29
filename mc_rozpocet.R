## Vydaje podle mestskych casti

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zoo)
library(ggiraph)
library(gganimate)
library(plotly)

## Nacist data, odstranit vedlejsi hospodarskou cinnost mesta
options(scipen = 666)
rozpocet <- read.csv("data/rozpocet.csv")
vydaje_mc2016 <- subset(rozpocet, rok == 2016 & k_za_ucetni_jedn == "Ne" & rozdeleni == "Výdaje")
vydaje_mc2016 <- vydaje_mc2016[!grepl("VHČ", vydaje_mc2016$nakladove_stredisko), ]
vydaje_mc2016 %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
vydaje_mc2016$nakladove_stredisko %<>% gsub(pattern = "Nákladové středisko ", replacement = "")

## Agregace podle skupiny
sum_vydaje <- ddply(vydaje_mc2016, .(nakladove_stredisko, skupina), summarise, 
                    vydaje = abs(sum(cerpano)), 
                    schvaleno = abs(sum(rozpocet_schvaleny)), 
                    upraveno = abs(sum(rozpocet_upraveny)))

sum_vydaje$skupina %<>% factor
sum_vydaje$nakladove_stredisko %<>% factor

## Doplnit chybejici mestske casti, ktere meli vydaje 0 v dane oblasti
imput <- data.frame(name = levels(sum_vydaje$nakladove_stredisko), 
                    whatever = NA)
sum_vydaje <- ddply(sum_vydaje, .(skupina), function(x) dplyr::full_join(x, imput, 
                                                                         by = c("nakladove_stredisko"="name")))
sum_vydaje$skupina %<>% na.locf
sum_vydaje$vydaje[is.na(sum_vydaje$vydaje)] <- 0

## plotly chart - absolutni vydaje podle mestskych casti
# hide_legend(ggplotly(
#     ggplot(sum_vydaje[sum_vydaje$nakladove_stredisko != "Magistrát města Brna", ], 
#            aes(x = nakladove_stredisko, 
#                        y = vydaje, fill = factor(skupina))) + geom_bar(stat = "identity") + 
#     coord_flip()))
sum_vydaje$note <- paste0(sum_vydaje$skupina, ": ", sum_vydaje$vydaje, " Kč")

vydaje_plot <- 
    ggplot(sum_vydaje[sum_vydaje$nakladove_stredisko != "Magistrát města Brna", ],
           aes(x = nakladove_stredisko, y = vydaje, fill = factor(skupina),
               tooltip = note)) +
    geom_bar_interactive(stat = "identity") + 
    labs(x = "Městská část", y = "Výdaje", 
         title = "Výdaje podle městských částí") +
    coord_flip() + theme_minimal() + 
    theme(legend.title = element_blank(),  
          legend.position = "bottom", 
          legend.text = element_text(size = 5),
          legend.text.align = 0, 
          legend.margin = margin(0, 0, 0, -50, unit = "pt")) + 
    guides(fill=guide_legend(ncol = 2, keywidth = 1, keyheight = 1))

ggiraph(code = {print(vydaje_plot)})

## Vydaje podle poctu obyvatel

mc_data <- read.csv("data/citizen-counts-brno2.csv", stringsAsFactors = FALSE)
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

sum_vydaje_pp <- inner_join(sum_vydaje, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_vydaje_pp$vydaje_pp <- with(sum_vydaje_pp, vydaje / pocet_obyv)

sum_vydaje_pp_wide <- tidyr::spread(sum_vydaje_pp[, c("nakladove_stredisko", "skupina", "vydaje_pp")], 
                                    skupina, vydaje_pp)
sum_vydaje_pp_wide$nakladove_stredisko <- reorder(sum_vydaje_pp_wide$nakladove_stredisko, 
                                                  rowSums(sum_vydaje_pp_wide[, 2:14]))
sum_vydaje_pp2 <- tidyr::gather(sum_vydaje_pp_wide, "skupina", "vydaje_pp", 2:14)

sum_vydaje_pp2$label_col <- ifelse(sum_vydaje_pp2$nakladove_stredisko == "Magistrát města Brna", "red", "black")
sum_vydaje_pp2$skupina %<>% gsub(pattern = "[0-9]*\\s-\\s", replacement = "")
sum_vydaje_pp2$skupina[sum_vydaje_pp2$skupina == "Neinvestiční transfery veřejnoprávním subjektům a mezi peněžními fondy téhož subjektu"] <- 
    "Neinvestiční transfery veřejnoprávním subjektům\na mezi peněžními fondy téhož subjektu"

## Vydaje na obyvatele podle mestskych casti
sum_vydaje_pp2$vydaje_pp %<>% round(2)
sum_vydaje_pp2$note <- paste0(sum_vydaje_pp$skupina, ": ", sum_vydaje_pp2$vydaje_pp, " Kč")
vydaje_pp_plot <- 
    ggplot(sum_vydaje_pp2,
           aes(x = nakladove_stredisko, y = vydaje_pp, fill = factor(skupina),
               tooltip = note)) +
    geom_bar_interactive(stat = "identity") + 
    labs(x = "Městská část", y = "Výdaje na obyvatele", 
         title = "Výdaje na obyvatele podle městských částí") +
    coord_flip() + theme_minimal() + 
    theme(legend.title = element_blank(),  
          legend.position = "bottom", 
          legend.text = element_text(size = 5),
          legend.text.align = 0, 
          legend.margin = margin(0, 0, 0, -50, unit = "pt"), 
          axis.text.y = element_text(colour = ifelse(levels(sum_vydaje_pp2$nakladove_stredisko) == "Magistrát města Brna", "red", "black"))) + 
    guides(fill=guide_legend(ncol = 2, keywidth = 1, keyheight = 1))

ggiraph(code = {print(vydaje_pp_plot)})


# PRIJMY ------------------------------------------------------------------

## Prijmy
prijmy_mc2016 <- subset(rozpocet, rok == 2016 & k_za_ucetni_jedn == "Ne" & rozdeleni == "Příjmy")
prijmy_mc2016 <- prijmy_mc2016[!grepl("VHČ", prijmy_mc2016$nakladove_stredisko), ]
prijmy_mc2016 %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
prijmy_mc2016$nakladove_stredisko %<>% gsub(pattern = "Nákladové středisko ", replacement = "")

## Agregace podle skupiny
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

## prijmy podle poctu obyvatel

sum_prijmy_pp <- inner_join(sum_prijmy, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_prijmy_pp$prijmy_pp <- with(sum_prijmy_pp, prijmy / pocet_obyv)

sum_prijmy_pp_wide <- tidyr::spread(sum_prijmy_pp[, c("nakladove_stredisko", "skupina", "prijmy_pp")], 
                                    skupina, prijmy_pp)
sum_prijmy_pp_wide$nakladove_stredisko <- reorder(sum_prijmy_pp_wide$nakladove_stredisko, 
                                                  rowSums(sum_prijmy_pp_wide[, 2:12]))
sum_prijmy_pp2 <- tidyr::gather(sum_prijmy_pp_wide, "skupina", "prijmy_pp", 2:12)

sum_prijmy_pp2$label_col <- ifelse(sum_prijmy_pp2$nakladove_stredisko == "Magistrát města Brna", "red", "black")

sum_prijmy_pp2$prijmy_pp %<>% round(2)
sum_prijmy_pp2$note <- paste0(sum_prijmy_pp$skupina, ": ", sum_prijmy_pp2$prijmy_pp, " Kč")
sum_prijmy_pp2$skupina %<>% gsub(pattern = "[0-9]*\\s-\\s", replacement = "")
sum_prijmy_pp2$skupina[sum_prijmy_pp2$skupina == "Příjmy z prodeje dlouhodobého majetku a ostatní kapitálové příjmy"] <- 
    "Příjmy z prodeje dlouhodobého majetku\n a ostatní kapitálové příjmy"
sum_prijmy_pp2$skupina[sum_prijmy_pp2$skupina == "Příjmy z prodeje nekapitálového majetku a ostatní nedaňové příjmy"] <- 
    "Příjmy z prodeje nekapitálového majetku\n a ostatní nedaňové příjmy"
sum_prijmy_pp2$skupina[sum_prijmy_pp2$skupina == "Příjmy z vlastní činnosti a odvody přebytků organizací s přímým vztahem"] <- 
    "Příjmy z vlastní činnosti a odvody přebytků\n organizací s přímým vztahem"

prijmy_pp_plot <- 
    ggplot(sum_prijmy_pp2,
           aes(x = nakladove_stredisko, y = prijmy_pp, fill = factor(skupina),
               tooltip = note)) +
    geom_bar_interactive(stat = "identity") + 
    labs(x = "Městská část", y = "Příjmy na obyvatele", 
         title = "Příjmy na obyvatele podle městských částí") +
    coord_flip() + theme_minimal() + 
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.title = element_blank(),
          legend.box = "horizontal", 
          legend.text = element_text(size = 5),
          legend.text.align = 0, 
          legend.margin = margin(0, 0, 0, -50, unit = "pt"), 
          axis.text.y = element_text(colour = ifelse(levels(sum_prijmy_pp2$nakladove_stredisko) == "Magistrát města Brna", "red", "black"))) + 
    guides(fill=guide_legend(ncol = 2, keywidth = 1, keyheight = 1))


ggiraph(code = {print(prijmy_pp_plot)})

# POMER SCHVALENO / UPRAVENO ----------------------------------------------
## schvaleno / upraveno

vydaje_mc <- ddply(sum_vydaje, "nakladove_stredisko", summarise, 
                   schvaleno = sum(schvaleno, na.rm = TRUE), 
                   upraveno = sum(upraveno, na.rm = TRUE), 
                   vydaje = sum(vydaje, na.rm = TRUE))

vydaje_mc$vydaj_schvaleno <- with(vydaje_mc, vydaje / schvaleno)
vydaje_mc$vydaj_upraveno <- with(vydaje_mc, vydaje / upraveno)

vydaje_dodrzeni <- ggplot(vydaje_mc[vydaje_mc$nakladove_stredisko != "Magistrát města Brna", ], 
                          aes(x = schvaleno / 1000000, y = vydaje / 1000000, tooltip = nakladove_stredisko)) + geom_point_interactive() + 
    geom_abline(intercept = 0, slope = 1, colour = "gray65", alpha = 0.4) + 
    labs(x = "Schválený rozpočet (mil. Kč)", y = "Skutečné výdaje (mil. Kč)", 
         title = "Rozdíl mezi schváleným a upraveným rozpočtem (v mil. Kč)") + 
    coord_fixed(xlim = c(0, 520), ylim = c(0, 520)) + theme_minimal()

ggiraph(code = {print(vydaje_dodrzeni)})


# BUBBLE CHART ------------------------------------------------------------
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
    labs(x = "Výdaje na obyvatele (Kč)", y = "Příjmy na obyvatele (Kč)", 
         title = "Příjmy a výdaje na obyvatele městských částí") + coord_fixed() + 
    xlim(c(0, 40000)) + ylim(c(0, 40000)) + scale_size_continuous(name = "Počet obyvatel") + 
    geom_abline(intercept = 0, slope = 1, colour = "gray65", alpha = 0.4) + 
    theme_minimal() + theme(legend.position = "bottom")

ggiraph(code = {print(bubble)})

## Bez Jehnic (outlier)
bubble2 <- ggplot(bubble_data[bubble_data$nakladove_stredisko != "Magistrát města Brna", ], 
                  aes(x = vydaje_pp, y = prijmy_pp, colour = nakladove_stredisko, 
                      tooltip = nakladove_stredisko)) + 
    geom_point_interactive(aes(size = pocet_obyv)) + guides(colour = "none") + 
    labs(x = "Výdaje na obyvatele (Kč)", y = "Příjmy na obyvatele (Kč)", 
         title = "Příjmy a výdaje na obyvatele městských částí") + coord_fixed() + 
    xlim(c(0, 15000)) + ylim(c(0, 15000)) + scale_size_continuous(name = "Počet obyvatel") + 
    geom_abline(intercept = 0, slope = 1, colour = "gray65", alpha = 0.4) + 
    theme_minimal() + theme(legend.position = "bottom")

ggiraph(code = {print(bubble2)})

# ANIMATE -----------------------------------------------------------------

animate_data <- ddply(vydaje_mc2016, "mesic", summarise, 
                      cerpano = sum(-cerpano), 
                      schvaleno = sum(-rozpocet_schvaleny), 
                      upraveno = sum(-rozpocet_upraveny))

animate_data$cerpano %<>% cumsum
animate_data$schvaleno %<>% cumsum
animate_data$upraveno %<>% cumsum

## budget of the whole Brno (cerne cerpani, cervene upraveny rozpocet)
an_plot <- ggplot(animate_data, aes(x = mesic, y = cerpano, frame = mesic, cumulative = TRUE)) + 
    geom_point() + geom_line() + geom_hline(yintercept = animate_data$schvaleno[1]) + 
    geom_point(aes(x = mesic, y = upraveno), colour = "red") + 
    geom_line(aes(x = mesic, y = upraveno), colour = "red") + scale_x_continuous(breaks = 1:12) + 
    labs(x = "Měsíc", y = "Celkové výdaje (v mld. Kč)", title = "Čerpání výdajů Brna") + theme_minimal() + 
    scale_y_continuous(breaks = c(5000000000, 10000000000, 15000000000), 
                       labels = c(5, 10, 15))

gganimate(an_plot, interval = 1.0, file = "animate_plot.gif")

animate_data2 <- ddply(vydaje_mc2016, .(mesic, nakladove_stredisko), summarise, 
                       cerpano = sum(-cerpano), 
                       schvaleno = sum(-rozpocet_schvaleny), 
                       upraveno = sum(-rozpocet_upraveny))
animate_data2 %<>% subset(nakladove_stredisko != "Magistrát města Brna")
animate_data2 <- ddply(animate_data2, .(nakladove_stredisko), transform,
                       cerpano = cumsum(cerpano),
                       schvaleno = cumsum(schvaleno),
                       upraveno = cumsum(upraveno))
lvls <- arrange(animate_data2[animate_data2$mesic == 12, ], cerpano)$nakladove_stredisko

## Cerpani rozpoctu podle mestskych casti v case
animate_data2$mesic %<>% factor(levels = 1:12, 
                                labels = paste0("2016 ", c("leden", "únor", "březen", "duben", 
                                                           "květen", "červen", "červenec", "srpen", 
                                                           "září", "říjen", "listopad", "prosinec")))
mc_animate <- ggplot(animate_data2, aes(x = factor(nakladove_stredisko, levels = lvls), y = cerpano, frame = mesic)) + 
    geom_bar(stat = "identity", position = "identity", fill = "#8F5AE0") + 
    geom_point(aes(x = nakladove_stredisko, y = schvaleno), pch = 1, colour = "black") + 
    geom_point(aes(x = nakladove_stredisko, y = upraveno), pch = 1, colour = "red") + 
    scale_y_continuous(labels = c("200 mil. Kč", "400 mil. Kč", "600 mil. Kč"), 
                       breaks = c(200000000, 400000000, 600000000)) + 
    coord_flip() + labs(x = "Městská část", y = "Čerpání rozpočtu") + 
    ggtitle("Čerpání rozpočtu po městských částech:\n")

gganimate(mc_animate, interval = 1, file = "graphs/mc_animate_absolute.gif")

## Cerpani rozpoctu v relativnich cislech
animate_data2 <- ddply(vydaje_mc2016, .(mesic, nakladove_stredisko), summarise, 
                       cerpano = sum(-cerpano), 
                       schvaleno = sum(-rozpocet_schvaleny), 
                       upraveno = sum(-rozpocet_upraveny))
animate_data2 %<>% subset(nakladove_stredisko != "Magistrát města Brna")
animate_data2 <- ddply(animate_data2, .(nakladove_stredisko), transform,
                       cerpano = cumsum(cerpano),
                       schvaleno = cumsum(schvaleno),
                       upraveno = cumsum(upraveno))

animate_data2$cerpano_rel <- round(with(animate_data2, cerpano / schvaleno) * 100, 2)
animate_data2$upraveno_rel <- round(with(animate_data2, upraveno / schvaleno) * 100, 2)

lvls_rel <- arrange(animate_data2[animate_data2$mesic == 12, ], cerpano_rel)$nakladove_stredisko

animate_data2$mesic %<>% factor(levels = 1:12, 
                                labels = paste0("2016 ", c("leden", "únor", "březen", "duben", "květen", 
                                                           "červen", "červenec", "srpen", "září", "říjen", 
                                                           "listopad", "prosinec")))
mc_animate2 <- ggplot(animate_data2, aes(x = factor(nakladove_stredisko, levels = lvls_rel), 
                                         y = cerpano_rel, frame = mesic)) + 
    geom_bar(stat = "identity", position = "identity", fill = "#8F5AE0") + 
    geom_bar(aes(y = upraveno_rel), stat = "identity", position = "identity", alpha = 0.4, 
             fill = "#8F5AE0") + 
    geom_hline(yintercept = 100) + 
    coord_flip() + labs(x = "Městská část", y = "Čerpání rozpočtu") + 
    ggtitle("Čerpání rozpočtu po městských částech:\n") + 
    scale_y_continuous(breaks = seq(0, 600, length.out = 4), 
                       labels = paste0(as.character(seq(0, 600, length.out = 4)), " %"))

# gganimate(mc_animate2)
gganimate(mc_animate2, interval = 1, file = "graphs/mc_animate_relative.gif")

# KAPITALOVE VYDAJE -------------------------------------------------------
kapital_vydaj <- read.csv("data/kapitalove_vydaje.csv")
kapital_vydaj$nekapital <- with(kapital_vydaj, 1 - podil_cerpano)
kapital_vydaj$misto30 %<>% gsub(pattern = "Nákladové středisko ", replacement = "")
kapital_vydaj$misto30 %<>% factor %>% reorder(kapital_vydaj$podil_cerpano)
kapital_long <- tidyr::gather(kapital_vydaj, "vydaj", "hodnota", 6:7)
kapital_long$hodnota %<>% `*`(100)
kapital_long$note <- paste0(ifelse(kapital_long$vydaj == "nekapital", "Ostatní výdaje: ", "Kapitálové výdaje: "), kapital_long$hodnota, " %") 
kapital_long$col_label <- ifelse(kapital_long$misto30 == "Magistrát města Brna", "red", "black")

kapital_plot <- ggplot(kapital_long, aes(x = misto30, y = hodnota, fill = vydaj, tooltip = note)) + 
    geom_bar_interactive(stat = "identity") + 
    labs(x = "Městská část", y = "Procento výdajů", 
         title = "Podíl kapitálových výdajů na celkových výdajích") +
    coord_flip() + theme_minimal() + 
    theme(axis.text.y = element_text(
        colour = ifelse(levels(kapital_long$misto30) == "Magistrát města Brna", "red", "black")), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        plot.title = element_text(margin(0, 0, 0, -50, unit = "pt"))) + 
    scale_fill_discrete(breaks = c("podil_cerpano", "nekapital"), 
                        labels = c("Kapitálové výdaje", "Ostatní výdaje")) + 
    scale_y_continuous(breaks = seq(0, 100, length.out = 5), 
                       labels = paste0(as.character(seq(0, 100, length.out = 5)), " %"))

ggiraph(code = {print(kapital_plot)})

## Cerpani rozpoctu podle odboru MMB
# podle ORJ ---------------------------------------------------------

orj_2016 <- ddply(vydaje_mc2016[vydaje_mc2016$nakladove_stredisko == "Magistrát města Brna", ], 
                  .(orj, mesic), 
                  summarise, 
                  cerpano = sum(cerpano), 
                  schvaleno = sum(rozpocet_schvaleny),
                  upraveno = sum(rozpocet_upraveny))

orj_2016 %<>% subset(orj != "0000000000")
orj_2016 %<>% subset(orj != "0000008889 - Nespecifikované příjmy z pokladny k rozúčtování")
orj_2016$orj %<>% gsub(pattern = "[0-9]{1,}\\s-\\s", replacement = "")

imput <- data.frame(mesic = 1:12, 
                    whatever = NA)
orj_2016_2 <- ddply(orj_2016, .(orj), 
                    function(x) dplyr::full_join(x, imput, 
                                                 by = "mesic"))
orj_2016_2$orj %<>% na.locf
orj_2016_2$whatever <- NULL
orj_2016_2[is.na(orj_2016_2)] <- 0
orj_2016_2 %<>% arrange(orj, mesic)

animate_orj <- ddply(orj_2016_2, .(orj), transform,
                     cerpano = cumsum(-cerpano),
                     schvaleno = cumsum(-schvaleno),
                     upraveno = cumsum(-upraveno))
animate_orj %<>% subset(orj != "DPH - reverse charge")
lvls <- arrange(animate_orj[animate_orj$mesic == 12, ], cerpano)$orj

animate_orj$mesic %<>% factor(levels = 1:12, 
                              labels = paste0("2016 ", c("leden", "únor", "březen", "duben", "květen", 
                                                         "červen", "červenec", "srpen", "září", "říjen", 
                                                         "listopad", "prosinec")))

mc_animate_orj <- ggplot(animate_orj, aes(x = factor(orj, levels = lvls), y = cerpano, frame = mesic)) + 
    geom_bar(stat = "identity", position = "identity", fill = "#8F5AE0") + 
    geom_point(aes(x = orj, y = schvaleno), pch = 1, colour = "black") + 
    geom_point(aes(x = orj, y = upraveno), pch = 1, colour = "red") + 
    coord_flip() + labs(x = "Odbor MMB", y = "Čerpání rozpočtu (mld. Kč)") + 
    ggtitle("Čerpání rozpočty odborů magistrátu:\n") + 
    scale_y_continuous(breaks = c(0, 1000000000, 2000000000), labels = c(0, 1, 2))

# gganimate(mc_animate_orj)
gganimate(mc_animate_orj, interval = 1, file = "graphs/mc_animate_orj.gif")

# ORJ RELATIVE ------------------------------------------------------------

animate_orj$cerpano_rel <- round(with(animate_orj, cerpano / schvaleno) * 100, 2)
animate_orj$schvaleno_rel <- 100
animate_orj$upraveno_rel <- round(with(animate_orj, upraveno / schvaleno) * 100, 2)

lvls_rel <- arrange(animate_orj[animate_orj$mesic == "2016 prosinec", ], cerpano_rel)$orj

mc_animate_orj_rel <- ggplot(animate_orj, aes(x = factor(orj, lvls_rel), 
                                              y = cerpano_rel, frame = mesic)) + 
    geom_bar(stat = "identity", position = "identity") + 
    geom_bar(aes(y = upraveno_rel), alpha = 0.25,  stat = "identity", position = "identity", 
             fill = "#8F5AE0") + 
    coord_flip() + labs(x = "Odbor MMB", y = "Čerpání rozpočtu") + 
    ggtitle("Rozpočty podle odboru:") + 
    geom_hline(yintercept = 100) + 
    scale_y_continuous(breaks = seq(0, 150, length.out = 4), 
                       labels = paste0(as.character(seq(0, 150, length.out = 4)), " %")) + 
    theme_minimal() 

gganimate(mc_animate_orj_rel, interval = 1, file = "graphs/mc_animate_orj_rel.gif")
