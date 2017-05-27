## prijmy podle mestskych casti

library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(zoo)
library(plotly)

rozpocet <- read.csv("rozpocet.csv")
prijmy_mc2016 <- subset(rozpocet, rok == 2016 & k_za_ucetni_jedn == "Ne" & rozdeleni == "Příjmy"
                        & nakladove_stredisko != "Nákladové středisko Magistrát města Brna") 
prijmy_mc2016 <- prijmy_mc2016[!grepl("VHČ", prijmy_mc2016$nakladove_stredisko), ]
prijmy_mc2016 %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")

## Aggregace podle skupiny
sum_prijmy <- ddply(prijmy_mc2016, .(nakladove_stredisko, skupina), summarise, 
                    prijmy = abs(sum(cerpano)))
#sum_prijmy %<>% subset(skupina2 != "Bez zařazení")
sum_prijmy %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
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
lapply(levels(sum_prijmy$skupina), 
       function(x) ggplot(sum_prijmy[sum_prijmy$skupina == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), prijmy), y = prijmy)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

hide_legend(ggplotly(
    ggplot(sum_prijmy, aes(x = nakladove_stredisko, 
                           y = prijmy, fill = factor(skupina))) + geom_bar(stat = "identity") + 
        coord_flip()))

## prijmy podle poctu obyvatel
mc_data <- read.csv("citizen-counts-brno.csv")
colnames(mc_data) <- c("nazev", "pocet_obyv", "katastr", "pocet_zsj")
mc_data$nazev %<>% gsub(pattern = "Městská část", replacement ="MČ")
sum_prijmy_pp <- dplyr::full_join(sum_prijmy, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_prijmy_pp$prijmy_pp <- with(sum_prijmy_pp, prijmy / pocet_obyv)

lapply(levels(sum_prijmy_pp$skupina), 
       function(x) ggplot(sum_prijmy_pp[sum_prijmy_pp$skupina == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), prijmy_pp), y = prijmy_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

hide_legend(ggplotly(
    ggplot(sum_prijmy_pp, 
           aes(x = nakladove_stredisko, y = prijmy_pp, fill = factor(skupina))) + 
        geom_bar(stat = "identity") + labs(x = "Městská část", y = "Výdaje na obyvatele") + 
        coord_flip() 
))

## Copy-pasta
# oddil -------------------------------------------------------------------
## Agregace podle oddilu
sum_prijmy <- ddply(prijmy_mc2016, .(nakladove_stredisko, oddil), summarise, 
                    prijmy = abs(sum(cerpano)))
#sum_prijmy %<>% subset(skupina2 != "Bez zařazení")
sum_prijmy$oddil %<>% factor
sum_prijmy$nakladove_stredisko %<>% factor

## doplnit chybejici mestske casti, ktere meli prijmy 0 v dane oblasti
imput <- data.frame(name = levels(sum_prijmy$nakladove_stredisko), 
                    whatever = NA)
sum_prijmy <- ddply(sum_prijmy, .(oddil), function(x) dplyr::full_join(x, imput, 
                                                                       by = c("nakladove_stredisko"="name")))
sum_prijmy$oddil %<>% na.locf
sum_prijmy$prijmy[is.na(sum_prijmy$prijmy)] <- 0

## Celkove prijmy na oddilu podle mestskych casti
lapply(levels(sum_prijmy$oddil), 
       function(x) ggplot(sum_prijmy[sum_prijmy$oddil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), prijmy), y = prijmy)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

hide_legend(ggplotly(
    ggplot(sum_prijmy, 
           aes(x = nakladove_stredisko, y = prijmy, fill = factor(oddil))) + 
        geom_bar(stat = "identity") + coord_flip()
))


## prijmy podle poctu obyvatel
sum_prijmy_pp <- dplyr::full_join(sum_prijmy, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_prijmy_pp$prijmy_pp <- with(sum_prijmy_pp, prijmy / pocet_obyv)

lapply(levels(sum_prijmy_pp$oddil), 
       function(x) ggplot(sum_prijmy_pp[sum_prijmy_pp$oddil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), prijmy_pp), y = prijmy_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

hide_legend(ggplotly(
    ggplot(sum_prijmy_pp, aes(x = nakladove_stredisko, 
                              y = prijmy_pp, fill = factor(oddil))) + geom_bar(stat = "identity") + 
        coord_flip() + labs(x = "Městská část", y = "Výdaje na 1 obyvatele")))

# pododdil -------------------------------------------------------------------
## Agregace podle pododdilu
sum_prijmy <- ddply(prijmy_mc2016, .(nakladove_stredisko, pododdil), summarise, 
                    prijmy = abs(sum(cerpano)))
#sum_prijmy %<>% subset(skupina2 != "Bez zařazení")
sum_prijmy %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
sum_prijmy$pododdil %<>% factor
sum_prijmy$nakladove_stredisko %<>% factor

## doplnit chybejici mestske casti, ktere meli prijmy 0 v dane oblasti
imput <- data.frame(name = levels(sum_prijmy$nakladove_stredisko), 
                    whatever = NA)
sum_prijmy <- ddply(sum_prijmy, .(pododdil), function(x) dplyr::full_join(x, imput, 
                                                                          by = c("nakladove_stredisko"="name")))
sum_prijmy$pododdil %<>% na.locf
sum_prijmy$prijmy[is.na(sum_prijmy$prijmy)] <- 0

## Celkove prijmy na oddilu podle mestskych casti
lapply(levels(sum_prijmy$pododdil), 
       function(x) ggplot(sum_prijmy[sum_prijmy$pododdil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), prijmy), y = prijmy)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

## prijmy podle poctu obyvatel
sum_prijmy_pp <- dplyr::full_join(sum_prijmy, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_prijmy_pp$prijmy_pp <- with(sum_prijmy_pp, prijmy / pocet_obyv)

lapply(levels(sum_prijmy_pp$pododdil), 
       function(x) ggplot(sum_prijmy_pp[sum_prijmy_pp$pododdil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), prijmy_pp), y = prijmy_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

