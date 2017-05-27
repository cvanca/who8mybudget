## Vydaje podle mestskych casti

library(plyr)
library(magrittr)
library(ggplot2)
library(zoo)

vydaje_mc2016 <- subset(rozpocet, rok == 2016 & k_za_ucetni_jedn == "Ne" & rozdeleni == "Výdaje"
                     & nakladove_stredisko != "Nákladové středisko Magistrát města Brna") 
vydaje_mc2016 <- vydaje_mc2016[!grepl("VHČ", vydaje_mc2016$nakladove_stredisko), ]
vydaje_mc2016 %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")

## Aggregace podle skupiny
sum_vydaje <- ddply(vydaje_mc2016, .(nakladove_stredisko, skupina), summarise, 
                    vydaje = abs(sum(cerpano)))
#sum_vydaje %<>% subset(skupina2 != "Bez zařazení")
sum_vydaje %<>% subset(nakladove_stredisko != "Nákladové středisko - Jídelna")
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
lapply(levels(sum_vydaje$skupina), 
       function(x) ggplot(sum_vydaje[sum_vydaje$skupina == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje), y = vydaje)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())

library(plotly)
ggplotly(
    ggplot(sum_vydaje, aes(x = nakladove_stredisko, 
                       y = vydaje, fill = factor(skupina))) + geom_bar(stat = "identity") + 
    coord_flip())

## Vydaje podle poctu obyvatel
mc_data <- read.csv("citizen-counts-brno.csv")
colnames(mc_data) <- c("nazev", "pocet_obyv", "katastr", "pocet_zsj")
mc_data$nazev %<>% gsub(pattern = "Městská část", replacement ="MČ")
sum_vydaje_pp <- dplyr::full_join(sum_vydaje, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_vydaje_pp$vydaje_pp <- with(sum_vydaje_pp, vydaje / pocet_obyv)


lapply(levels(sum_vydaje_pp$skupina), 
       function(x) ggplot(sum_vydaje_pp[sum_vydaje_pp$skupina == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje_pp), y = vydaje_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())



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

## Vydaje podle poctu obyvatel
sum_vydaje_pp <- dplyr::full_join(sum_vydaje, mc_data, by = c("nakladove_stredisko"="nazev"))
sum_vydaje_pp$vydaje_pp <- with(sum_vydaje_pp, vydaje / pocet_obyv)

lapply(levels(sum_vydaje_pp$oddil), 
       function(x) ggplot(sum_vydaje_pp[sum_vydaje_pp$oddil == x, ], 
                          aes(x = reorder(factor(nakladove_stredisko), vydaje_pp), y = vydaje_pp)) + 
           geom_bar(stat = "identity") + xlab(x) + coord_flip())



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

