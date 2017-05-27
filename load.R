library(dplyr)
library(readxl)


# LOAD DATA FROM https://datahub.io/dataset/rozpoctova-data
rozpocet <- read_excel("rozpoctovadata.xlsx")
names(rozpocet) <- c("rok", "mesic", "ucetni_stredisko", "nakladove_stredisko", "rozdeleni", "trida", "skupina", "podskupina", "polozka", "skupina2", "oddil", "pododdil", "paragraf", "ucetni_stredisko4", "org", "suau", "synteticky_ucet", "analyticky_ucet", "orj", "k_za_ucetni_jedn", "k_smb", "rozpocet_schvaleny", "rozpocet_upraveny", "cerpano")

# EXPLORE ###########################################
glimpse(rozpocet)
summary(rozpocet)

nakladove_stredisko <- data.frame(table(rozpocet$nakladove_stredisko))
rozdeleni <- data.frame(table(rozpocet$rozdeleni))
trida <- data.frame(table(rozpocet$trida))
polozka <- data.frame(table(rozpocet$polozka))
skupina <- data.frame(table(rozpocet$skupina))
skupina2 <- data.frame(table(rozpocet$skupina2))
podskupina <- data.frame(table(rozpocet$podskupina))
oddíl <- data.frame(table(rozpocet$oddil))
pododdíl <- data.frame(table(rozpocet$pododdil))
paragraf <- data.frame(table(rozpocet$paragraf))
org <- data.frame(table(rozpocet$org))
orj <- data.frame(table(rozpocet$orj))

table(rozpocet$cerpano > 0)
table(rozpocet$cerpano < 0)
table(is.na(rozpocet$cerpano))

table(rozpocet$rozpocet_upraveny > 0)
table(rozpocet$rozpocet_upraveny < 0)
table(is.na(rozpocet$rozpocet_upraveny))


# CLEAN ############################################

# Odfiltrovat vedlejsi hospodarskou cinnost (same 0)
vhc <- c("MČ Brno - Bystrc - VHČ, Obecní byty",
         "MČ Brno - Jundrov - VHČ, Obecní byty", 
         "MČ Brno - Královo Pole - VHČ Obecní byty",
         "MČ Brno - sever - VHČ Poliklinika Lesná", 
         "MČ Brno - střed - VHČ Obecní byty",
         "Nákladové středisko - Jídelna",
         "Nákladové středisko - VHČ DPH")

# Vytvorit sloupec ROK-MESIC-01
yearmonth <- function(year, month){
    as.Date(sprintf("%s-%s-01", year, month))
}

rozpocet$rok_mesic <- yearmonth(rozpocet$rok, rozpocet$mesic)

rozpocet <- rozpocet %>%
  filter(!nakladove_stredisko %in% vhc)

upraveny_rozpocet <- rozpocet %>%
  filter(rozpocet_schvaleny != rozpocet_upraveny)

rozpocet %>%
  group_by()


rozdeleni_bez_zarazeni <- rozpocet %>%
  filter(Rozdělení == "Bez zařazení")



# TODO
# agregovat radky, aby byly 3 sloupce s castkami
# prejmenovat na MU XXX


