library(ggplot2)

# NA REPLACE #######################################
rozpocet <- readRDS("rozpocet.rds")

rozpocet_kons <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ano")

# Vyfiltruj data pro mestke casti
rozpocet <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ne") %>% # municipality
  group_by(rok, mesic, ucetni_stredisko, nakladove_stredisko, rozdeleni, trida, skupina, podskupina,
           polozka, skupina2, oddil, pododdil, paragraf, ucetni_stredisko4, org, suau, orj, 
           k_za_ucetni_jedn, k_smb, rozpocet_schvaleny, rozpocet_upraveny) %>%
  summarise(cerpano = sum(cerpano)) %>%
  group_by(rok, mesic, ucetni_stredisko, nakladove_stredisko, rozdeleni, trida, skupina, podskupina,
           polozka, skupina2, oddil, pododdil, paragraf, ucetni_stredisko4, org, suau, orj, 
           k_za_ucetni_jedn, k_smb) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny),
            rozpocet_upraveny = sum(rozpocet_upraveny),
            cerpano = sum(cerpano)) %>%
  ungroup()


observed <- c("5 - Běžné výdaje", "6 - Kapitálové výdaje")
# Vyfiltruj data pro mestke casti
rozpocet_2016 <- rozpocet %>%
  filter(rok == 2016 & trida %in% observed) %>%
  filter(k_za_ucetni_jedn == "Ne") %>% # municipality
  group_by(nakladove_stredisko, trida) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny),
            rozpocet_upraveny = sum(rozpocet_upraveny),
            cerpano = sum(cerpano)) %>%
  ungroup()

write.csv(rozpocet_2016, "rozpocet_2016.csv")
