
# NA REPLACE #######################################
rozpocet$org[is.na(rozpocet$org)] <- 0
rozpocet$orj[is.na(rozpocet$orj)] <- 0

rozpocet_kons <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ano" | k_smb == "Ano")

rozpocet <- rozpocet %>%
  group_by(rok, rok_mesic, ucetni_stredisko, nakladove_stredisko, rozdeleni, trida, skupina, podskupina,
           polozka, skupina2, oddil, pododdil, paragraf, ucetni_stredisko4, org, suau, orj, 
           k_za_ucetni_jedn, k_smb, rozpocet_schvaleny, rozpocet_upraveny) %>%
  summarise(cerpano = sum(cerpano)) %>%
  group_by(rok, rok_mesic, ucetni_stredisko, nakladove_stredisko, rozdeleni, trida, skupina, podskupina,
           polozka, skupina2, oddil, pododdil, paragraf, ucetni_stredisko4, org, suau, orj, 
           k_za_ucetni_jedn, k_smb) %>%
  summarise(rok, rozpocet_schvaleny = sum(rozpocet_schvaleny),
            rozpocet_upraveny = sum(rozpocet_upraveny),
            cerpano = sum(cerpano)) %>%
  ungroup()

rozpocet_2016 <- rozpocet %>%
  ungroup() %>%
  filter(rok == 2016) %>%
  filter(k_smb != "Ano") %>%
  group_by(trida, skupina, podskupina, polozka) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))
write.csv(rozpocet_2016, "rozpocet_2016_a.csv")

rozpocet_2016 <- rozpocet %>%
  ungroup() %>%
  filter(rok == 2016) %>%
  filter(k_smb != "Ano") %>%
  group_by(trida,
    skupina2, oddil, pododdil, paragraf, orj) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))
write.csv(rozpocet_2016, "rozpocet_2016_b.csv")

rozpocet_2016 <- rozpocet_2016 %>%
  ungroup() %>%
  group_by(trida,
    skupina2, oddil, pododdil, paragraf) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))


# Aggregate by month and trida
trida_agg <- rozpocet %>%
  group_by(rok_mesic, trida) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))

library(ggplot2)
# Visualise
ggplot(trida_agg, 
       aes())



