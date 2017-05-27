library(ggplot2)
library(tidyr)
library(RColorBrewer)

rozpocet <- readRDS("rozpocet.rds")

rozpocet_kons <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ano" & k_smb == "Ano")

# Vyfiltruj data pro mesto
rozpocet <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
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

# Chobotnice
rozpocet_chobotnice <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  group_by(podskupina) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))

# Monthly granularity
rozpocet_chobotnice <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  rename(variable = podskupina) %>%
  group_by(rok, mesic, variable) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano)) %>%
  select(rok, mesic, variable, cerpano) %>%
  filter(cerpano != 0) %>%
  mutate(io = ifelse(cerpano > 0, "i", "o"),
         month = paste(io, mesic, sep = "_")) %>%
  ungroup() %>%
  select(-io, -mesic) %>%
  spread(month, cerpano) %>%
  mutate(type = "podskupina")

write.csv(rozpocet_chobotnice, "rozpocet_podskupina.csv")
chobotnice_all <- rozpocet_chobotnice

rozpocet_chobotnice <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  group_by(pododdil) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))

# Monthly granularity
rozpocet_chobotnice <- rozpocet %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  rename(variable = pododdil) %>%
  group_by(rok, mesic, variable) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano)) %>%
  select(rok, mesic, variable, cerpano) %>%
  filter(cerpano != 0) %>%
  mutate(io = ifelse(cerpano > 0, "I", "O"),
         month = paste0(io, mesic)) %>%
  ungroup() %>%
  select(-io, -mesic) %>%
  spread(month, cerpano) %>%
  mutate(type = "pododdil")
write.csv(rozpocet_chobotnice, "rozpocet_pododdil.csv")

chobotnice_all <- rbind(chobotnice_all, rozpocet_chobotnice)

chobotnice_all[is.na(chobotnice_all)] <- 0
write.csv(chobotnice_all, "chobotnice_all.csv", row.names = F)

# Yearly totals
rozpocet_2016 <- rozpocet %>%
  filter(rok == 2016) %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  group_by(nakladove_stredisko) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano))
write.csv(rozpocet_2016, "rozpocet_2016.csv")

rozpocet_2016 <- rozpocet %>%
  ungroup() %>%
  filter(rok == 2016) %>%
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  group_by(trida,
    skupina2, oddil, pododdil) %>%
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
  filter(k_za_ucetni_jedn == "Ne" & k_smb == "Ne") %>% # mesto
  group_by(rok, mesic, trida) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(cerpano)) %>%
  mutate(type = ifelse(trida %in% prijmy, "in", "out"),
         trida = gsub("[[:digit:]]", "", trida),
         trida = trimws(gsub("[[:punct:]]", "", trida)),
         cerpano = cerpano/1000000000,
         rok_mesic = yearmonth(rok, mesic)) %>%
  group_by(trida) %>%
  mutate(cumsum = cumsum(cerpano))

# Visualise
options(scipen=1)
ggplot(trida_agg, 
       aes(rok_mesic, cerpano,
           group = trida, fill = trida)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5),
                     name = "Obrat v miliardách Kč") +
  scale_x_date(name = "Datum",
               date_minor_breaks = "month") +
  ggtitle("Rozpočet města Brna (2016-01 až 2017-04) v mld. Kč") +
  scale_fill_brewer(name = "Tok", 
                    palette = "Set2") +
  geom_area() +
  theme_bw()
ggsave("rozpocet_area.jpg", width = 8, height = 5)

# Visualise cumsum
options(scipen=1)
trida_agg %>%
  filter(rok == 2016) %>%
  ggplot(aes(rok_mesic, cumsum,
           group = trida, fill = trida)) +
  scale_y_continuous(breaks = seq(-13, 13, by = 1),
                     name = "Obrat v miliardách Kč") +
  scale_x_date(name = "Datum",
               date_minor_breaks = "month") +
  ggtitle("Rozpočet města Brna za 2016 v mld. Kč") +
  scale_fill_brewer(name = "Tok", 
                    palette = "Set2") +
  geom_area() +
  theme_bw()
ggsave("rozpocet_area_cumsum.jpg", width = 8, height = 5)


ggplot(trida_agg, 
       aes(rok_mesic, cerpano,
           group = trida, fill = type)) +
  geom_area()


type_agg <- trida_agg %>%
  group_by(rok_mesic, rok, type) %>%
  summarise(rozpocet_schvaleny = sum(rozpocet_schvaleny), 
            rozpocet_upraveny = sum(rozpocet_upraveny), 
            cerpano = sum(abs(cerpano))) %>%
  group_by(type) %>%
  mutate(cumsum = cumsum(cerpano))

ggplot(type_agg, 
       aes(rok_mesic, cerpano, color = type)) +
  scale_y_continuous(breaks = seq(-2, 2, by = 0.5),
                     name = "Obrat v miliardách Kč",
                     limits = c(0, 2)) +
  scale_x_date(name = "Datum",
               date_minor_breaks = "month") +
  ggtitle("Rozpočet města Brna (2016-01 až 2017-04) v mld. Kč") +
  geom_vline(xintercept = as.numeric(as.Date("2016-12-01"), linetype = 4)) +
  scale_color_discrete(name = "Tok") +
  geom_line() +
  theme_bw()
ggsave("rozpocet_line.jpg", width = 8, height = 5)

type_agg %>%
  filter(rok == 2016) %>%
  ggplot(aes(rok_mesic, cumsum, color = type)) +
  scale_y_continuous(breaks = seq(0, 13, by = 1),
                     name = "Obrat v miliardách Kč",
                     limits = c(0, 13)) +
  scale_x_date(name = "Datum",
               date_minor_breaks = "month") +
  ggtitle("Rozpočet města Brna (2016-01 až 2017-04) v mld. Kč") +
  scale_color_discrete(name = "Tok") +
  geom_line() +
  theme_bw()
ggsave("rozpocet_line_cumsum.jpg", width = 8, height = 5)
