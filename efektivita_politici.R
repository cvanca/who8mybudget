setwd('c:/Dropbox/github_hackathon_2017/who8mybudget/')

efCerp <- read.csv("rozpocty_polit.csv", header = TRUE, sep = ",", encoding = "UTF-8")

barva <- c("ANO" = "#400080", "CSSD" =  "#ff9900", "KDU" = "#ffff00", "NK" = "#808080", "ODS" = "#0000cc",
           "SOL" = "#009933", "STAN" = "#0d0d0d", "SZ" = "#00ff00", "ZB" = "#ff66cc")

efektivitaStarostu <- ggplot(efCerp, aes(x = reorder(misto30, efektivitacerpani6), y = efektivitacerpani6, fill = factor(star30))) +
  geom_bar(stat = "identity") + scale_fill_manual(values = barva) +
  coord_flip()  + labs(x = "Městská část", 
                       y = "Efektivita čerpání kapitálových výdajů oproti upravenému rozpočtu", 
                       title = "Efektivita čerpání kapitálových výdajů",
                       position = 'center')+
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) +
  guides(fill=guide_legend(title = "Politická příslušnost"))
  
 ggiraph({print(efektivitaStarostu)})
  
 
 
 
 ####
 
 
 podil_cerpani_zdroj <- read.csv("podil_kapital.csv", header = TRUE, encoding = "UTF-8", sep = ",")
 
 
 podil_cerpani <- ggplot(podil_cerpani_zdroj, aes(x = reorder(misto30, podil_cerpano), y = podil_cerpano, fill = factor(star30))) +
   geom_bar(stat = "identity") + scale_fill_manual(values = barva) +
   coord_flip()  + labs(x = "Městská část", 
                        y = "Podíl kapitálových výdajů na celkových výdajích", 
                        title = "Podíl kapitálových výdajů na celkových výdajích",
                        position = 'center')+
   scale_y_continuous(expand = c(0,0), limits = c(0, 1)) +
  
   guides(fill=guide_legend(title = "Politická příslušnost"))
 
 ggiraph(code = {print(podil_cerpani)})
 
 ####
 
 efCerp <- read.csv("rozpocty_polit.csv", header = TRUE, encoding = "UTF-8")
 
 barva <- c("ANO" = "#400080", "CSSD" =  "#ff9900", "KDU" = "#ffff00", "NK" = "#808080", "ODS" = "#0000cc",
            "SOL" = "#009933", "STAN" = "#0d0d0d", "SZ" = "#00ff00", "ZB" = "#ff66cc")
 
boxplot_cerpani_kapitalu <- ggplot(efCerp, aes(x = star30, y = efektivitacerpani6, group(star30), fill = star30)) +
   geom_boxplot() + scale_fill_manual(values = barva)+ stat_boxplot(geom = 'errorbar')+
  labs(x = "Radnice se starostou dané politické strany", 
       y = "Efektivita čerpání kapitálových výdajů oproti upravenému rozpočtu", 
       title = "Efektivita čerpání kapitálových výdajů") +
  guides(fill=guide_legend(title = "Politická příslušnost"))
  
 
 ggiraph({print(boxplot_cerpani_kapitalu) })
 
 ggplot(podil_cerpani_zdroj, aes(x = star30, y = podil_cerpano, group(star30), fill = star30)) +
   geom_boxplot() + scale_fill_manual(values = barva)+ stat_boxplot(geom = 'errorbar')+
   labs(x = "Radnice se starostou dané politické strany", 
        y = "Podíl kapitálových výdajů na celkových výdajích", 
        title = "Podíl kapitálových výdajů na celkových výdajích") +
   guides(fill=guide_legend(title = "Politická příslušnost"))
 