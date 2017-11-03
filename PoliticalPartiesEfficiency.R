podil_kapital <- read.csv("data/podil_kapital.csv", header = TRUE)
kapvyd <- read.csv("data/rozpocty_polit.csv", header = TRUE)
polparrad <- read.csv("data/BrnoRadaMatrixRel.csv", header = TRUE)

attach(podil_kapital)
attach(kapvyd)
attach(polparrad)

tabmax<-cbind(polparrad,podil_cerpano,efektivitacerpani6)
tab23<-tabmax[cbind(-2,-6,-7,-10,-18,-25,-28),-1]
rownames(tab23)<-tab23[,1]
tab23<-tab23[,-1]

PolStran<-tab23[,-(11:12)]
hc1 = hclust(dist(PolStran))
plot(hc1)
library(vegan)
PCA<-rda(PolStran)
biplot(PCA,display="species")
biplot(PCA,display="sites")

hc <- hclust(dist(PolStran))
plot(hc)

library(ggplot2)
mtPolStran <- melt(t(PolStran))
a <- ggplot(mtPolStran, aes(x=Municipality, y=poměr,
                            fill=PolPar))

a+geom_bar(stat = "identity"
)+labs(title="Složení rad brněnských městských částí", x="Městská část",y="Složení rady (%)"
)+theme_minimal(
)+coord_flip(
)+guides(fill=guide_legend(title="Politická strana")
)+theme(plot.title = element_text(hjust = 0.5)
)+scale_y_reverse(
)+scale_fill_manual(values=c(
  "cyan","orange","yellow","red","blue",
  "brown","purple","green","pink","gray"))


fit <- lm(podil_cerpano ~ ANO+ČSSD+KDU.ČSL+KSČM+ODS+STAN+TOP.09+Zelení+ŽB+Nezávislí, data=tab23)
summary(fit)
fit2 <- lm(efektivitacerpani6 ~ ANO+ČSSD+KDU.ČSL+KSČM+ODS+STAN+TOP.09+Zelení+ŽB+Nezávislí, data=tab23)
summary(fit2)
fit3 <- lm(podil_cerpano ~ efektivitacerpani6)
summary(fit3)
plot(efektivitacerpani6, podil_cerpano, xlab="Efektivita čerpání upraveného rozpočtu", ylab="Podíl kapitálových výdajů")
abline(fit3, col="red")
