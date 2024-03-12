########################  TP3bis  #######################

rm(list=ls())
install.packages("openxlsx")
install.packages("mapsf")

library(sf)
library(dplyr)
library(mapsf)
library(classInt)
library(leaflet)

##### Exercice 1 #####

### Question 1
# Import des donnees 
# Fond communes France metropolitaine
communes_fm<- st_read("TP2.R/fonds/commune_francemetro_2021.shp", options = "ENCODING=WINDOWS-1252") %>% 
  select(code,libelle,surf)
# Import des population légales des communes en 2019
pop_com_2019<-openxlsx::read.xlsx("TP2.R/fonds/Pop_legales_2019.xlsx")
# Correction pour la ville de Paris
pop_com_2019<-pop_com_2019 %>% 
  mutate(COM=if_else(substr(COM,1,3)=="751","75056",COM)) %>% 
  group_by(code=COM) %>% 
  summarise(pop=sum(PMUN19))
# Jointure
communes_fm<-communes_fm %>% 
  left_join(pop_com_2019,
            by="code") %>% 
  mutate(densite=pop/surf)


### Question 2
summary(communes_fm$densite)
hist(communes_fm$densite)


### Question 3
plot(communes_fm["densite"], border=FALSE)


### Question 4
plot(communes_fm["densite"], breaks="quantile", main="quantile", border = FALSE)
plot(communes_fm["densite"], breaks="sd", main="sd", border = FALSE)
plot(communes_fm["densite"], breaks="jenks", main="jenks", border = FALSE)
plot(communes_fm["densite"], breaks="pretty", main="pretty", border = FALSE)


### Question 5
#a
densInt <- classIntervals(
  communes_fm$densite,
  style = "quantile",
  n = 5
)
summary(densInt)
str(densInt)
show(densInt$brks)

#b
pal1 <- RColorBrewer::brewer.pal(n = 5, name = "YlOrRd")
plot(densInt, main = "Densité de population", pal = pal1, border = FALSE)

#c
analyser_discret <- function(method, nb_classes){
  denspop_c <- classIntervals(
    communes_fm$densite,
    style = method, 
    n = nb_classes
  )
  print(denspop_c$brks)
  plot(
    denspop_c,
    pal = pal1,
    main = method
  )
  return(denspop_c)
}
# Avec cinq classes:
all_discret <- sapply(c("quantile", "sd","pretty","jenks"), analyser_discret, nb_classes = 5)

#d
denspop_man_brks5 <- c(0,40,162,1000,8000,27200)
popcomfm_sf <- communes_fm %>%
  mutate(
    densite_c = cut(
      densite,
      breaks = denspop_man_brks5,
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )
  )

#e
table(popcomfm_sf$densite_c)
pal2 <- c(
  RColorBrewer::brewer.pal(
    n=5,
    name="Greens"
  )[4:3],
  RColorBrewer::brewer.pal(
    n=5,
    name="YlOrRd"
  )[c(2,4:5)]
)
plot(
  popcomfm_sf["densite_c"], 
  pal=pal2, 
  border = FALSE,
  main = "Densité de population",
)



##### Exercice 2 #####

### Question 1
dep_fm <- st_read("TP2.R/fonds/dep_francemetro_2021.shp")
tx_pauvrete <- openxlsx::read.xlsx("TP2.R/fonds/Taux_pauvrete_2018.xlsx")
mer<-st_read("TP2.R/fonds/merf_2021.shp")

str(dep_fm)
summary(dep_fm)

dep_fm_pauv<-dep_fm %>% 
  left_join(tx_pauvrete %>% select(-Dept),
            by=c("code"="Code"))

# Methode de Fisher
mf_map(x = dep_fm_pauv, 
       var = "Tx_pauvrete", 
       type = "choro",
       nbreaks = 4,
       breaks= "jenks"
)
# Methode des classes de même amplitude
mf_map(x = dep_fm_pauv, 
       var = "Tx_pauvrete", 
       type = "choro",
       nbreaks = 4,
       breaks= "equal"
)
# Methode des quantiles
mf_map(x = dep_fm_pauv, 
       var = "Tx_pauvrete", 
       type = "choro",
       nbreaks = 4,
       breaks= "quantile"
)

### Question 2
tx_pauv_brks5 <- c(0,13,17,25,max(dep_fm_pauv$Tx_pauvrete))
# Methode de Fisher
mf_map(x = dep_fm_pauv, 
       var = "Tx_pauvrete", 
       type = "choro",
       breaks= tx_pauv_brks5
)
mf_inset_on(x = dep_fm_pauv , pos = "topright", 
            cex = .2)
# (sinon Paris et sa couronne restent très petits)
mf_init(dep_fm_pauv %>%
          filter(code %in% c("75","92","93","94")))
# On recrée la carte choroplethe sur Paris et sa couronne
mf_map(dep_fm_pauv %>% 
         filter(code %in% c("75","92","93","94")), 
       var = "Tx_pauvrete", 
       type = "choro",
       breaks= c(0,13,17,25,max(dep_fm_pauv$tx_pauvrete)),
       # Ne pas faire apparaître la legende de l'encadre
       leg_pos = NA,
       # Ne pas oublier le add=TRUE pour superposer l'encadre à notre carte
       add = TRUE)
# Rajout des codes départements
mf_label(dep_fm_pauv %>% 
           filter(code %in% c("75","92","93","94")),
         var = "code", 
         col = "black")
# Fin de l'encadre (fermeture)
mf_inset_off()

couleur<-rev(mf_get_pal(4,"Mint"))
mf_legend(
  type="choro",
  title = "Taux de pauvreté",
  # Creation de labels personnalisés
  # On est oblige de mettre une modalite fictive (ici "") pour avoir une tranche
  val=c("","Moins de 13","De 13 à moins de 17","De 17 à moins de 25","25 ou plus"),
  pal=couleur,
  pos = "left"
)
# Si l'on veut rajouter la mer
mf_map(mer, add=TRUE)
mf_layout(title = "Taux de pauvreté par département en 2018",
          credits = "Source : Insee")



##### Exercice 3 #####



