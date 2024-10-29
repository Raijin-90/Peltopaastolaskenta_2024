
#Muutos 23/8/24: 
#Poistetaan N2O laskennasta

library(tidyverse);library(here);library(openxlsx)


#GTK data
library(readxl)
CO2_tuotekertoimet_gtk <-
  read_excel(
    "Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_gtk.xlsx",
    sheet = "Yksittaiset_tuotteet"
  )

Tuotekertoimet_gtk <- CO2_tuotekertoimet_gtk 


#Dropataan "Kesannot yms" sekä "Muut kasvit

Tuotekertoimet_gtk <-
  Tuotekertoimet_gtk %>% filter(!(Tuoteryhmä %in% c("Kesannot,laitumet,yms.", "Muut")))

Tuotekertoimet_gtk <-
  Tuotekertoimet_gtk %>% mutate(Tuhatta_euroa = Euroa_per_tuote / 1000) #EUR-> kEUR


Tuotekertoimet_gtk <-
  Tuotekertoimet_gtk %>% mutate(
    Paastokerroin_t_CO2eq_kEUR = CO2_tonnia / Tuhatta_euroa,
    Paastokerroin_t_CO2eq_t = CO2_tonnia / Satotonnia
  )

Tuotekertoimet_gtk$Laskuri <- 1

#Satomääräinen kerroin irti. Voidaan laskea vain niistä joilla satokerroin on. 
#Myös ne pois, joille hintadata puuttuu

Tuotekertoimet_gtk <-
  Tuotekertoimet_gtk %>%  filter(!(is.na(Satotonnia)) & !(is.na(Tuhatta_euroa)))

#Lasketaan kertoimen hajonta kasviryhmittäin


Hajonta_gtk <-
  Tuotekertoimet_gtk %>% group_by(Tuoteryhmä) %>% summarise(Hajonta_satokerroin_gtk = sd(Paastokerroin_t_CO2eq_t),
                                                                 Hajonta_eurokerroin_gtk = sd(Paastokerroin_t_CO2eq_kEUR))


rm(CO2_tuotekertoimet_gtk)


#Viljavuusdata


library(readxl)
CO2_tuotekertoimet_viljav <- read_excel("Output/Yksinkertaistettu_intensiteettilaskenta/CO2_tuotekertoimet_viljav.xlsx", 
                                     sheet = "Yksittaiset_tuotteet")
Tuotekertoimet_viljav<-CO2_tuotekertoimet_viljav


Tuotekertoimet_viljav <-
  Tuotekertoimet_viljav %>% filter(!(Tuoteryhmä %in% c("Kesannot,laitumet,yms.", "Muut")))


Tuotekertoimet_viljav <-
  Tuotekertoimet_viljav %>% mutate(Tuhatta_euroa = Euroa_per_tuote / 1000) #EUR-> kEUR


Tuotekertoimet_viljav <-
  Tuotekertoimet_viljav %>% mutate(
    Paastokerroin_t_CO2eq_kEUR = CO2_tonnia / Tuhatta_euroa,
    Paastokerroin_t_CO2eq_t = CO2_tonnia / Satotonnia
  )

Tuotekertoimet_viljav$Laskuri <- 1

#Satomääräinen kerroin irti. Voidaan laskea vain niistä joilla satokerroin on. 
#Myös ne pois, joille hintadata puuttuu

Tuotekertoimet_viljav <-
  Tuotekertoimet_viljav %>%  filter(!(is.na(Satotonnia)) & !(is.na(Tuhatta_euroa)))

#Tuoteryhman sisainen hajonta kummastakin kertoimesta


Hajonta_viljav <-
  Tuotekertoimet_viljav %>% group_by(Tuoteryhmä) %>% summarise(Hajonta_satokerroin = sd(Paastokerroin_t_CO2eq_t),
                                                               Hajonta_eurokerroin = sd(Paastokerroin_t_CO2eq_kEUR))



rm(CO2_tuotekertoimet_viljav)



#Rinnastetaan t/t kertoimet gtk-aineistosta ja viljavuudesta. Lasketaan kerroin tuoteryhmälle, sekä  kerrointen hajonta ryhmän sisällä

#Satomääräinen gtk kerroin
                                                                         
a<-Tuotekertoimet_gtk %>% group_by(Tuoteryhmä) %>% summarise(Satotonnia_yht = sum(Satotonnia), 
                                                               CO2eq_t_yht = sum(CO2_tonnia),
                                                               Tuotteita_ryhmassa = sum(Laskuri))

a<-a %>% mutate(Paastokerroin_t_CO2eq_t = CO2eq_t_yht/Satotonnia_yht)


#Hajonnan liittäminen

a<-a %>% inner_join(Hajonta_gtk, by="Tuoteryhmä") 
a<-a %>% select(-7)

colnames(a)<-  c(
  "Tuoteryhmä",
  "Satonnia_gtk",
  "CO2eq_t_yht_gtk",
  "Tuotteita_ryhmassa",
  "Paastokerroin_t_CO2eq_t_gtk",
  "Hajonta_satokerroin_gtk"
  
)

a<-a %>% select(1:3,5:6,4)


library(openxlsx)
taulukointi<-createWorkbook()
addWorksheet(taulukointi, "gtk_tuotekerrointaulukko_sato")
writeData(taulukointi,"gtk_tuotekerrointaulukko_sato",a )


#Euroille gtk kerroin


b<-Tuotekertoimet_gtk %>% group_by(Tuoteryhmä) %>% summarise(Tuhatta_euroa = sum(Tuhatta_euroa), 
                                                             CO2eq_t_yht = sum(CO2_tonnia),
                                                             Tuotteita_ryhmassa = sum(Laskuri))


b<-b %>% mutate(Paastokerroin_t_CO2eq_keur = CO2eq_t_yht/Tuhatta_euroa)

#Hajonnan liittäminen

b<-b %>% inner_join(Hajonta_gtk, by="Tuoteryhmä") 
b<-b %>% select(-6)

colnames(b)<-  c(
  "Tuoteryhmä",
  "tuhatta_euroa",
  "CO2eq_t_yht_gtk",
  "Tuotteita_ryhmassa",
  "Paastokerroin_t_CO2eq_keur_gtk",
  "Hajonta_eurokerroin_gtk"
  
)

b<-b %>% select(1:3,5:6,4)

addWorksheet(taulukointi, "gtk_tuotekerrointaulukko_euro")
writeData(taulukointi,"gtk_tuotekerrointaulukko_euro",b )

saveWorkbook(taulukointi, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/GTK_tuotekerrointaulu.xlsx"), overwrite = T)



#Satomääräinen viljavuuskerroin

c<-Tuotekertoimet_viljav %>%  group_by(Tuoteryhmä) %>% summarise(Satotonnia_yht = sum(Satotonnia), 
                                                                   CO2eq_t_yht = sum(CO2_tonnia),
                                                                   Tuotteita_ryhmassa = sum(Laskuri))
c<-c %>% mutate(Paastokerroin_t_CO2eq_t = CO2eq_t_yht/Satotonnia_yht) 


#Hajonnan liittäminen

c<-c %>% inner_join(Hajonta_viljav, by="Tuoteryhmä")
c<-c %>% select(-7)
c<-c %>% select(1:3,5:6,4)

colnames(c) <-
  c(
    "Tuoteryhmä",
    "Satonnia_viljav",
    "CO2eq_t_yht_viljav",
    "Paastokerroin_t_CO2eq_t_viljav",
    "Hajonta_satokerroin_viljav",
    "Tuotteita_ryhmassa_viljav"
  )

library(openxlsx)
taulukointi<-createWorkbook()
addWorksheet(taulukointi, "viljav_tuotekerrointaulukko")
writeData(taulukointi,"viljav_tuotekerrointaulukko",c )



#Euroille viljavuuskerroin


d<-Tuotekertoimet_viljav %>% group_by(Tuoteryhmä) %>% summarise(Tuhatta_euroa = sum(Tuhatta_euroa), 
                                                             CO2eq_t_yht = sum(CO2_tonnia),
                                                             Tuotteita_ryhmassa = sum(Laskuri))


d<-d %>% mutate(Paastokerroin_t_CO2eq_keur = CO2eq_t_yht/Tuhatta_euroa)

#Hajonnan liittäminen

d<-d %>% inner_join(Hajonta_viljav, by="Tuoteryhmä") 
d<-d %>% select(-6)

colnames(d)<-  c(
  "Tuoteryhmä",
  "tuhatta_euroa",
  "CO2eq_t_yht_viljav",
  "Tuotteita_ryhmassa",
  "Paastokerroin_t_CO2eq_keur_viljav",
  "Hajonta_eurokerroin_viljav"
  
)

d<-d %>% select(1:3,5:6,4)

addWorksheet(taulukointi, "viljav_tuotekerrointaulu_eur")
writeData(taulukointi,"viljav_tuotekerrointaulu_eur",d )

saveWorkbook(taulukointi, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/viljav_tuotekerroint.xlsx"), overwrite = T)







#RInnastuskuva




#Rinnastetaan satomäärään suhteutettu kerroin           

gtk_vert<-a %>% select(1,4,5,6)               
viljav_vert<-c %>% select(1,4,5)               
               
Vertailu<-inner_join(gtk_vert, viljav_vert, by="Tuoteryhmä")
Vertailu<-Vertailu %>% select(1:3,5:6,4)


Vertailu<-Vertailu %>% mutate(Tuoteryhmä = case_when(Tuoteryhmä == "Hedelmät ja marjat" ~ "Fruits & berries",
                                          Tuoteryhmä == "Palkokasvit" ~ "Legumes",
                                          Tuoteryhmä == "Peruna" ~ "Potato",
                                          Tuoteryhmä == "Rehukasvit ja rehunurmet" ~ "Fodder crops",
                                          Tuoteryhmä == "Sokerijuurikas" ~ "Sugar beet",
                                          Tuoteryhmä == "Vihannekset, juurekset mausteet ja yrtit" ~ "Vegetables, root vegetables, spices & herbs",
                                          Tuoteryhmä == "Viljat" ~ "Cereals",
                                          Tuoteryhmä == "Öljykasvit" ~ "Oilseed crops"))
  
  

write.xlsx(Vertailu, file=here("Output/Yksinkertaistettu_intensiteettilaskenta/Kerroinvertailu_tuotteille_1306.xlsx"))




#Kuva

a<-Vertailu %>% select(1,2) 
colnames(a)<-c("RAC","Coefficient")  
a$Dataset <-"Geospatial"

b<-Vertailu %>% select(1,3) 
colnames(b)<-c("RAC","SD")  
b$Dataset <-"Geospatial"

x<-merge(a, b, by=c("RAC","Dataset")) #Kuvan tarvitsema muoto, gtk-osa



a<-Vertailu %>% select(1,4) 
colnames(a)<-c("RAC","Coefficient")  
a$Dataset <-"Soil fertility"

b<-Vertailu %>% select(1,5) 
colnames(b)<-c("RAC","SD")  
b$Dataset <-"Soil fertility"


z<-merge(a, b, by=c("RAC","Dataset")) # Villainous


#yhdistetään molemmat datat kuvan piirtämistä varten7
#install.packages("scales")
library(scales)

Kuva<-rbind(z,x)

library(scales)
Kuva %>% ggplot(mapping = aes(x = RAC, y = Coefficient, fill = Dataset)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_grey()+
  geom_errorbar(
    aes(ymin = Coefficient, ymax = Coefficient + SD),
    width = .2,
    position = position_dodge(.9)
  ) +
  ylab(expression("tn" ~ "CO"["2"] ~ "-eq." ~ "tn" ^ -1)) +  
  xlab("Crop")+
  scale_x_discrete(labels = label_wrap(10))+
  theme_classic() 
 ggsave(filename="Kerroinvertailu_bw.tiff", dpi = 1200, path = here("Output/Grafiikka"))
  
#Potut ja sugarbeet pelkkään taulukkoon? Nyt niiden välistä eroa ei näe


