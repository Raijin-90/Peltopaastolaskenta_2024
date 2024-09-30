library(here)
library(tidyverse)

# GTK-Datan taulukot ####
#Verrataan samalla tavoin laskettuja päästökertoimia perus (keskiarvo)-sato ja eurokertoimista, sekä eri vuosien kertoimista.
#Paljonko kerroin heittää, kun hehtaarisadon tai tuotoksen arvoa muutetaan? Muu ei vaihdu, viljelyalat ja päästökertoimet ovat samoja. 

#Vakioarvoilla tuotetut kertoimet t/t satoa ja t/1000e

library(readxl)
Peruskertoimet_sato <- read_excel("Output/Herkkyystarkastelu/Peruskertoimet_gtk.xlsx", 
                             sheet = "Satopohjaiset")


Peruskertoimet_euro <- read_excel("Output/Herkkyystarkastelu/Peruskertoimet_gtk.xlsx", 
                                  sheet = "Europohjaiset")
#Verrokkikertoimet

library(readxl)
Verrokkikertoimet_sato <- read_excel("Output/Herkkyystarkastelu/Herkkystarkastelu_verrokkikertoimet_gtk.xlsx", 
                                                  sheet = "Verrokkikertoimet_sato")

Verrokkikertoimet_euro <- read_excel("Output/Herkkyystarkastelu/Herkkystarkastelu_verrokkikertoimet_gtk.xlsx", 
                                     sheet = "Verrokkikertoimet_euro")


#Satopohjaiset kertoimet

a<-Peruskertoimet_sato
a<-a %>% select(2,5)

b<-Verrokkikertoimet_sato
b<-b %>% select(2, 8:length(b))

c<-merge(a, b, by="Kasvinimi")
c[2:length(c)]<-c[2:length(c)]<-round(c[2:length(c)], 1)

colnames(c)<-c("Crop","Baseline", "2016", "2017", "2018", "2019")

c<-c %>% pivot_longer(cols= 2:length(c), names_to = "Coeff_name", values_to = "Coeff")

Satopohjaiset<-c

Satopohjaiset<-Satopohjaiset %>% mutate(Crop = case_when(Crop == "Kaura" ~ "Oat",
                                                         Crop == "Kevätrypsi" ~ "Spring rape",
                                                         Crop == "Porkkana" ~ "Carrot", 
                                                         Crop == "Syysrypsi" ~ "Fall rape" ,
                                                         Crop == "Tarhaherne" ~ "Garden pea",
))


ord<-c("Baseline","2016","2017","2018","2019")



Satopohjaiset$Coeff_name<-as.factor(Satopohjaiset$Coeff_name)



Satotaulukko<-Satopohjaiset %>% pivot_wider(names_from = Crop, values_from = Coeff)


Satotaulukko<-gt(Satotaulukko) %>% 
  opt_stylize(style=1) %>%
  cols_align("left", columns = 1) %>%
  cols_align("center", columns = 2:length(Satotaulukko)) %>%
  tab_spanner(label=html("tn CO<sub>2</sub> tn<sup>-1</sup>"), 
              columns = everything()) %>%
  cols_label(Coeff_name = "Coefficient")
  

Graph1<-Satopohjaiset %>% 
  ggplot(aes(x=factor(Coeff_name, ord), y=Coeff, fill=Crop))+
  geom_bar(stat="identity")+
  facet_wrap(~Crop)+
  xlab(NULL)+
  ylab(expression("t"~"CO"[2]~"t"^"-1"))+
  theme(axis.text.x = element_text(angle = 50, hjust=1))+
  theme(legend.position = "none")

  

#Europohjaiset kertoimet

a<-Peruskertoimet_euro
a<-a %>% select(1,2,6)

b<-Verrokkikertoimet_euro
b<-b %>% select(1,2, 8:length(b))

c<-merge(a, b, by=c("Kasvinimi","Kasvikoodi"))
c[3:length(c)]<-round(c[3:length(c)], 1)

c<-c %>% select(-2)
colnames(c)<-c("Crop","Baseline", "2016", "2017", "2018", "2019")

c<-c %>% pivot_longer(cols= 2:length(c), names_to = "Coeff_name", values_to = "Coeff")

Europohjaiset<-c

Europohjaiset<-Europohjaiset %>% mutate(Crop = case_when(Crop == "Kaura" ~ "Oat",
                                          Crop == "Kevätrypsi" ~ "Spring rape",
                                          Crop == "Porkkana" ~ "Carrot", 
                                          Crop == "Syysrypsi" ~ "Fall rape" ,
                                          Crop == "Tarhaherne" ~ "Garden pea",
                                          ))

ord<-c("Baseline","2016","2017","2018","2019")

Graph2<-Europohjaiset %>% 
  ggplot(aes(x=factor(Coeff_name, ord), y=Coeff, fill=Crop))+
  geom_bar(stat="identity")+
  facet_wrap(~Crop)+
  xlab("Coefficient")+
  ylab(expression("t"~"CO"[2]~"1000"~"EUR"^~-1))+
  theme(axis.text.x = element_text(angle = 70, hjust=1))+
  theme(legend.position = "none")


library("ggpubr")
JoinedGraph<-ggarrange(Graph1, Graph2, nrow=2)

Eurotaulukko<-Europohjaiset %>% pivot_wider(names_from = Crop, values_from = Coeff)

Eurotaulukko<-gt(Eurotaulukko) %>% 
  opt_stylize(style=1, color = "green") %>%
  cols_align("left", columns = 1) %>%
  cols_align("center", columns = 2:length(Eurotaulukko)) %>%
  tab_spanner(label=html("tn CO<sub>2</sub> 1000 €<sup>-1</sup>"), 
              columns = everything()) %>%
  cols_label(Coeff_name = "Coefficient")


# Viljavuusdatan taulukot

#Jatko-osa: Sama homma ja samat koodit. Mutta tehdään viljavuusaineiston kautta. 

library(readxl)
Peruskertoimet_sato <- read_excel("Output/Herkkyystarkastelu/Peruskertoimet_viljav.xlsx", 
                                  sheet = "Satopohjaiset")


Peruskertoimet_euro <- read_excel("Output/Herkkyystarkastelu/Peruskertoimet_viljav.xlsx", 
                                  sheet = "Europohjaiset")
#Verrokkikertoimet

library(readxl)
Verrokkikertoimet_sato <- read_excel("Output/Herkkyystarkastelu/Herkkystarkastelu_verrokkikertoimet_viljav.xlsx", 
                                     sheet = "Verrokkikertoimet_sato")

Verrokkikertoimet_euro <- read_excel("Output/Herkkyystarkastelu/Herkkystarkastelu_verrokkikertoimet_viljav.xlsx", 
                                     sheet = "Verrokkikertoimet_euro")


#Satopohjaiset kertoimet

a<-Peruskertoimet_sato
a<-a %>% select(2,5)

b<-Verrokkikertoimet_sato
b<-b %>% select(2, 8:length(b))

c<-merge(a, b, by="Kasvinimi")
c[2:length(c)]<-c[2:length(c)]<-round(c[2:length(c)], 1)

colnames(c)<-c("Crop","Baseline", "2016", "2017", "2018", "2019")

c<-c %>% pivot_longer(cols= 2:length(c), names_to = "Coeff_name", values_to = "Coeff")

Satopohjaiset<-c

Satopohjaiset<-Satopohjaiset %>% mutate(Crop = case_when(Crop == "Kaura" ~ "Oat",
                                                         Crop == "Kevätrypsi" ~ "Spring rape",
                                                         Crop == "Porkkana" ~ "Carrot", 
                                                         Crop == "Syysrypsi" ~ "Fall rape" ,
                                                         Crop == "Tarhaherne" ~ "Garden pea",
))


ord<-c("Baseline","2016","2017","2018","2019")

Satopohjaiset$Coeff_name<-as.factor(Satopohjaiset$Coeff_name)


Satotaulukko<-Satopohjaiset %>% pivot_wider(names_from = Crop, values_from = Coeff)


Satotaulukko_viljav<-gt(Satotaulukko) %>% 
  opt_stylize(style=6) %>%
  cols_align("left", columns = 1) %>%
  cols_align("center", columns = 2:length(Satotaulukko)) %>%
  tab_spanner(label=html("tn CO<sub>2</sub> tn<sup>-1</sup>"), 
              columns = everything()) %>%
  cols_label(Coeff_name = "Coefficient")


Satokuva_viljav<-Satopohjaiset %>% 
  ggplot(aes(x=factor(Coeff_name, ord), y=Coeff, fill=Crop))+
  geom_bar(stat="identity")+
  facet_wrap(~Crop)+
  xlab(NULL)+
  ylab(expression("t"~"CO"[2]~"t"^"-1"))+
  theme(axis.text.x = element_text(angle = 50, hjust=1))+
  theme(legend.position = "none")

#Europohjaiset kertoimet

a<-Peruskertoimet_euro
a<-a %>% select(1,2,6)

b<-Verrokkikertoimet_euro
b<-b %>% select(1,2, 8:length(b))

c<-merge(a, b, by=c("Kasvinimi","Kasvikoodi"))
c[3:length(c)]<-round(c[3:length(c)], 1)

c<-c %>% select(-2)
colnames(c)<-c("Crop","Baseline", "2016", "2017", "2018", "2019")

c<-c %>% pivot_longer(cols= 2:length(c), names_to = "Coeff_name", values_to = "Coeff")

Europohjaiset<-c

Europohjaiset<-Europohjaiset %>% mutate(Crop = case_when(Crop == "Kaura" ~ "Oat",
                                                         Crop == "Kevätrypsi" ~ "Spring rape",
                                                         Crop == "Porkkana" ~ "Carrot", 
                                                         Crop == "Syysrypsi" ~ "Fall rape" ,
                                                         Crop == "Tarhaherne" ~ "Garden pea"))

ord<-c("Baseline","2016","2017","2018","2019")

Eurokuva_viljav<-Europohjaiset %>% 
  ggplot(aes(x=factor(Coeff_name, ord), y=Coeff, fill=Crop))+
  geom_bar(stat="identity")+
  facet_wrap(~Crop)+
  xlab("Coefficient")+
  ylab(expression("t"~"CO"[2]~"1000"~"EUR"^~-1))+
  theme(axis.text.x = element_text(angle = 70, hjust=1))+
  theme(legend.position = "none")


Eurotaulukko<-Europohjaiset %>% pivot_wider(names_from = Crop, values_from = Coeff)

Eurotaulukko_viljav<-gt(Eurotaulukko) %>% 
  opt_stylize(style=6, color = "green") %>%
  cols_align("left", columns = 1) %>%
  cols_align("center", columns = 2:length(Eurotaulukko)) %>%
  tab_spanner(label=html("tn CO<sub>2</sub> 1000 €<sup>-1</sup>"), 
              columns = everything()) %>%
  cols_label(Coeff_name = "Coefficient")


library("ggpubr")
JoinedGraph2<-ggarrange(Satokuva_viljav, Eurokuva_viljav, nrow=2)

