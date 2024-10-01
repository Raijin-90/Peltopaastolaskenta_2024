#Lasketaan tiloittain GTK-datan pohjalta maalajisuhde sen arvioimiseksi, mitkä tilat ovat erityisen painottuneita org. maalle ja tarvitsevat sitä maata toimintaansa. 

#Sisältää sellaiset tilat joiden: 
#Peltolohkoille voidaan määrittää maalaji,
#Ja joilla ylipäänsä on 2017 kasvulohkodatasta löytyviä polygoneja, ts. jotka tekivät tuolloin kasvinviljelyä rekisteriin sisältyvällä tavalla. 

# Tiloja yhteensä 51 764
# Originaalilistassa 50 588

51764-50588 #vajetta 1176

#Pellottomia tiloja 1173

51764-50588-1173 #vajetta 3


#Kasvulohkot2017 paikkatietodatassa on lähtökohtaisesti (ennen käsittelyjä) 50 588 tilaa, joka on vähemmän kuin arvioitu kokonaismäärä. Pellottomat tilat eivät näy datassa. 


#Ajetaan pinta-ala-aggregaatit
source_lines <- function(file, lines){
  source(textConnection(readLines(file)[lines]))
}

source_lines(here("Skriptit/Uudet skriptit/GTK_datan_aggregointi.R"), 1:260)

library(varhandle)
rm.all.but("GTKdata")

#Leikataan datasta tarvittavat muuttujat

Tiladata<-GTKdata %>% select(MAATILA_TUNNUS,
                             Tuotantosuunta,
                   KASVIKOODI_lohkodata_reclass,
                   KASVINIMI_reclass,
                   Maannossumma,
                   Eloperaista,
                   Mineraalia)

#Tuotantosuuntaryhmä-taso

library(readxl)
Tuotantosuuntaryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Tuotantosuunnat ryhmittäin",
    col_types = c("text",
                  "text",
                  "skip"
    )
  )
colnames(Tuotantosuuntaryhmat) <-
  c("Tuotantosuunta", "Tuotantosuuntaryhmä","ETOL_koodi")



#Tuotantosuunnat samaan kirjoitusasuun
Tiladata<-Tiladata %>% mutate(Tuotantosuunta = case_when(Tuotantosuunta == "Lammas- ja vuohitilat" ~ "Lammas_ja_vuohitilat",
                                  Tuotantosuunta == "Muut nautakarjatilat" ~ "Muut_nautakarjatilat",
                                  Tuotantosuunta == "Nurmet, laitumet, hakamaat" ~ "Nurmet_laitumet_hakamaat",
                                  Tuotantosuunta == "Palkokasvit pl. tarhaherne" ~ "Palkokasvit_pl_tarhaherne",
                                  Tuotantosuunta == "Rypsi ja rapsi" ~ "Rypsi_rapsi",
                                  Tuotantosuunta == "Tattari ja kinoa" ~ "Tattari_kinoa",
                                  Tuotantosuunta == "Vihannekset ja juurekset" ~ "Vihannekset_juurekset",
                                  Tuotantosuunta == "Viljat pl. ohra" ~ "Viljat_pl_ohra",
                                  .default = Tuotantosuunta))



outersect(unique(Tiladata$Tuotantosuunta),unique(Tuotantosuuntaryhmat$Tuotantosuunta))


nrow(inner_join(Tiladata, Tuotantosuuntaryhmat, by="Tuotantosuunta"))
Tiladata<-inner_join(Tiladata, Tuotantosuuntaryhmat, by="Tuotantosuunta")

#Tuoteryhmät

library(readxl)
Tuoteryhmat <-
  read_excel(
    here("Data","Muuntoavain_tuotantosuunnat_tuotteet_ETOL.xlsx"),
    sheet = "Kasvit tuoteryhmittäin",
    col_types = c("text",
                  "text",
                  "skip"
    )
  )


colnames(Tiladata)[colnames(Tiladata)=="KASVIKOODI_lohkodata_reclass"]<-"Kasvikoodi"
nrow(inner_join(Tiladata, Tuoteryhmat, by="Kasvikoodi"))
Tiladata<-inner_join(Tiladata, Tuotantosuuntaryhmat, by="Tuotantosuunta")



