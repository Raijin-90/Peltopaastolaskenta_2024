#27.11.24 HV
#Varastolannan (pelloille levitettävän) sisältämien typpi- ja fosforimassan laskenta tilakohtaisesti
#Eläinmäärien ja Biomassa-atlaksen lannan ravinnesisältökerrointen perusteella


#Data sisään. Eläinsuojalantaa ei levitetä pelloille, jätetään siksi ulos.

library(readxl)
Elainmaarat <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", 
                                     sheet = "Eläimet_tiloittain_cleaned")
Ravinnekertoimet <- read_excel("Data/Ravinnelaskennan_aineisto/Lannan_ravinnelaskenta.xlsx", 
                                     sheet = "Ravinnekertoimet_cleaned", range = "A1:D9000")