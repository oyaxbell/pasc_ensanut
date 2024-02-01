## Nationally representative prevalence estimates of post-acute sequelae of 
## SARS-CoV-2 infection amongst Mexican adults in 2022
## Data Analysis: Omar Yaxmehen Bello-Chavolla (oyaxbell@yahoo.com.mx)
## Latest version of Analysis 02-July-2022
## Any question regarding analysis, please contact Omar Yaxmehen Bello-Chavolla

#### Package load ####
pacman::p_load(readr, haven, tidyverse, nortest, ggstance, sf, spdep, biscale, shadowtext, jtools, spdep,readxl,FactoMineR,nomclust,logbin,glm2,fmsb,
               na.tools, survey, jtools, ggpubr, ggsci, rmapshaper, gapminder, performance,lubridate,gtsummary,cluster,factoextra,dendextend,naniar,
               remotes, ggplot2, reshape2, cowplot, ggthemes, lme4, lmerTest, glmmTMB, flextable, officer, OptimalCutpoints, ggstats, VIM, ggplotify)

#### Ensamble ENSANUT 2022 ####
#setwd("~/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PACS")
#data <- read_sav("Data/data.sav")
#salud <- read_sav("Data/salud.sav")
#fisica <- read_sav("Data/fisica.sav")
#antro <- read_sav("Data/antro.sav")
#bioquim <- read_sav("Data/bioquim.sav")
#covid <- read_sav("Data/covid.sav")

#ensanut_2022<- data %>% left_join(salud, by="FOLIO_INT")  %>% 
#  left_join(antro, by="FOLIO_INT") %>% left_join(fisica, by="FOLIO_INT")%>% 
#  left_join(bioquim, by="FOLIO_INT")%>% left_join(covid, by="FOLIO_INT")
#write.csv(ensanut_2022, "Data/ensanut2022.csv")

#### Shapes ####
setwd("~/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/ENSANUT COVID/")
#setwd("/Users/carlosfermin/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/ENSANUT COVID")

#Shapes df
geom_mx <-  sf::st_read(dsn="shapes", layer="areas_geoestadisticas_estatales")  %>%
  sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",stringsAsFactors=FALSE)
geom_mx<-ms_simplify(geom_mx, keep = 0.01, keep_shapes = T)
geom_mx$CVE_ENT<-as.numeric(geom_mx$CVE_ENT)
geom_mx$CVE_ENT_NUM<-as.numeric(geom_mx$CVE_ENT)

#Region
geom_mx$REGIONES<-NULL
geom_mx$REGIONES[geom_mx$CVE_ENT_NUM %in% c(2,3,5,8,10,19,26,28)]<-1
geom_mx$REGIONES[geom_mx$CVE_ENT_NUM %in% c(1,6,11,14,16,17,18,22,24,25,32)]<-2
geom_mx$REGIONES[geom_mx$CVE_ENT_NUM %in% c(9,15)]<-3
geom_mx$REGIONES[geom_mx$CVE_ENT_NUM %in% c(4,7,12,13,20,21,29,23,27,30,31)]<-4

#Subregion

geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(2,3,18,25,26)]<-1
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(5,8,19,28)]<-2
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(6,14,16)]<-3
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(1,10,11,22,24,32)]<-4
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(13,29,30)]<-5
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(9,15)]<-6
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(12,17,20,21)]<-7
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(4,7,27,23,31)]<-8

#### Load ENSANUT 2022 ####
setwd("~/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PACS")
#setwd("/Users/carlosfermin/Library/CloudStorage/OneDrive-UNIVERSIDADNACIONALAUTÓNOMADEMÉXICO/PACS")

ensanut_2022 <- read.csv("Data/ensanut2022.csv")
ensanut_2022<-ensanut_2022 %>% mutate(ID = row_number())
ensanut_2022$YEAR<-2022
ensanut_2022$CVE_ENT<-as.numeric(ensanut_2022$entidad.x)

#Subregiones Ensanut
ensanut_2022$SUBREGION<-NULL
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(2,3,18,25,26)]<-1
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(5,8,19,28)]<-2
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(6,14,16)]<-3
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(1,10,11,22,24,32)]<-4
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(13,29,30)]<-5
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(9, 15)]<-6
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(12,17,20,21)]<-7
ensanut_2022$SUBREGION[ensanut_2022$CVE_ENT %in% c(4,7,27,23,31)]<-8

#Region
ensanut_2022$REGIONES<-NULL
ensanut_2022$REGIONES[ensanut_2022$CVE_ENT %in% c(2,3,5,8,10,19,26,28)]<-1
ensanut_2022$REGIONES[ensanut_2022$CVE_ENT %in% c(1,6,11,14,16,17,18,22,24,25,32)]<-2
ensanut_2022$REGIONES[ensanut_2022$CVE_ENT %in% c(9,15)]<-3
ensanut_2022$REGIONES[ensanut_2022$CVE_ENT %in% c(4,7,12,13,20,21,29,23,27,30,31)]<-4

#### DISLI ####
#SLI dataset (ESTADO)
marg.ent<-read_excel("Data/margindicadores_estado_2020.xlsx")
marg.ent$CVE_ENT<-as.numeric(marg.ent$Clave)
marg.ent$cve<-as.numeric(marg.ent$Clave)
marg.ent.1<-marg.ent%>%dplyr::select(CVE_ENT,marg,marg_cat)
marg.ent.1$densidad<-c(253.9,52.8,10.8,16.1,20.8,130.0,75.6,15.1,
                       6163.3,14.9,201.5,55.7,148.1,106.2,760.2,81.0,
                       404.1,44.4,90.2,44.1,191.9,202.6,41.6,46.2,
                       52.8,16.4,97.1,44.0,336.0,112.3,58.7,21.5)
marg.ent.1$disli<-lm(marg~log(densidad), data=marg.ent.1)$residuals

marg.ent.1$DISLI_CAT<-NULL
marg.ent.1$DISLI_CAT[marg.ent.1$marg_cat=="Alto"]<-1
marg.ent.1$DISLI_CAT[marg.ent.1$marg_cat=="Muy alto"]<-1
marg.ent.1$DISLI_CAT<-na.tools::na.replace(marg.ent.1$DISLI_CAT,0)

ensanut_2022<-ensanut_2022 %>% left_join(marg.ent.1, by="CVE_ENT")
ensanut_2022 <- ensanut_2022 %>% 
  mutate("DISLI_cat"=cut(disli, breaks = c( -Inf, quantile(x = disli, probs = c(0.25, 0.5, 0.75)), Inf),
                         labels=c("Q1", "Q2", "Q3", "Q4")))

quantile(ensanut_2022$disli)

#### Weights for modeling ####
ensanut_2022$ponde_f.y<-as.numeric(as.character(ensanut_2022$ponde_f.y)) #Ponderador o Factor de Expansión
ensanut_2022$sampleid<-paste(ensanut_2022$CVE_ENT, as.numeric(ensanut_2022$est_sel.y)) #Crear un ID con el estado y la variable de estratificacion
#within each sampling unit, sum the weights
wts<-tapply(ensanut_2022$ponde_f.y,ensanut_2022$sampleid,sum)
#make a data frame from this
wts<-data.frame(id=names(unlist(wts)), wt=unlist(wts))
#get the unique sampling location ids'
t1<-as.data.frame(table(ensanut_2022$sampleid))
#put all of this into a data set
wts2<-data.frame(ids=wts$id, sumwt=wts$wt, jn=t1$Freq)
#merge all of this back to the original data file
ensanut_2022<-merge(ensanut_2022, wts2, by.x="sampleid", by.y="ids", all.x=T)
#In the new data set, multiply the original weight by the fraction of the
#sampling unit total population each person represents
ensanut_2022$swts<-ensanut_2022$ponde_f.y*(ensanut_2022$jn/ensanut_2022$sumwt)

#### Symptom codify ####
ensanut_2022$secuela<-ifelse(ensanut_2022$H1213A=="U" | ensanut_2022$H1213A=="",0,1)
ensanut_2022$secuela[ensanut_2022$H1213A=="X"]<-0
ensanut_2022$secuela[ensanut_2022$H1213A=="Y"]<-0
ensanut_2022$secuela[is.na(ensanut_2022$secuela)]<-0
ensanut_2022$olfato<-ifelse(((ensanut_2022$H1213A=="Q" | ensanut_2022$H1213A=="R") |
                              (ensanut_2022$H1213B=="Q" | ensanut_2022$H1213B=="R") |
                              (ensanut_2022$H1213C=="Q" | ensanut_2022$H1213C=="R") |
                              (ensanut_2022$H1213D=="Q" | ensanut_2022$H1213D=="R") |
                              (ensanut_2022$H1213E1=="Q" | ensanut_2022$H1213E1=="R") |
                              (ensanut_2022$H1213F=="Q" | ensanut_2022$H1213F=="R") |
                              (ensanut_2022$H1213G=="Q" | ensanut_2022$H1213G=="R") |
                              (ensanut_2022$H1213H=="Q" | ensanut_2022$H1213H=="R") |
                              (ensanut_2022$H1213I=="Q" | ensanut_2022$H1213I=="R") |
                              (ensanut_2022$H1213J=="Q" | ensanut_2022$H1213J=="R") |
                              (ensanut_2022$H1213K=="Q" | ensanut_2022$H1213K=="R") |
                              (ensanut_2022$H1213L=="Q" | ensanut_2022$H1213L=="R") |
                              (ensanut_2022$H1213M=="Q" | ensanut_2022$H1213M=="R") |
                              (ensanut_2022$H1213N=="Q" | ensanut_2022$H1213N=="R") |
                              (ensanut_2022$H1213O=="Q" | ensanut_2022$H1213O=="R") |
                              (ensanut_2022$H1213P=="Q" | ensanut_2022$H1213P=="R") |
                              (ensanut_2022$H1213Q=="Q" | ensanut_2022$H1213Q=="R") |
                              (ensanut_2022$H1213R=="Q" | ensanut_2022$H1213R=="R")),1,0)
ensanut_2022$olfato[is.na(ensanut_2022$olfato)]<-0
ensanut_2022$olfato2<-ifelse(((ensanut_2022$H1213A=="Q" ) |
                               (ensanut_2022$H1213B=="Q") |
                               (ensanut_2022$H1213C=="Q") |
                               (ensanut_2022$H1213D=="Q") |
                               (ensanut_2022$H1213E1=="Q") |
                               (ensanut_2022$H1213F=="Q") |
                               (ensanut_2022$H1213G=="Q") |
                               (ensanut_2022$H1213H=="Q") |
                               (ensanut_2022$H1213I=="Q") |
                               (ensanut_2022$H1213J=="Q") |
                               (ensanut_2022$H1213K=="Q") |
                               (ensanut_2022$H1213L=="Q") |
                               (ensanut_2022$H1213M=="Q") |
                               (ensanut_2022$H1213N=="Q") |
                               (ensanut_2022$H1213O=="Q") |
                               (ensanut_2022$H1213P=="Q") |
                               (ensanut_2022$H1213Q=="Q") |
                               (ensanut_2022$H1213R=="Q")),1,0)
ensanut_2022$olfato2[is.na(ensanut_2022$olfato)]<-0
ensanut_2022$gusto<-ifelse(((ensanut_2022$H1213A=="R" ) |
                              (ensanut_2022$H1213B=="R") |
                              (ensanut_2022$H1213C=="R") |
                              (ensanut_2022$H1213D=="R") |
                              (ensanut_2022$H1213E1=="R") |
                              (ensanut_2022$H1213F=="R") |
                              (ensanut_2022$H1213G=="R") |
                              (ensanut_2022$H1213H=="R") |
                              (ensanut_2022$H1213I=="R") |
                              (ensanut_2022$H1213J=="R") |
                              (ensanut_2022$H1213K=="R") |
                              (ensanut_2022$H1213L=="R") |
                              (ensanut_2022$H1213M=="R") |
                              (ensanut_2022$H1213N=="R") |
                              (ensanut_2022$H1213O=="R") |
                              (ensanut_2022$H1213P=="R") |
                              (ensanut_2022$H1213Q=="R") |
                              (ensanut_2022$H1213R=="R")),1,0)
ensanut_2022$gusto[is.na(ensanut_2022$gusto)]<-0
ensanut_2022$fog<-ifelse((ensanut_2022$H1213A=="S" | ensanut_2022$H1213B=="S" | ensanut_2022$H1213C=="S" | 
                            ensanut_2022$H1213D=="S"| ensanut_2022$H1213E1=="S"| ensanut_2022$H1213F=="S"|
                            ensanut_2022$H1213G=="S"| ensanut_2022$H1213H=="S"| ensanut_2022$H1213I=="S"|
                            ensanut_2022$H1213J=="S"| ensanut_2022$H1213K=="S"| ensanut_2022$H1213M=="S"|
                            ensanut_2022$H1213N=="S"| ensanut_2022$H1213O=="S"| ensanut_2022$H1213P=="S"|
                            ensanut_2022$H1213Q=="S"| ensanut_2022$H1213R=="S"| ensanut_2022$H1213S=="S"|
                            ensanut_2022$H1213L=="S"),1,0)
ensanut_2022$fog[is.na(ensanut_2022$fog)]<-0
ensanut_2022$gastrointestinal<-ifelse((ensanut_2022$H1213A=="P" | ensanut_2022$H1213B=="P" | ensanut_2022$H1213C=="P" | 
                            ensanut_2022$H1213D=="P"| ensanut_2022$H1213E1=="P"| ensanut_2022$H1213F=="P"|
                            ensanut_2022$H1213G=="P"| ensanut_2022$H1213H=="P"| ensanut_2022$H1213I=="P"|
                            ensanut_2022$H1213J=="P"| ensanut_2022$H1213K=="P"| ensanut_2022$H1213M=="P"|
                            ensanut_2022$H1213N=="P"| ensanut_2022$H1213O=="P"| ensanut_2022$H1213P=="P"|
                            ensanut_2022$H1213L=="P"),1,0)
ensanut_2022$gastrointestinal[is.na(ensanut_2022$gastrointestinal)]<-0
ensanut_2022$chest_pain<-ifelse((ensanut_2022$H1213A=="O" | ensanut_2022$H1213B=="O" | ensanut_2022$H1213C=="O" | 
                                         ensanut_2022$H1213D=="O"| ensanut_2022$H1213E1=="O"| ensanut_2022$H1213F=="O"|
                                         ensanut_2022$H1213G=="O"| ensanut_2022$H1213H=="O"| ensanut_2022$H1213I=="O"|
                                         ensanut_2022$H1213J=="O"| ensanut_2022$H1213K=="O"| ensanut_2022$H1213M=="O"|
                                         ensanut_2022$H1213N=="O"| ensanut_2022$H1213O=="O"| ensanut_2022$H1213L=="O"),1,0)
ensanut_2022$chest_pain[is.na(ensanut_2022$chest_pain)]<-0
ensanut_2022$breath<-ifelse((ensanut_2022$H1213A=="N" | ensanut_2022$H1213B=="N" | ensanut_2022$H1213C=="N" | 
                                   ensanut_2022$H1213D=="N"| ensanut_2022$H1213E1=="N"| ensanut_2022$H1213F=="N"|
                                   ensanut_2022$H1213G=="N"| ensanut_2022$H1213H=="N"| ensanut_2022$H1213I=="N"|
                                   ensanut_2022$H1213J=="N"| ensanut_2022$H1213K=="N"| ensanut_2022$H1213M=="N"|
                                   ensanut_2022$H1213N=="N"| ensanut_2022$H1213L=="N"),1,0)
ensanut_2022$breath[is.na(ensanut_2022$breath)]<-0
ensanut_2022$breath2<-ifelse((ensanut_2022$H1213A=="M" | ensanut_2022$H1213B=="M" | ensanut_2022$H1213C=="M" | 
                               ensanut_2022$H1213D=="M"| ensanut_2022$H1213E1=="M"| ensanut_2022$H1213F=="M"|
                               ensanut_2022$H1213G=="M"| ensanut_2022$H1213H=="M"| ensanut_2022$H1213I=="M"|
                               ensanut_2022$H1213J=="M"| ensanut_2022$H1213K=="M"| ensanut_2022$H1213M=="M"|
                               ensanut_2022$H1213L=="M"),1,0)
ensanut_2022$breath2[is.na(ensanut_2022$breath2)]<-0
ensanut_2022$pain<-ifelse((ensanut_2022$H1213A=="L" | ensanut_2022$H1213B=="L" | ensanut_2022$H1213C=="L" | 
                             ensanut_2022$H1213D=="L"| ensanut_2022$H1213E1=="L"| ensanut_2022$H1213F=="L"|
                             ensanut_2022$H1213G=="L"| ensanut_2022$H1213H=="L"| ensanut_2022$H1213I=="L"|
                             ensanut_2022$H1213J=="L"| ensanut_2022$H1213K=="L" | ensanut_2022$H1213L=="L"),1,0)
ensanut_2022$pain[is.na(ensanut_2022$pain)]<-0
ensanut_2022$dizzy<-ifelse((ensanut_2022$H1213A=="K" | ensanut_2022$H1213B=="K" | ensanut_2022$H1213C=="K" | 
                             ensanut_2022$H1213D=="K"| ensanut_2022$H1213E1=="K"| ensanut_2022$H1213F=="K"|
                             ensanut_2022$H1213G=="K"| ensanut_2022$H1213H=="K"| ensanut_2022$H1213I=="K"|
                             ensanut_2022$H1213J=="K"| ensanut_2022$H1213K=="K"),1,0)
ensanut_2022$dizzy[is.na(ensanut_2022$dizzy)]<-0
ensanut_2022$headache<-ifelse((ensanut_2022$H1213A=="J" | ensanut_2022$H1213B=="J" | ensanut_2022$H1213C=="J" | 
                              ensanut_2022$H1213D=="J"| ensanut_2022$H1213E1=="J"| ensanut_2022$H1213F=="J"|
                              ensanut_2022$H1213G=="J"| ensanut_2022$H1213H=="J"| ensanut_2022$H1213I=="J"|
                              ensanut_2022$H1213J=="J"),1,0)
ensanut_2022$headache[is.na(ensanut_2022$headache)]<-0
ensanut_2022$weightloss<-ifelse((ensanut_2022$H1213A=="I" | ensanut_2022$H1213B=="I" | ensanut_2022$H1213C=="I" | 
                                ensanut_2022$H1213D=="I"| ensanut_2022$H1213E1=="I"| ensanut_2022$H1213F=="I"|
                                ensanut_2022$H1213G=="I"| ensanut_2022$H1213H=="I"| ensanut_2022$H1213I=="I"),1,0)
ensanut_2022$weightloss[is.na(ensanut_2022$weightloss)]<-0
ensanut_2022$apetite<-ifelse((ensanut_2022$H1213A=="H" | ensanut_2022$H1213B=="H" | ensanut_2022$H1213C=="H" | 
                                   ensanut_2022$H1213D=="H"| ensanut_2022$H1213E1=="H"| ensanut_2022$H1213F=="H"|
                                   ensanut_2022$H1213G=="H"| ensanut_2022$H1213H=="H"),1,0)
ensanut_2022$apetite[is.na(ensanut_2022$apetite)]<-0
ensanut_2022$kidney<-ifelse((ensanut_2022$H1213A=="G" | ensanut_2022$H1213B=="G" | ensanut_2022$H1213C=="G" | 
                                ensanut_2022$H1213D=="G"| ensanut_2022$H1213E1=="G"| ensanut_2022$H1213F=="G"|
                                ensanut_2022$H1213G=="G"),1,0)
ensanut_2022$kidney[is.na(ensanut_2022$kidney)]<-0
ensanut_2022$sleep<-ifelse((ensanut_2022$H1213A=="F" | ensanut_2022$H1213B=="F" | ensanut_2022$H1213C=="F" | 
                               ensanut_2022$H1213D=="F"| ensanut_2022$H1213E1=="F"| ensanut_2022$H1213F=="F"),1,0)
ensanut_2022$sleep[is.na(ensanut_2022$sleep)]<-0
ensanut_2022$fever<-ifelse((ensanut_2022$H1213A=="E" | ensanut_2022$H1213B=="E" | ensanut_2022$H1213C=="E" | 
                              ensanut_2022$H1213D=="E"| ensanut_2022$H1213E1=="E"),1,0)
ensanut_2022$fever[is.na(ensanut_2022$fever)]<-0
ensanut_2022$depression<-ifelse((ensanut_2022$H1213A=="D" | ensanut_2022$H1213B=="D" | ensanut_2022$H1213C=="D" | 
                              ensanut_2022$H1213D=="D"),1,0)
ensanut_2022$depression[is.na(ensanut_2022$depression)]<-0
ensanut_2022$anxiety<-ifelse((ensanut_2022$H1213A=="C" | ensanut_2022$H1213B=="C" | ensanut_2022$H1213C=="C"),1,0)
ensanut_2022$anxiety[is.na(ensanut_2022$anxiety)]<-0
ensanut_2022$fatigue<-ifelse((ensanut_2022$H1213A=="B" | ensanut_2022$H1213B=="B"),1,0)
ensanut_2022$fatigue[is.na(ensanut_2022$fatigue)]<-0
ensanut_2022$cough<-ifelse((ensanut_2022$H1213A=="A"),1,0)
ensanut_2022$cough[is.na(ensanut_2022$cough)]<-0
ensanut_2022$post_malaise<-ifelse(ensanut_2022$h1216==1 | (ensanut_2022$fatigue==1 & ensanut_2022$fog==1),1,0)
ensanut_2022$post_malaise[is.na(ensanut_2022$post_malaise)]<-0
ensanut_2022$inability<-ifelse(ensanut_2022$h1216==1,1,0)
ensanut_2022$inability[is.na(ensanut_2022$inability)]<-0

#### PASC definition ####
ensanut_2022$pacs_score<-ensanut_2022$olfato*8+ensanut_2022$cough*4+ ensanut_2022$fog*3+ensanut_2022$post_malaise*7+
  ensanut_2022$chest_pain*2+ensanut_2022$fatigue+ensanut_2022$dizzy+ensanut_2022$gastrointestinal
ensanut_2022$pacs_score[is.na(ensanut_2022$pacs_score)]<-0
ensanut_2022$pacs<-ifelse(ensanut_2022$h1215>=3,1,0)
ensanut_2022$pacs[is.na(ensanut_2022$h1215)]<-0
ensanut_2022$pacs2<-ifelse(ensanut_2022$pacs_score>=12,1,0)
ensanut_2022$duration<-ifelse(ensanut_2022$h1215>=3,1,0)
ensanut_2022$duration[is.na(ensanut_2022$h1215)]<-0
ensanut_2022$duration[ensanut_2022$h1215==5]<-2
ensanut_2022$duration<-factor(ensanut_2022$duration, labels = c("≤3 months", ">3 months", "Still persist"))
ensanut_2022$acutePASC<-ifelse(ensanut_2022$h1215<3,1,0)
ensanut_2022$acutePASC[is.na(ensanut_2022$h1215)]<-0
ensanut_2022$chronicPASC<-ifelse(ensanut_2022$h1215>=3,1,0)
ensanut_2022$chronicPASC[is.na(ensanut_2022$h1215)]<-0
ensanut_2022$h1215[is.na(ensanut_2022$h1215)]<-0

ensanut_2022$duration2<-factor(ensanut_2022$h1215, labels = c("No persistent symptoms","<1 month","1-3 months", "3-6 months", ">6 months",">6 months"))

ensanut_2022$duration3<-factor(ensanut_2022$h1215, labels = c("No persistent symptoms","<1 month","1-3 months", "3-6 months", ">6 months","Still persistent"))

#### Modifying factors ####
set.seed(123)
ensanut_2022$n_pos<-ifelse(ensanut_2022$Interpretacion_N_PF=="POSITIVO",1,0)
ensanut_2022$vacuna<-ifelse(ensanut_2022$h1602==1,1,0)
ensanut_2022$date_vaccinated<-as_date(paste0(ifelse(ensanut_2022$h16041d==99,sample.int(30, 1, replace = TRUE),ensanut_2022$h16041d),"/",
                                    ifelse(ensanut_2022$h16041m==99,"06",ensanut_2022$h16041m),"/",
                                    ifelse(ensanut_2022$h16041a==9999,"2021",ensanut_2022$h16041a)), format = "%d/%m/%y")
ensanut_2022$unvaccinated<-ifelse(ensanut_2022$h1602==2,1,0)
ensanut_2022$dose1<-ifelse(is.na(ensanut_2022$h16041a),0,1)
ensanut_2022$dose2<-ifelse(is.na(ensanut_2022$h16042a),0,1)
ensanut_2022$dose3<-ifelse(is.na(ensanut_2022$h16043a),0,1)
ensanut_2022$dose4<-ifelse(is.na(ensanut_2022$h16044a),0,1)
ensanut_2022$doses<-ensanut_2022$dose1+ensanut_2022$dose2+ensanut_2022$dose3+ensanut_2022$dose4
table(ensanut_2022$doses)
ensanut_2022$onlydose1<-ifelse(ensanut_2022$doses==1,1,0)
ensanut_2022$onlydose2<-ifelse(ensanut_2022$doses==2,1,0)
ensanut_2022$onlydose3<-ifelse(ensanut_2022$doses==3,1,0)
ensanut_2022$onlydose4<-ifelse(ensanut_2022$doses==4,1,0)

ensanut_2022$h1207b[ensanut_2022$h1207b==9999]<-NA
ensanut_2022$h1207a[ensanut_2022$h1207a==99]<-NA
ensanut_2022$covid<-ifelse(ensanut_2022$h1205 %in% c(98,99),0,1)
ensanut_2022$omicron<-ifelse(ensanut_2022$h1207b==2022,1,0)
ensanut_2022$omicron[ensanut_2022$h1207b==2021 & ensanut_2022$h1207a==12]<-1
ensanut_2022$ancestral<-ifelse(ensanut_2022$h1207b %in% c(2020),1,0)
ensanut_2022$b11519<-ifelse(ensanut_2022$h1207b %in% c(2021) & ensanut_2022$h1207a %in% c(1:5),1,0)
ensanut_2022$delta<-ifelse(ensanut_2022$h1207b %in% c(2021) & ensanut_2022$h1207a %in% c(6:11),1,0)
ensanut_2022$delta[ensanut_2022$h1207b==2021 & is.na(ensanut_2022$h1207a)]<-1
ensanut_2022$omicron[ensanut_2022$covid==1 & is.na(ensanut_2022$h1207b)]<-1
ensanut_2022$variant<-ensanut_2022$ancestral+ensanut_2022$b11519*2+ensanut_2022$delta*3+ensanut_2022$omicron*4

ensanut_2022$reinfection<-ifelse(ensanut_2022$h1205 %in% c(2,3,4,8),1,0)

ensanut_2022$age<-ensanut_2022$h0303.x
ensanut_2022$sex<-ifelse(ensanut_2022$h0302.x==1,1,0)
ensanut_2022$date_covid<-paste0("15/",ensanut_2022$h1207a,"/",ensanut_2022$h1207b)
ensanut_2022$date_covid[grepl("NA", ensanut_2022$date_covid)]<-NA
ensanut_2022$date_covid<-as.POSIXct(ensanut_2022$date_covid, tryFormats = "%d/%m/%Y")
ensanut_2022$fecha_ini_1<-as.POSIXct(ensanut_2022$fecha_ini_1, tryFormats = c("%d/%m/%Y"))
ensanut_2022$time_infection<-ensanut_2022$fecha_ini_1-ensanut_2022$date_covid
ensanut_2022$diabetes<-ifelse(ensanut_2022$a0301==1, 1, 0)
ensanut_2022$daily_smoking<-ifelse(ensanut_2022$a1301==1, 1,0)
ensanut_2022$daily_drinking<-ifelse(ensanut_2022$a1308==1, 1,0)

ensanut_2022$age_cat<-cut(ensanut_2022$age, breaks = c(-Inf, 29, 39, 49, 59, 69, Inf))

ensanut_2022$time1<-ifelse(ensanut_2022$h1215==1,1,0)
ensanut_2022$time2<-ifelse(ensanut_2022$h1215==2,1,0)
ensanut_2022$time3<-ifelse(ensanut_2022$h1215==3,1,0)
ensanut_2022$time4<-ifelse(ensanut_2022$h1215==4,1,0)
ensanut_2022$time5<-ifelse(ensanut_2022$h1215==5,1,0)
ensanut_2022$bmi<-ensanut_2022$an12_1/((ensanut_2022$an15_1/100)^2)
ensanut_2022$Hypertension<-ifelse(ensanut_2022$a0401==1,1,0)

ensanut_2022$a0211<-ensanut_2022$a0211-1
ensanut_2022$a0212<-ensanut_2022$a0212-1
ensanut_2022$a0213<-ensanut_2022$a0213-1
ensanut_2022$a0214<-ensanut_2022$a0214-1
ensanut_2022$a0215<-ensanut_2022$a0215-1
ensanut_2022$a0216<-ensanut_2022$a0216-1
ensanut_2022$a0217<-ensanut_2022$a0217-1
ensanut_2022$cesd<-ensanut_2022$a0211+ensanut_2022$a0212+ensanut_2022$a0213+ensanut_2022$a0214+
  ensanut_2022$a0215+ensanut_2022$a0216+ensanut_2022$a0217
ensanut_2022$weight_loss <- ifelse((ensanut_2022$a0107==2&ensanut_2022$a0108>5&ensanut_2022$a0109==1),1,0)
ensanut_2022$cvd <- ifelse(ensanut_2022$a0502a==1,1,0)
ensanut_2022$Setting<-ensanut_2022$estrato.x
ensanut_2022$Setting[ensanut_2022$Setting==3]<-2
ensanut_2022$Setting<-factor(ensanut_2022$Setting, labels = c("Rural", "Urban"))
ensanut_2022$Sinovac<-ifelse(ensanut_2022$h16051==3,1,0)

ensanut_2022$mRNA<- ifelse(ensanut_2022$h16051 %in% c(1,6),1,0)
ensanut_2022$adenoVector<- ifelse(ensanut_2022$h16051 %in% c(2,4,5,7),1,0)
ensanut_2022$otherVaccine<-ifelse(ensanut_2022$h16051 %in% c(8,9),1,0)
ensanut_2022$typeVaccine<-(ensanut_2022$vacuna==0)+ensanut_2022$mRNA*2+ensanut_2022$adenoVector*3+ensanut_2022$otherVaccine*4
ensanut_2022$typeVaccine[ensanut_2022$typeVaccine==4]<-5
ensanut_2022$typeVaccine[ensanut_2022$typeVaccine==0]<-4
ensanut_2022$typeVaccine<-factor(ensanut_2022$typeVaccine, labels = c("Unvaccinated", "mRNA", "Adenovirus vector", "Inactivated virus" ,"Other"))
#### Comorbidities ####
## Infarto
ensanut_2022$a0502a <- replace_na(ensanut_2022$a0502a, 2)
ensanut_2022$AMI_fin <- ifelse(ensanut_2022$a0502a==1,1,0)
ensanut_2022$AMI_fin %>% table(useNA = "always")

#Angina
ensanut_2022$a0502b <- replace_na(ensanut_2022$a0502b, 2)
ensanut_2022$ANG_fin <- ifelse(ensanut_2022$a0502b==1,1,0)
ensanut_2022$ANG_fin %>% table(useNA = "always")

#Heart failure
ensanut_2022$a0502c <- replace_na(ensanut_2022$a0502c, 2)
ensanut_2022$HFA_fin <- ifelse(ensanut_2022$a0502c==1,1,0)
ensanut_2022$HFA_fin %>% table(useNA = "always")

#Stroke
ensanut_2022$a0502d[ensanut_2022$a0502d==9]<-NA
ensanut_2022$EVC_fin <- ifelse(ensanut_2022$a0502d==1,1,0)
ensanut_2022$EVC_fin %>% table(useNA = "always")

#CKD
ensanut_2022$a0601c[ensanut_2022$a0601c==9]<-NA
ensanut_2022$CKD_fin <- ifelse(ensanut_2022$a0601c==1,1,0)
ensanut_2022$CKD_fin %>% table(useNA = "always")

##Unified Weight
ensanut_2022$peso_2022 <- ensanut_2022$an01_1 
ensanut_2022$peso_2022[ensanut_2022$edad>=60&!is.na(ensanut_2022$an12_1 )] <- with(
  ensanut_2022, an12_1 [ensanut_2022$edad>=60&!is.na(ensanut_2022$an12_1 )])
##Unified Height
ensanut_2022$talla_2022 <- ensanut_2022$an04_1 
ensanut_2022$talla_2022[ensanut_2022$edad>=60&!is.na(ensanut_2022$an15_1 )] <- with(
  ensanut_2022, an15_1 [ensanut_2022$edad>=60&!is.na(ensanut_2022$an15_1 )])
ensanut_2022$talla_metros_2022<-(ensanut_2022$talla_2022/100) #En metros

## BMI
ensanut_2022$imc_calculo_2022<-(ensanut_2022$peso_2022/ensanut_2022$talla_metros_2022^2)
ensanut_2022$imc_cat_2022[ensanut_2022$imc_calculo_2022>=18.5 & ensanut_2022$imc_calculo_2022<25]<-0 #Normal weight
ensanut_2022$imc_cat_2022[ensanut_2022$imc_calculo_2022>=25 & ensanut_2022$imc_calculo_2022<30]<-1 #Overweight
ensanut_2022$imc_cat_2022[ensanut_2022$imc_calculo_2022>=30]<-2 #Obesity
table(ensanut_2022$imc_cat_2022, useNA = "always")
ensanut_2022$imc_cat2 <- ensanut_2022$imc_calculo_2022 %>% cut(c(-Inf, 18.5, 25, 30, Inf))
table(ensanut_2022$imc_cat2, useNA = "always")

#### Clustering of symptoms ####
df<-ensanut_2022%>%
  filter(pacs==1) %>% select(c("FOLIO_INT",1153,1150,1151,1152,1154:1170))

names(df)<-c("FOLIO_INT",  "GIS","Anosmia","Dysgeusia", "Bain fog",
             "Chest pain", "Dyspnea", "Breathlessness", "MS-Pain", "Dizziness",
             "Headache", "WTL", "LoA", "Kidney-D",
             "Sleep-D", "Fever", "Depression", "Anxiety", "Fatigue", "Cough", "P-E Malaise", "Disability")

# creating an object with results of hierarchical clustering
hca.object <- nomclust(df[,-c(1,22)], measure = "goodall3", eval = TRUE,clu.high = 5, method = "complete")

# obtaining evaluation criteria for the provided dataset and cluster memberships
df$clusters<-hca.object$mem$clu_4
summary(hca.object)

## Bubble plots ##

data_cluster_1<-colSums(df[df$clusters==1,-c(1,23)], na.rm=T)/table(df$clusters==1)[2]*100
data_cluster_2<-colSums(df[df$clusters==2,-c(1,23)], na.rm=T)/table(df$clusters==2)[2]*100
data_cluster_3<-colSums(df[df$clusters==3,-c(1,23)], na.rm=T)/table(df$clusters==3)[2]*100
data_cluster_4<-colSums(df[df$clusters==4,-c(1,23)], na.rm=T)/table(df$clusters==4)[2]*100
df2<-data.frame(freq=c(data_cluster_1, data_cluster_2, data_cluster_3, data_cluster_4), 
                cluster=c(rep("Cluster 1",21),rep("Cluster 2",21),rep("Cluster 3",21),rep("Cluster 4",21)),
                symptoms=c(rep(names(data_cluster_1),4)))

fig5a<-df2 %>%
  ggplot(aes(y= fct_reorder(symptoms,-freq), x = cluster)) + 
  geom_point(aes(size = freq, fill = cluster), alpha = 0.75, shape = 21) + 
  scale_size_continuous(limits = c(0, 100), range = c(1,10), breaks = c(1,25,50,75,100)) + 
  labs( x= "", y = "", size = "Frequency (%)", fill = "") + 
  scale_fill_manual(values = scales::viridis_pal(option = "G")(5)[1:4], guide = FALSE) + 
  scale_y_discrete(limits = rev(levels(df2$symptoms)))+
  theme_minimal()+theme(legend.position = "top")

## Radar plots ##

data_cluster1<-colSums(df[df$clusters==1,-c(1,23)], na.rm=T)/table(df$clusters==1)[2]*100
data_cluster1 <-as.data.frame(rbind(rep(100,21) , rep(0,21) , data_cluster1))
p1 <- as.ggplot(~radarchart(data_cluster1, axistype=1, 
                  #custom polygon
                  pcol=alpha(scales::viridis_pal(option = "G")(5)[1],0.9) , pfcol=alpha(scales::viridis_pal(option = "G")(5)[1],0.5) , plwd=4 , 
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,5), cglwd=0.8,
                  #custom labels
                  vlcex=1))

data_cluster2<-colSums(df[df$clusters==2,-c(1,23)], na.rm=T)/table(df$clusters==2)[2]*100
data_cluster2 <-as.data.frame(rbind(rep(100,21) , rep(0,21) , data_cluster2))
p2 <- as.ggplot(~radarchart(data_cluster2, axistype=1, 
                 #custom polygon
                 pcol=alpha(scales::viridis_pal(option = "G")(5)[2],0.9) , pfcol=alpha(scales::viridis_pal(option = "G")(5)[2],0.5) , plwd=4 , 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,5), cglwd=0.8,
                 #custom labels
                 vlcex=1))

data_cluster3<-colSums(df[df$clusters==3,-c(1,23)], na.rm=T)/table(df$clusters==3)[2]*100
data_cluster3 <-as.data.frame(rbind(rep(100,21) , rep(0,21) , data_cluster3))
p3 <- as.ggplot(~radarchart(data_cluster3, axistype=1, 
                 #custom polygon
                 pcol=alpha(scales::viridis_pal(option = "G")(5)[3],0.9) , pfcol=alpha(scales::viridis_pal(option = "G")(5)[3],0.5) , plwd=4 , 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,5), cglwd=0.8,
                 #custom labels
                 vlcex=1))

data_cluster4<-colSums(df[df$clusters==4,-c(1,23)], na.rm=T)/table(df$clusters==4)[2]*100
data_cluster4 <-as.data.frame(rbind(rep(100,21) , rep(0,21) , data_cluster4))
p4 <- as.ggplot(~radarchart(data_cluster4, axistype=1, 
                 #custom polygon
                 pcol=alpha(scales::viridis_pal(option = "G")(5)[4],0.9) , pfcol=alpha(scales::viridis_pal(option = "G")(5)[4],0.5) , plwd=4 , 
                 #custom the grid
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,5), cglwd=0.8,
                 #custom labels
                 vlcex=1))



fig5<-ggarrange(fig5a, ggarrange(p1,p2,p3,p4, labels = LETTERS[2:5]),labels = c("A",""), ncol = 2, nrow=1, widths = c(0.35, 0.65))


ggsave(fig5,
       filename = "Figure5.jpg", 
       width = 40, 
       height = 25,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

tab1 <- ensanut_2022 %>% filter(h0303.x>=20) %>%
  left_join(df %>% select(FOLIO_INT, clusters), by="FOLIO_INT") %>%
  filter(pacs==1) %>%
  dplyr::select(clusters, sex, age, reinfection, omicron,vacuna,duration3,1148:1169, pacs_score,DISLI_cat, cesd, diabetes, Hypertension, daily_smoking,typeVaccine,inability) %>%
  tbl_summary(by = clusters,missing = "ifany")%>%
  add_p() %>%
  bold_labels()%>%
  add_overall()%>%
  modify_spanning_header(all_stat_cols() ~ "**Overall Sample**")%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label))%>%
  as_flex_table()%>% 
  align(align = "center",part = "all") %>% 
  autofit()

doc <- read_docx() %>% body_add_flextable(value = tab1, split = TRUE) %>%
  body_end_section_landscape() %>% print(target = "tablaS4_1.docx")


#### Survey objects ####
ensanut_2022_2 <- ensanut_2022 %>% filter(!is.na(ponde_f.x)) %>% filter(h0303.x>=20)
ensanut_2022_survey<-svydesign(data=ensanut_2022_2, id=~ID, strata=~est_sel.x, weights=~ponde_f.x, nest=TRUE)
ensanut_2022_survey2<-subset(ensanut_2022_survey, (ensanut_2022_2$h1205 %in% c(1,2,3,4,8))==T)
ensanut_2022_survey3<-subset(ensanut_2022_survey, ensanut_2022_2$n_pos==1)
ensanut_2022_survey4<-subset(ensanut_2022_survey, secuela==1)
ensanut_2022_survey5<-subset(ensanut_2022_survey, pacs==1)
ensanut_2022_survey6<-subset(ensanut_2022_survey, ensanut_2022_2$n_pos==1 & ensanut_2022_2$Sinovac!=1)
ensanut_2022_survey7<-subset(ensanut_2022_survey, (ensanut_2022_2$h1205 %in% c(1,2,3,4,8))==T & ensanut_2022_2$Sinovac!=1)
ensanut_2022_survey8<-subset(ensanut_2022_survey, ensanut_2022_2$Sinovac!=1)
ensanut_2022_survey9<-subset(ensanut_2022_survey, pacs==0 & covid==1)
ensanut_2022_survey10<-subset(ensanut_2022_survey, pacs2==1)

## Gráfico
nrow(ensanut_2022)
table(ensanut_2022_2$n_pos)

t1<-ensanut_2022 %>% filter(!is.na(ponde_f.x)) %>% filter(h0303.x>=20) %>% filter(!is.na(n_pos))


#### Flowchart numbers ####
nrow(ensanut_2022) ## Number of interviews
nrow(ensanut_2022 %>% filter(h0303.x>=20)) ## Individuals ≥20 years
nrow(ensanut_2022 %>% filter(h0303.x>=20) %>% filter(covid==1)) ## COVID-19(+)
nrow(ensanut_2022 %>% filter(h0303.x>=20) %>% filter(!is.na(n_pos))) ## Seropositivity data
nrow(ensanut_2022 %>% filter(h0303.x>=20) %>% filter(!is.na(n_pos)) %>% filter(covid==1)) ## COVID-19(+)
nrow(ensanut_2022 %>% filter(h0303.x>=20) %>% filter(!is.na(n_pos)) %>% filter(covid==1))
#### Prevalence amongst COVID-19 positive ####
svyby(~covid, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(covid*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Diagnosed COVID-19") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~n_pos, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(n_pos*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Seroprevalence") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~vacuna, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(vacuna*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Vaccination") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~onlydose1, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(onlydose1*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="One dose") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~onlydose2, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(onlydose2*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fully vaccinated") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~onlydose3, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(onlydose3*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="1 booster") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~onlydose4, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(onlydose4*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="2 boosters") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~time1, by=~YEAR, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(time1*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="<1 month") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~time2, by=~YEAR, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(time2*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="1-3 months") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~time3, by=~YEAR, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(time3*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="3-6 months month") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~time4, by=~YEAR, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(time4*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster=">6 months month") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~time5, by=~YEAR, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(time5*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~secuela, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~secuela, by=~YEAR, design=ensanut_2022_survey, svytotal, na.rm=T) %>%  
  mutate(prop=round(secuela, digits=2), IC95=round((se*1.96), digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Total sequelae") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~secuela, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~secuela, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs, by=~YEAR, design=ensanut_2022_survey, svytotal, na.rm=T) %>%  
  mutate(prop=round(pacs, digits=2), IC95=round((se*1.96), digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Total PASC") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs2, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs2*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs2, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs2*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~pacs2, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs2*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~inability, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(inability*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~inability, by=~YEAR, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(inability*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~inability, by=~YEAR, design=ensanut_2022_survey5, svymean, na.rm=T) %>%  
  mutate(prop=round(inability*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~inability, by=~YEAR, design=ensanut_2022_survey9, svymean, na.rm=T) %>%  
  mutate(prop=round(inability*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~inability, by=~YEAR, design=ensanut_2022_survey10, svymean, na.rm=T) %>%  
  mutate(prop=round(inability*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

svyby(~Sinovac, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(Sinovac*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

pacs_year<-svyby(~pacs, by=~YEAR+h1207b, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Still persist") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
pacs_year$year<-c(2020, 2021, 2022)

pacs_year %>% 
  mutate(year=factor(year, levels=year)) %>% 
  ggplot(aes(x=year, y=prop)) +
  geom_bar(stat="identity", width=0.3) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.1)+
  geom_text(aes(label=round(prop,2)), vjust=2, color="white", size=3)+
  theme_bw() +
  xlab("")+
  ylab("PASC prevalence per year of last SARS-CoV-2 infection (%, 95%CI)")

#### Sequelae subtypes ####
r1<-svyby(~secuela, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Any persistent\nsymptom") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

r2<-svyby(~acutePASC, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(acutePASC*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sub-acute SARS-CoV-2\ninfection") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

r3<-svyby(~chronicPASC, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(chronicPASC*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-acute\nCOVID-19") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

pasc_pos<-rbind(r1,r2,r3)

fs3a<-pasc_pos %>% 
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot(aes(x=cluster, y=prop)) +
  geom_bar(stat="identity", width=0.3) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.1)+
  geom_text(aes(label=round(prop,2)), vjust=2, color="white", size=3)+
  theme_bw() +
  xlab("")+
  ylab("PASC subtype prevalence (%, 95%CI)")

fs3<-ggarrange("",fs3a,"", labels = c("", "C", ""), widths = c(0.25, 0.5, 0.25), nrow=1, ncol=3)


s3<-svyby(~olfato, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(olfato*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Loss of smell or taste") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s4<-svyby(~fog, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fog*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Brain fog") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s5<-svyby(~post_malaise, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(post_malaise*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-exertional malaise") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s6<-svyby(~cough, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(cough*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Cough") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s7<-svyby(~chest_pain, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(chest_pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Chest Pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s8<-svyby(~gastrointestinal, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(gastrointestinal*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Gastrointestinal") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s9<-svyby(~dizzy, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(dizzy*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dizziness") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s10<-svyby(~headache, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(headache*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Headache") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s11<-svyby(~pain, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Musculoskeletal pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s12<-svyby(~weightloss, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(weightloss*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Weight loss") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s13<-svyby(~apetite, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(apetite*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Decreased apetite") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s14<-svyby(~depression, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(depression*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Depression") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s15<-svyby(~anxiety, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(anxiety*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Anxiety") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s16<-svyby(~fever, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fever*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fever") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s17<-svyby(~sleep, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(sleep*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sleep disorders") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s18<-svyby(~kidney, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(kidney*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Kidney problems") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s19<-svyby(~breath, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(breath*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dyspnea") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s20<-svyby(~fatigue, by=~YEAR+acutePASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fatigue*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fatigue") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

symptoms1<-rbind(s3,s4,s5,s6,s7,s8,s9,s10,
                s11,s12,s13,s14,s15,s16,s17,s18,s19,s20)
symptoms1$acute<-rep(c("Non-acute", "Acute PASC"),18)
  
fs1<-symptoms1 %>% filter(acute=="Acute PASC") %>%
  arrange(prop) %>%
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot(aes(x=cluster, y=prop)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  geom_point( size=4) +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Prevalence,subacute COVID-19 (%, 95%CI)")

s3<-svyby(~olfato, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(olfato*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Loss of smell or taste") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s4<-svyby(~fog, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fog*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Brain fog") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s5<-svyby(~post_malaise, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(post_malaise*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-exertional malaise") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s6<-svyby(~cough, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(cough*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Cough") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s7<-svyby(~chest_pain, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(chest_pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Chest Pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s8<-svyby(~gastrointestinal, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(gastrointestinal*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Gastrointestinal") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s9<-svyby(~dizzy, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(dizzy*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dizziness") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s10<-svyby(~headache, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(headache*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Headache") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s11<-svyby(~pain, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Musculoskeletal pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s12<-svyby(~weightloss, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(weightloss*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Weight loss") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s13<-svyby(~apetite, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(apetite*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Decreased apetite") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s14<-svyby(~depression, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(depression*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Depression") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s15<-svyby(~anxiety, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(anxiety*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Anxiety") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s16<-svyby(~fever, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fever*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fever") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s17<-svyby(~sleep, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(sleep*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sleep disorders") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s18<-svyby(~kidney, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(kidney*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Kidney problems") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s19<-svyby(~breath, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(breath*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dyspnea") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s20<-svyby(~fatigue, by=~YEAR+chronicPASC, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fatigue*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fatigue") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

symptoms2<-rbind(s3,s4,s5,s6,s7,s8,s9,s10,
                s11,s12,s13,s14,s15,s16,s17,s18,s19,s20)
symptoms2$chronic<-rep(c("Non-chronic", "Chronic PASC"))

fs2<-symptoms2 %>% filter(chronic=="Chronic PASC")%>%
  arrange(prop) %>%
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot( aes(x=cluster, y=prop)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  geom_point( size=4) +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Prevalence, chronic post-COVID-19 (%, 95%CI)")

figs1a<-ggarrange(fs1,fs2, labels = LETTERS[1:2],ncol=2, nrow=1)
figs1<-ggarrange(figs1a,fs3, ncol=1, nrow=2)


ggsave(figs1,
       filename = "FigureS1.jpg", 
       width = 30, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Figure 1: Symptom and PASC prevalence in SARS-CoV-2 seropositive adults ####

s2<-svyby(~secuela, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Any symptom") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s3<-svyby(~olfato, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(olfato*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Loss of smell or taste") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s4<-svyby(~fog, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fog*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Brain fog") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s5<-svyby(~post_malaise, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(post_malaise*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-exertional malaise") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s6<-svyby(~cough, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(cough*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Cough") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s7<-svyby(~chest_pain, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(chest_pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Chest Pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s8<-svyby(~gastrointestinal, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(gastrointestinal*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Gastrointestinal") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s9<-svyby(~dizzy, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(dizzy*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dizziness") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s10<-svyby(~headache, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(headache*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Headache") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s11<-svyby(~pain, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Musculoskeletal pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s12<-svyby(~weightloss, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(weightloss*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Weight loss") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s13<-svyby(~apetite, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(apetite*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Decreased apetite") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s14<-svyby(~depression, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(depression*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Depression") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s15<-svyby(~anxiety, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(anxiety*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Anxiety") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s16<-svyby(~fever, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fever*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fever") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s17<-svyby(~sleep, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(sleep*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sleep disorders") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s18<-svyby(~kidney, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(kidney*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Kidney problems") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s19<-svyby(~breath, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(breath*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dyspnea") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s20<-svyby(~fatigue, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fatigue*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fatigue") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

symptoms<-rbind(s2,s3,s4,s5,s6,s7,s8,s9,s10,
                s11,s12,s13,s14,s15,s16,s17,s18,s19,s20)

f1<-symptoms %>% 
  arrange(prop) %>%
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot( aes(x=cluster, y=prop)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  geom_point( size=4) +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Prevalence amongst N-protein seropositive (%, 95%CI)")

t1<-svyby(~pacs, by=~YEAR, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Overall \npopulation") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
t2<-svyby(~pacs, by=~YEAR, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="N-protein \nSeropositive") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
t3<-svyby(~pacs, by=~YEAR, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Previously \ndiagnosed COVID-19") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

pacs<-rbind(t1,t2,t3)

f2<-pacs %>% 
  arrange(prop) %>%
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot(aes(x=cluster, y=prop)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.1)+
  geom_bar(stat="identity", width=0.4) +
  geom_text(aes(label=round(prop,2)), vjust=2, color="white", size=3)+
  theme_bw() +
  xlab("")+
  ylab("PASC prevalence (%, 95%CI)")

fig1<-ggarrange(f1,f2, labels = LETTERS[1:2], widths = c(0.6, 0.4))

ggsave(fig1,
       filename = "Figure1.jpg", 
       width = 25, 
       height = 10,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

ggsave(fig1,
       filename = "Figure1.pdf", 
       width = 25, 
       height = 10,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Figure 1a: Symptom and PASC prevalence in SARS-CoV-2 seropositive adults without CoronaVac ####

s2<-svyby(~secuela, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Any symptom") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s3<-svyby(~olfato, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(olfato*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Loss of smell or taste") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s4<-svyby(~fog, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(fog*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Brain fog") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s5<-svyby(~post_malaise, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(post_malaise*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-exertional malaise") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s6<-svyby(~cough, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(cough*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Cough") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s7<-svyby(~chest_pain, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(chest_pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Chest Pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s8<-svyby(~gastrointestinal, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(gastrointestinal*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Gastrointestinal") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s9<-svyby(~dizzy, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(dizzy*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dizziness") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s10<-svyby(~headache, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(headache*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Headache") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s11<-svyby(~pain, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Musculoskeletal pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s12<-svyby(~weightloss, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(weightloss*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Weight loss") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s13<-svyby(~apetite, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(apetite*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Decreased apetite") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s14<-svyby(~depression, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(depression*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Depression") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s15<-svyby(~anxiety, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(anxiety*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Anxiety") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s16<-svyby(~fever, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(fever*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fever") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s17<-svyby(~sleep, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(sleep*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sleep disorders") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s18<-svyby(~kidney, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(kidney*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Kidney problems") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s19<-svyby(~breath, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(breath*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dyspnea") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s20<-svyby(~fatigue, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(fatigue*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fatigue") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

symptoms<-rbind(s2,s3,s4,s5,s6,s7,s8,s9,s10,
                s11,s12,s13,s14,s15,s16,s17,s18,s19,s20)

f1<-symptoms %>% 
  arrange(prop) %>%
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot( aes(x=cluster, y=prop)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  geom_point( size=4) +
  coord_flip() +
  theme_bw() +
  xlab("")+
  ylab("Prevalence amongst N-protein seropositive (%, 95%CI)")

t1<-svyby(~pacs, by=~YEAR, design=ensanut_2022_survey8, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Overall \npopulation") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
t2<-svyby(~pacs, by=~YEAR, design=ensanut_2022_survey6, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="N-protein \nSeropositive") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
t3<-svyby(~pacs, by=~YEAR, design=ensanut_2022_survey7, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Previously \ndiagnosed COVID-19") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

pacs<-rbind(t1,t2,t3)

f2<-pacs %>% 
  arrange(prop) %>%
  mutate(cluster=factor(cluster, levels=cluster)) %>% 
  ggplot(aes(x=cluster, y=prop)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.1)+
  geom_bar(stat="identity", width=0.4) +
  geom_text(aes(label=round(prop,2)), vjust=2, color="white", size=3)+
  theme_bw() +
  xlab("")+
  ylab("PASC prevalence (%, 95%CI)")

fig1a<-ggarrange(f1,f2, labels = LETTERS[1:2], widths = c(0.6, 0.4))

ggsave(fig1a,
       filename = "Figure1a.jpg", 
       width = 25, 
       height = 10,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

ggsave(fig1a,
       filename = "Figure1a.pdf", 
       width = 25, 
       height = 10,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Symptom and PASC prevalence in SARS-CoV-2 seropositive adults by sex ####

s2<-svyby(~secuela, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Any symptom") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s3<-svyby(~olfato, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(olfato*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Loss of smell or taste") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s4<-svyby(~fog, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fog*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Brain fog") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s5<-svyby(~post_malaise, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(post_malaise*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-excertional malaise") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s6<-svyby(~cough, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(cough*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Cough") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s7<-svyby(~chest_pain, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(chest_pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Chest Pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s8<-svyby(~gastrointestinal, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(gastrointestinal*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Gastrointestinal") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s9<-svyby(~dizzy, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(dizzy*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dizziness") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s10<-svyby(~headache, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(headache*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Headache") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s11<-svyby(~pain, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Musculoskeletal pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s12<-svyby(~weightloss, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(weightloss*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Weight loss") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s13<-svyby(~apetite, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(apetite*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Decreased apetite") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s14<-svyby(~depression, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(depression*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Depression") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s15<-svyby(~anxiety, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(anxiety*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Anxiety") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s16<-svyby(~fever, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fever*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fever") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s17<-svyby(~sleep, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(sleep*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sleep disorders") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s18<-svyby(~kidney, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(kidney*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Kidney problems") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s19<-svyby(~breath, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(breath*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dyspnea") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s20<-svyby(~fatigue, by=~YEAR+sexo, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fatigue*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fatigue") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

symptoms_sex<-rbind(s2,s3,s4,s5,s6,s7,s8,s9,s10,
                s11,s12,s13,s14,s15,s16,s17,s18, s19,s20)
symptoms_sex$sexo<-rep(c(0,1),19)

fs2a<-symptoms_sex %>% filter(cluster!="Sequel") %>%
  mutate(sexo=factor(sexo, labels = c("Female", "Male"))) %>%
  mutate(cluster=factor(cluster)) %>% 
  mutate(cluster=cluster %>% fct_reorder(prop)) %>%
  ggplot(aes(x=cluster, y=prop, col=sexo, group=sexo)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  geom_point( size=4) +
  coord_flip() +
  theme_bw() +
  xlab("")+facet_wrap(~sexo)+
  ylab("Prevalence amongst N-protein seropositive (%, 95%CI)")+labs(col="Sex")+
  theme(legend.position = "top")


#### Symptom and PASC prevalence in SARS-CoV-2 seropositive adults by reinfection status ####

s2<-svyby(~secuela, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(secuela*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Any symptom") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s3<-svyby(~olfato, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(olfato*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Loss of smell or taste") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s4<-svyby(~fog, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(fog*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Brain fog") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s5<-svyby(~post_malaise, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(post_malaise*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Post-excertional malaise") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s6<-svyby(~cough, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(cough*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Cough") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s7<-svyby(~chest_pain, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(chest_pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Chest Pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s8<-svyby(~gastrointestinal, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(gastrointestinal*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Gastrointestinal") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s9<-svyby(~dizzy, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(dizzy*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dizziness") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s10<-svyby(~headache, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(headache*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Headache") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s11<-svyby(~pain, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pain*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Musculoskeletal pain") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s12<-svyby(~weightloss, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(weightloss*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Weight loss") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s13<-svyby(~apetite, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(apetite*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Decreased apetite") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s14<-svyby(~depression, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(depression*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Depression") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s15<-svyby(~anxiety, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(anxiety*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Anxiety") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s16<-svyby(~fever, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(fever*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fever") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s17<-svyby(~sleep, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(sleep*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Sleep disorders") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s18<-svyby(~kidney, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(kidney*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Kidney problems") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s19<-svyby(~breath, by=~YEAR+reinfection, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(breath*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Dyspnea") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
s20<-svyby(~fatigue, by=~YEAR+reinfection, design=ensanut_2022_survey3, svymean, na.rm=T) %>%  
  mutate(prop=round(fatigue*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Fatigue") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

symptoms_status<-rbind(s2,s3,s4,s5,s6,s7,s8,s9,s10,
                s11,s12,s13,s14,s15,s16,s17,s18,s19,s20)
symptoms_status$reinfection<-factor(rep(c(0,1),19), labels = c("Primoinfection", "Reinfection"))

fs2b<-symptoms_status %>% filter(cluster!="Sequel") %>% 
  filter(cluster!="Sequel") %>%
  mutate(cluster=factor(cluster)) %>% 
  mutate(cluster=cluster %>% fct_reorder(prop)) %>%
  ggplot(aes(x=cluster, y=prop, col=reinfection, group=reinfection)) +
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  geom_point( size=4) +
  coord_flip() +
  theme_bw() +
  xlab("")+facet_wrap(~reinfection)+labs(col="SARS-CoV-2 status")+
  ylab("Prevalence amongst cases with previous COVID-19 (%, 95%CI)")+scale_color_jama()+
  theme(legend.position = "top")


fs2<-ggarrange(fs2a, fs2b, labels = c("A", "B"), ncol=1, nrow=2)

ggsave(fs2,
       filename = "FigureS2.jpg", 
       width = 20, 
       height = 21,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

ggsave(fs2,
       filename = "FigureS2.pdf", 
       width = 20, 
       height = 21,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Figure 2A: PASC by region ####
p1<-svyby(~pacs, by=~YEAR+SUBREGION, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

#Overall
data <-data.frame(SUBREGION = c(1:8),SUBREGION2 = c(1:8),prop=p1$prop, LI=p1$lIC95, LS=p1$uIC95)
data$SUBREGION2<-factor(data$SUBREGION2, labels = c("Northern\nPacific","US Border","Central\nPacific","Central\nNorthern","Center","Mexico City\nMexico State","Southern\nPacific","Peninsula"))

data_region<-geom_mx%>%dplyr::left_join(data,by="SUBREGION")
geo_data <- poly2nb(data_region$geometry)
color<-get_palette(palette = "BrBG",8)
rev<-rev(color)

f1<-data %>% 
  arrange(desc(prop)) %>% 
  mutate(SUBREGION2=factor(SUBREGION2, levels=SUBREGION2)) %>% 
  ggplot( aes(x=SUBREGION2, y=prop)) +
  geom_bar(stat = "identity", width = 0.6, fill=rev) +
  geom_errorbar(aes(ymin = LI, ymax = LS), position = position_dodge(0.9), width = 0.05, color="black", size=0.3)+
  theme_bw() +
  xlab("")+
  ylab("PASC prevalence in cases with\npreviously diagnosed COVID-19 (%)")
  
f2<-data_region %>%
  ggplot() +
  geom_sf(mapping = aes(fill = factor(prop), geometry=geometry), color = "darkgray", size = 0.1, show.legend = T) +
  ggtitle("Post-acute SARS-CoV-2 sequelae prevalence")+
  scale_fill_manual(values = color)+
  labs(fill="Prevalence (%)\nENSANUT 2022")+
  theme_map()+
  theme(legend.position = "right",
        legend.margin=margin(0,0,0,0),
        legend.key.size = unit(0.5, 'cm'))

fig2a<-ggarrange(f1,f2, widths = c(0.45,0.55))


#### Figures 2B-E: PASC prevalence by modifying factors ####

p1<-svyby(~pacs, by=~YEAR+age_cat+sex, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
age_cat2<-c("20-29", "30-39", "40-49", "50-59", "60-69", "≥70")
p1$age_cat<-rep(age_cat2,2)
p1$sex<-c(rep("Female",6),rep("Male",6))

t1<-p1 %>%
  mutate(age_cat=factor(age_cat, levels=age_cat2))%>%
  ggplot(aes(x=age_cat, y=prop, col=sex))+
  geom_point(size=4,position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  theme_bw()+
  ylab("PACS prevalence in cases with\npreviously diagnosed COVID-19 (%)")+
  xlab("Age groups (years)")+labs(col="Sex")+
  theme(legend.position = "top")+facet_wrap(~sex)

p2<-svyby(~pacs, by=~YEAR+variant+vacuna, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
p2$variant<-rep(c("Ancestral", "B.1.1.519", "Delta", "Omicron"),2)
p2$vacuna<-c(rep("Unvaccinated",4),rep("Vaccinated",4))

t2<-p2 %>%
  ggplot(aes(x=variant, y=prop, col=vacuna))+
  geom_point(size=4,position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  theme_bw()+
  ylab("PASC prevalence in cases with\npreviously diagnosed COVID-19 (%)")+
  xlab("SARS-CoV-2 predominant variant at last infection")+labs(col="SARS-Cov-2 vaccination status")+
  ylim(0,50)+
  theme(legend.position = "top")+facet_wrap(~vacuna)+
  scale_color_jama()

p3<-svyby(~pacs, by=~YEAR+DISLI_cat+Setting, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
p3$disli<-c("Q1", "Q2","Q3","Q4")
p3$Setting<-c(rep("Rural",4), rep("Urban",4))

t3<-p3 %>%
  ggplot(aes(x=disli, y=prop, col=disli))+
  geom_point(size=4,position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  theme_bw()+
  ylab("PASC prevalence in cases with\npreviously diagnosed COVID-19 (%)")+
  xlab("DISLI quartile")+labs(col="DISLI quartile")+
  theme(legend.position = "top")+facet_wrap(~Setting)+
  scale_color_economist()

p4<-svyby(~pacs, by=~YEAR+reinfection+omicron, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
p4$variant<-rep(c("Primoinfection", "Reinfection"),2)
p4$omicron<-c("Non-Omicron", "Non-Omicron","Omicron","Omicron")
t4<-p4 %>%
  ggplot(aes(x=variant, y=prop, col=omicron))+
  geom_point(size=4,position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  theme_bw()+
  ylab("PASC prevalence in cases with\npreviously diagnosed COVID-19 (%)")+
  xlab("SARS-CoV-2 infection status")+labs(col="Variant predominance")+
  theme(legend.position = "top")+facet_wrap(~omicron)+
  scale_color_startrek()

fig2b<-ggarrange(t1,t2,t3,t4, widths = c(0.6, 0.4), nrow=2, ncol=2,labels = c("B", "C", "D", "E"))

fig2<-ggarrange(fig2a, fig2b, ncol=1, nrow=2,labels = c("A", ""), heights = c(0.4, 0.6))

ggsave(fig2,
       filename = "Figure2.jpg", 
       width = 35, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

ggsave(fig2,
       filename = "Figure2.pdf", 
       width = 35, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### PASC prevalence by vaccine type ####
p2<-svyby(~pacs, by=~YEAR+omicron+reinfection+typeVaccine, design=ensanut_2022_survey2, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
p2$lIC95[15]<-0.1
p2$variant<-rep(c("Non-omicron", "Omicron"),10)
p2$reinfection<-rep(c("Primoinfection", "Primoinfection","Reinfection", "Reinfection"),5)
p2$vacuna<-c(rep("Unvaccinated",4), rep("mRNA",4), rep("Adenovirus vector",4), rep("Inactivated virus",4) ,rep("Other",4))

t2<-p2 %>%filter(vacuna!="Other") %>%
  ggplot(aes(x=variant, y=prop, col=reinfection, group=reinfection))+
  geom_point(size=4,position = position_dodge(width = 0.9))+
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(width = 0.9), width = 0.25)+
  theme_bw()+
  ylab("PASC prevalence in cases with\npreviously diagnosed COVID-19 (%)")+
  xlab("SARS-CoV-2 predominant variant at last infection")+labs(col="Status")+
  ylim(0,80)+
  theme(legend.position = "top")+facet_wrap(~vacuna)+
  scale_color_jama()

#### Table S1: PASC positive vs. Unspecified ####

tab1 <- ensanut_2022_2 %>% filter(covid==1) %>%
  dplyr::select(pacs2, sex, age, reinfection, omicron,vacuna,duration3,1145:1165, pacs_score,DISLI_cat, cesd, diabetes, Hypertension, daily_smoking,typeVaccine,inability) %>%
  tbl_summary(by = pacs2,missing = "ifany")%>%
  add_p() %>%
  bold_labels()%>%
  add_overall()%>%
  modify_spanning_header(all_stat_cols() ~ "**Overall Sample**")%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label))%>%
  as_flex_table()%>% 
  align(align = "center",part = "all") %>% 
  autofit()

doc <- read_docx() %>% body_add_flextable(value = tab1, split = TRUE) %>%
  body_end_section_landscape() %>% print(target = "tablaS1_1.docx")


#### Table S1: PASC CDC vs. Unspecified ####

tab1 <- ensanut_2022_2 %>% filter(covid==1) %>%
  dplyr::select(secuela, sex, age, reinfection, omicron,vacuna,duration3,1145:1169, pacs_score,DISLI_cat, cesd, diabetes, Hypertension, daily_smoking,typeVaccine,inability) %>%
  tbl_summary(by = secuela,missing = "ifany")%>%
  add_p() %>%
  bold_labels()%>%
  add_overall()%>%
  modify_spanning_header(all_stat_cols() ~ "**Overall Sample**")%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label))%>%
  as_flex_table()%>% 
  align(align = "center",part = "all") %>% 
  autofit()

doc <- read_docx() %>% body_add_flextable(value = tab1, split = TRUE) %>%
  body_end_section_landscape() %>% print(target = "tablaS3_1.docx")



#### Table 1: PASC NICE vs. Unspecified ####

tab1 <- ensanut_2022_2 %>% filter(covid==1) %>%
  dplyr::select(pacs, sex, age, reinfection, omicron,vacuna,duration3,1145:1165, pacs_score,DISLI_cat, cesd, diabetes, Hypertension, AMI_fin, ANG_fin, HFA_fin, EVC_fin, CKD_fin,imc_cat2,daily_smoking,typeVaccine,inability) %>%
  tbl_summary(by = pacs,missing = "ifany")%>%
  add_p() %>%
  bold_labels()%>%
  add_overall()%>%
  modify_spanning_header(all_stat_cols() ~ "**Overall Sample**")%>%
  modify_table_body(
    dplyr::mutate,
    label = ifelse(label == "N missing (% missing)",
                   "Unknown",
                   label))%>%
  as_flex_table()%>% 
  align(align = "center",part = "all") %>% 
  autofit()

doc <- read_docx() %>% body_add_flextable(value = tab1, split = TRUE) %>%
  body_end_section_landscape() %>% print(target = "tabla1_1.docx")

#### Prevalence of vaccination ####
vacuna<-svyby(~unvaccinated, by=~YEAR+SUBREGION, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(unvaccinated*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="Diagnosed COVID-19") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)
vacuna$SUBREGION<-c(1:8)

p1<-svyby(~pacs, by=~YEAR+CVE_ENT, design=ensanut_2022_survey, svymean, na.rm=T) %>%  
  mutate(prop=round(pacs*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

#Overall
data <-data.frame(CVE_ENT = c(1:32),prop=p1$prop, LI=p1$lIC95, LS=p1$uIC95)

data_region<-geom_mx%>%dplyr::left_join(data,by="CVE_ENT")
geo_data <- poly2nb(data_region$geometry)

moran.test(data_region$prop, nb2listw(geo_data))

data_region<-data_region%>%dplyr::left_join(vacuna,by="SUBREGION")

spdep::lee.test(data_region$prop.x, data_region$prop.y, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)

covid1<-bi_class(data_region, y = prop.y, x = prop.x, style = "quantile", dim = 3)
labels <- biscale::bi_class_breaks(data_region, y = prop.x, x = prop.y, style = "quantile", dim = 3, dig_lab = 2, split = FALSE)

Fig3A<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = covid1, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 3)+ ggtitle("PASC prevalence vs. COVID-19 vaccination") +
              theme_map()+ theme(text = element_text(size = 12)) , 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 3,
                      xlab = "\n\n\n\n\n\n\n\nUnvaccinated",
                      ylab = "PASC prevalence (%)",
                      size = 12), 0.7, .5, 0.25, 0.25)



#### Figure 3: Models ####
model<-ensanut_2022_2 %>% filter(covid==1) %>%
  dplyr::select(pacs, pacs2,omicron, variant,age,age_cat, sex, reinfection, vacuna, diabetes, CVE_ENT, swts, estrato.x, 
                DISLI_cat,fa0400,daily_smoking, daily_drinking, Hypertension, cesd, weight_loss, cvd,doses, h0317a, h0311,typeVaccine, duration2, inability, h1205) %>%
  mutate('Predominant variant'=factor(variant, labels = c("Ancestral strain", "B.1.1.519", "Delta", "Omicron")))%>%
  rename(PASC=pacs, 'Omicron variant'=omicron, 'Age (years)'=age_cat, Reinfection=reinfection, Sleep=fa0400,'Number of reinfections'=h1205,
         Vaccinated =vacuna,'Male sex'=sex, Diabetes=diabetes, Setting=estrato.x, DISLI=DISLI_cat, 'COVID-19 vaccine doses'=doses,
         `Daily smoking`=daily_smoking, Drinking=daily_drinking, 'CES-D'=cesd,'Weight loss'=weight_loss, CVD=cvd,Indigenous=h0311,'Vaccine type'=typeVaccine, 'Symptom duration'=duration2, 'Disabilitating symptoms'=inability)
nrow(model)
age_cat2<-c("20-29", "30-39", "40-49", "50-59", "60-69", "≥70")
model$`Age (years)`<-factor(model$`Age (years)`, labels = age_cat2)
model$Setting[model$Setting==3]<-2
model$Setting<-factor(model$Setting, labels = c("Rural", "Urban"))
model$`Number of reinfections`<-factor(model$`Number of reinfections`, labels = c("1","2","≥3","≥3","≥3"))
model$Setting<-relevel(model$Setting, ref="Urban")
model$DISLI<-relevel(model$DISLI, ref="Q1")
model$`Depressive symptoms`<-ifelse((model$age<=60 & model$`CES-D`>=9)|(model$age>60 & model$`CES-D`>=5),1,0)
model$Education[model$h0317a %in% c(0,1,2)]<-1
model$Education[model$h0317a %in% c(3)]<-2
model$Education[model$h0317a %in% c(4,5,6,7,8)]<-3
model$Education[model$h0317a %in% c(9,10,11,12)]<-4
model$Education<-factor(model$Education, labels = c("Primary or less", "Secondary", "High school or technical", "University or higher"))
model$`Booster vaccination`<-ifelse(model$`COVID-19 vaccine doses`>2,1,0)
model$`COVID-19 vaccine doses`<-factor(model$`COVID-19 vaccine doses`)
model$`PASC score`<-factor(model$pacs2, labels = c("<12", "≥12"))

## Models 
m1<-glm(PASC ~ `Omicron variant` + `Age (years)` + `Male sex` + Vaccinated + Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model,weights=swts,start=c(rep(-0.01,21)))
fig3a<-ggcoef_table(m1, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)
m2<-glm(pacs2 ~ `Omicron variant` + `Age (years)` + `Male sex` + Vaccinated + Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model,weights=swts, start=c(rep(-0.01,21)))
fig3b<-ggcoef_table(m2, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)
fig3<-ggarrange(fig3a, fig3b, labels = c("A", "B"))

ggsave(fig3,
       filename = "Figure3.jpg", 
       width = 40, 
       height = 15,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


m0<-glm(PASC ~ `Omicron variant` + `Age (years)` + `Male sex` + `COVID-19 vaccine doses` + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model,weights=swts,start=c(rep(-0.0001,23)))
fig3a<-ggcoef_table(m0, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)
m2<-glm(pacs2 ~ `Omicron variant` + `Age (years)` + `Male sex` + `COVID-19 vaccine doses` + Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model,weights=swts,start=c(rep(-0.01,24)))
fig3b<-ggcoef_table(m2, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)

figs3<-ggarrange(fig3a, fig3b, labels = c("A", "B"))

ggsave(figs3,
       filename = "FigureS3.jpg", 
       width = 40, 
       height = 15,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

m1<-glm(PASC ~ `Omicron variant` + `Age (years)` + `Male sex` + `Booster vaccination` + Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model %>% filter(Vaccinated==1),weights=swts,start=c(rep(-0.01,21)))
fig3a<-ggcoef_table(m1, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)

m2<-glm(pacs2 ~ `Omicron variant` + `Age (years)` + `Male sex` + `Booster vaccination` + Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model%>% filter(Vaccinated==1),weights=swts,start=c(rep(-0.01,21)))
fig3b<-ggcoef_table(m2, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)

figs3b<-ggarrange(fig3a, fig3b, labels = c("A", "B"))

ggsave(figs3b,
       filename = "FigureS3b.jpg", 
       width = 40, 
       height = 15,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

m1<-glm(PASC ~ `Omicron variant` + `Age (years)` + `Male sex` + `Vaccine type`+Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education + Indigenous,family=binomial(link="log"),data=model,weights=swts,start=c(rep(-0.0001,25)))
fig3a<-ggcoef_table(m1, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)

m1<-glm(pacs2 ~ `Omicron variant` + `Age (years)` + `Male sex` + `Vaccine type` + Reinfection + Diabetes + Hypertension + `Daily smoking` + `Depressive symptoms` + DISLI + Setting + Education,family=binomial(link="log"),data=model,weights=swts,start=c(rep(-0.0001,24)))
fig3b<-ggcoef_table(m1, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL)

figs4<-ggarrange(fig3a, fig3b, labels = c("A", "B"))

ggsave(figs4,
       filename = "FigureS4.jpg", 
       width = 40, 
       height = 17,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Figure 4: Incapacitating symptoms ####
severe<-svyby(~inability, by=~YEAR+duration2+pacs2, design=ensanut_2022_survey4, svymean, na.rm=T) %>%  
  mutate(prop=round(inability*100, digits=2), IC95=round((se*1.96)*100, digits=2)) %>%
  mutate(lIC95=prop-IC95,uIC95=prop+IC95, cluster="PACS") %>%
  dplyr::select(prop,IC95,lIC95,uIC95, cluster)

fig4a<-severe %>%
  mutate(duration=factor(rep(1:4,2), labels = c("<1 month","1-3 months", "3-6 months", ">6 months")),
         pasc=factor(c(rep(0,4), rep(1,4)), labels = c("<12 points", "≥12 points"))) %>%
  ggplot(aes(x=duration, y=prop, col=pasc))+
  geom_point(size=4,position = position_dodge(0.9))+
  geom_errorbar(aes(ymin = lIC95, ymax = uIC95), position = position_dodge(0.9), width = 0.25)+
  theme_bw()+
  ylab("Prevalence of incapacitating symptoms \n amongst patients with persistent sequelae (%)")+
  xlab("Additional symptom duration one month post-COVID-19")+labs(col="PASC score")+
  theme(legend.position = "top")+scale_color_aaas()

## Models 
m1<-glm(`Disabilitating symptoms` ~ `Omicron variant` + `Age (years)` + `Male sex` + Vaccinated + Reinfection + Diabetes+ Hypertension + `Depressive symptoms` + Setting + Education,data= model %>% filter(PASC==1),family=binomial(log),weights=swts, singular.ok = F, start = rep(-0.0001,17))
fig4b<-ggcoef_table(m1, exponentiate = TRUE, significance = NULL, table_text_size = 3.5, colour = NULL,)

fig4<-ggarrange(fig4a,"", fig4b, labels = c("A", "B",""), nrow=3, ncol=1, heights = c(0.48, 0.02,0.5))

ggsave(fig4,
       filename = "Figure4.jpg", 
       width = 22, 
       height = 25,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)
m1<-glm(`Disabilitating symptoms` ~ `Omicron variant` + `Age (years)` + `Male sex` + Vaccinated + Reinfection + Diabetes+ Hypertension + `Depressive symptoms` + Setting + Education + `PASC score`,data= model %>% filter(PASC==1),family=binomial(log),weights=swts, singular.ok = F, start = rep(-0.0001,18))
summ(m1, exp=T, confint=T)

#### Missing data ####
aggr_plot <- aggr(model, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(aggr), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

model2<-ensanut_2022_2 %>%
  dplyr::select(pacs, pacs2,omicron, variant,age,age_cat, sex, reinfection, vacuna, diabetes, CVE_ENT, swts, estrato.x, 
                DISLI_cat,fa0400,daily_smoking, daily_drinking, Hypertension, cesd, weight_loss, cvd,doses, h0317a, h0311,typeVaccine,when_vaccine, duration2, inability, h1205) %>%
  mutate('Predominant variant'=factor(variant, labels = c("Ancestral strain", "B.1.1.519", "Delta", "Omicron")))%>%
  rename(PASC=pacs, 'Omicron variant'=omicron, 'Age (years)'=age_cat, Reinfection=reinfection, Sleep=fa0400,'Number of reinfections'=h1205,
         Vaccinated =vacuna,'Vaccine timing' =when_vaccine,'Male sex'=sex, Diabetes=diabetes, Setting=estrato.x, DISLI=DISLI_cat, 'COVID-19 vaccine doses'=doses,
         `Daily smoking`=daily_smoking, Drinking=daily_drinking, 'CES-D'=cesd,'Weight loss'=weight_loss, CVD=cvd,Indigenous=h0311,'Vaccine type'=typeVaccine, 'Symptom duration'=duration2, 'Disabilitating symptoms'=inability)
age_cat2<-c("20-29", "30-39", "40-49", "50-59", "60-69", "≥70")
model2$`Age (years)`<-factor(model2$`Age (years)`, labels = age_cat2)
model2$Setting[model2$Setting==3]<-2
model2$Setting<-factor(model2$Setting, labels = c("Rural", "Urban"))
model2$Setting<-relevel(model2$Setting, ref="Urban")
model2$DISLI<-relevel(model2$DISLI, ref="Q1")
model2$`Depressive symptoms`<-ifelse((model2$age<=60 & model2$`CES-D`>=9)|(model2$age>60 & model2$`CES-D`>=5),1,0)
model2$Education[model2$h0317a %in% c(0,1,2)]<-1
model2$Education[model2$h0317a %in% c(3)]<-2
model2$Education[model2$h0317a %in% c(4,5,6,7,8)]<-3
model2$Education[model2$h0317a %in% c(9,10,11,12)]<-4
model2$Education<-factor(model2$Education, labels = c("Primary or less", "Secondary", "High school or technical", "University or higher"))
model2$`Booster vaccination`<-ifelse(model2$`COVID-19 vaccine doses`>2,1,0)
model2$`COVID-19 vaccine doses`<-factor(model2$`COVID-19 vaccine doses`)
model2$`PASC score`<-factor(model2$pacs2, labels = c("<12", "≥12"))

aggr_plot <- aggr(model2, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(aggr), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
