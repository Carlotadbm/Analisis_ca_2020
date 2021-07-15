#Carlota de Benito Moreno
#Análisis "ca": COSER
#Apartado 3.2


#librerías
library(tidyverse)
library(broom.mixed)
'%ni%' <- Negate('%in%')
library(lme4)

#El mapa 3 se ha realizado en QGIS a partir del archivo Forma_casa_COSER_12042020_coords.csv
  
coser <- read_delim("Enca_encasa_COSER_12042020_analisis.csv", delim = ";")

##Formas de "casa"
coser %>% 
  count(Forma)

##Preposición anterior por forma de "casa"
coser %>% 
  count(Forma, Preposicion_anterior)

##Preposición "de" (tabla 4)
coser %>%
  filter(!is.na(Preposicion_de)) %>%
  group_by(Forma) %>%
  count(Preposicion_de) %>% 
  mutate(Total = sum(n), Perc = round(n/Total*100,1))

##Preposición "de" y forma de "casa": glmer (tabla 5)
coser_prepde <- coser %>%
  filter(!is.na(Preposicion_de))
levels(factor(coser_prepde$Preposicion_de)) 

#valor de referencia: "no", se predice "si"
coser_glm <- glmer(factor(Preposicion_de) ~ Forma + (1 | COSERID), family = "binomial", data = coser_prepde)
summary(coser_glm)
range(resid(coser_glm)) 
hist(resid(coser_glm)) #parece una distribución bastante normal

coser_glm_tidy <- tidy(coser_glm, exponentiate = T, conf.int = T) 
coser_glm_tidy

##Forma de "casa" y preposición temporal (tabla 6)
coser_prepant <- coser %>%
  filter(Preposicion_anterior %in% c("en", "a", "an")) %>%
  filter(Verbo %in% c("movimiento", "estado")) %>%
  group_by(Verbo, Forma) %>%
  count(Preposicion_anterior) %>% 
  mutate(Total = sum(n), Perc = round(n/Total*100,1))

##Forma de "casa" y preposición temporal: glmer (tabla 7)
coser_prepant_glm <- coser %>%
  filter(Preposicion_anterior %in% c("en", "a")) %>%
  filter(Verbo %in% c("movimiento", "estado")) 
levels(factor(coser_prepant_glm$Preposicion_anterior)) #valor de referencia: a, se predice "en"
levels(factor(coser_prepant_glm$Verbo)) #valor de referencia: estado

coser_glm_prepant <- glmer(factor(Preposicion_anterior) ~ Forma*Verbo + (1 | COSERID), family = "binomial", data = coser_prepant_glm)
summary(coser_glm_prepant) 
range(resid(coser_glm_prepant)) 
hist(resid(coser_glm_prepant))#parece una distribución bastante normal

coser_glm_prepant_tidy1 <- tidy(coser_glm_prepant, exponentiate = T, conf.int = T) 
coser_glm_prepant_tidy1

coser_prepant_glm$Verbo <- factor(coser_prepant_glm$Verbo, levels = c("movimiento", "estado")) #relevelling para la segunda vez
coser_glm_prepant <- glmer(factor(Preposicion_anterior) ~ Forma*Verbo + (1 | COSERID), family = "binomial", data = coser_prepant_glm)
summary(coser_glm_prepant) 

coser_glm_prepant_tidy2 <- tidy(coser_glm_prepant, exponentiate = T, conf.int = T) 
coser_glm_prepant_tidy2

coser_glm_prepant_tidy <- rbind(coser_glm_prepant_tidy1, coser_glm_prepant_tidy2)
coser_glm_prepant_tidy

