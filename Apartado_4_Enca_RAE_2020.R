#Enca: corpus académicos
#1 de mayo de 2020

#librerías
library(tidyverse)
'%ni%' <- Negate('%in%')
library(extrafont)
library(grDevices)
library(ggtext)
library(ggExtra)
library(broom.mixed)
library(lme4)

enca_RAE <- read_delim("Corpus_RAE_20200501.csv", delim = ";")

##Número de casos totales
nrow(enca_RAE)

##Formas de "casa" a lo largo del tiempo, por tipo de observación (figura 2)
#Limpiar tabla
enca_RAE %>% 
  distinct(Forma_casa)
enca_RAE %>% 
  distinct(Tipo_observacion)
enca_RAE %>% 
  distinct(Indicacion_fecha)

enca_RAE_tipo_obs <- enca_RAE %>%
  filter(Indicacion_fecha %ni% c("rojo", "ambar")) %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  mutate(Tipo_observacion = ifelse(Tipo_observacion == "Refrán", "Oralidad popular",
                                   ifelse(Tipo_observacion == "ambiguo", "Ambiguo", 
                                          ifelse(Tipo_observacion == "Locución", "Mímesis oralidad popular",
                                                 ifelse(Tipo_observacion == "no marcado", "No marcado", Tipo_observacion)))))

#Crear gráfico
yheight_marcacion <- 75 #para el recorte el eje y, ver abajo
ggplot(enca_RAE_tipo_obs, aes(x=Fecha_procesada, fill=Tipo_observacion)) + 
  geom_dotplot(binwidth =20, binpositions="all", stackgroups = T, dotsize = 0.91) + 
  labs(title="Marcación diafásica de *ca* vs. *cas* en la historia (corpus académicos)", x="Año", fill = "Marcación diafásica", y = "Número de ocurrencias") + 
  facet_wrap(~Forma_casa) + 
  coord_fixed(ratio=18.2*yheight_marcacion) + #esto recorta el eje y
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0), breaks = seq(0, 10, 10/yheight_marcacion), labels=seq(0,yheight_marcacion)*10) + #esto hace que tenga significado
  scale_fill_manual(values=c('white', 'gold', 'cadetblue2', 'blue', 'black', 'darkcyan')) + 
  theme_bw(base_family = "Times New Roman", base_size = 12) + 
  theme(plot.title = element_markdown())

##Número de casos por autor
enca_RAE %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  filter(Tipo_observacion %ni% c("Refrán", "Oralidad popular")) %>% 
  count(Autor) %>%
  arrange(desc(n))

##Formas de "casa" a lo largo del tiempo y país, un ejemplo por autor (figura 3)


#Fijar la muestra aleatoria
set.seed(234) 
#Crear la tabla
enca_RAE_casa_autor <- enca_RAE %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  filter(Tipo_observacion %ni% c("Refrán", "Oralidad popular")) %>% 
  group_by(Autor, Forma_casa) %>%
  sample_n(1)

#Crear el gráfico
yheight_autor <- 14 #para el recorte el eje y, ver abajo
ggplot(enca_RAE_casa_autor, aes(x=Fecha_procesada, fill= Pais)) + 
  geom_dotplot(binwidth =20, binpositions="all", stackgroups = T, dotsize = 1.3, show.legend = T) + 
  labs(title="*Ca* vs. *cas* en la historia (corpus académicos): un ejemplo por autor", x="Año", y = "Número de ocurrencias", fill = "País") + 
  facet_wrap(~Forma_casa) + 
  coord_fixed(ratio=26.2*yheight_autor) + #esto recorta el eje y
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0), breaks = seq(0, 5, 5/yheight_autor), labels=seq(0,yheight_autor)*5) + #esto hace que tenga significado
  scale_fill_manual(values = c("cadetblue2", "gold", "white", "black", "darkcyan", "blue")) + 
  theme_bw(base_family = "Times New Roman", base_size = 12) + 
  theme(plot.title = element_markdown())

## Forma de "casa" y uso de la preposición posterior
#tabla 8
enca_RAE %>% 
  distinct(Prep_posterior)
enca_RAE %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  filter(Prep_posterior %in% c("no", "de")) %>% #excluyo "sin_complemento", "posesivo", "no, e" 
  group_by(Forma_casa) %>%
  count(Prep_posterior) %>% 
  mutate(Total = sum(n), Perc = round(n/Total*100,1))

#Glmer (tabla 9)
enca_RAE_prep_post <- enca_RAE %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  filter(Prep_posterior %in% c("no", "de")) #excluyo "sin_complemento", "posesivo", "no, e" 
enca_RAE_prep_post$Prep_posterior <- factor(enca_RAE_prep_post$Prep_posterior, levels = c("no", "de")) #Relevelling para predecir la probabilidad de "de"

crae_de_glm <- glmer(factor(Prep_posterior) ~ Forma_casa + (1 | Autor), family = "binomial", data = enca_RAE_prep_post)
summary(crae_de_glm)
range(resid(crae_de_glm))
hist(resid(crae_de_glm)) #parece una distribución bastante normal

#Ponerlo en formato tidy
crae_de_glm_tidy <- tidy(crae_de_glm, exponentiate = T, conf.int = T) #statistic es el z-value
crae_de_glm_tidy


## Forma de "casa" y uso de la preposición posterior (tabla 10)
enca_RAE %>% 
  distinct(Prep_anterior)
enca_RAE %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  mutate(Prep_anterior = ifelse(Prep_anterior == "no?", "no", Prep_anterior)) %>%
  filter(Prep_anterior %in% c("en", "a", "an", "no")) %>%
  filter(Prep_posterior %in% c("no", "de")) %>% #excluyo "sin_complemento", "posesivo", "no, e" 
  filter(Verbo %in% c("estado", "movimiento")) %>%
  group_by(Verbo, Forma_casa) %>%
  count(Prep_anterior) %>% 
  mutate(Total = sum(n), Perc = round(n/Total*100,1))

#Casos de "no" por autor:
enca_RAE %>%
  mutate(Forma_casa = ifelse(Forma_casa == "cas(a)", "cas", Forma_casa)) %>%
  mutate(Prep_anterior = ifelse(Prep_anterior == "no?", "no", Prep_anterior)) %>%
  filter(Prep_anterior %in% c("en", "a", "an", "no")) %>%
  filter(Prep_posterior %in% c("no", "de")) %>% #excluyo "sin_complemento", "posesivo", "no, e" 
  filter(Verbo %in% c("estado", "movimiento")) %>%
  filter(Prep_anterior == "no") %>%
  filter(Forma_casa == "ca") %>%
  count(Autor) %>%
  mutate(Total = sum(n))

