#Atlas: en ca

##Mapas: se han realizado en QGIS
#El mapa 1 emplea los datos del archivo "35_Voy a casa del maestro_CdBM_variante1.csv"
#El mapa 2 emplea los datos de los archivos "aleican_casa_variante1.csv", "ALECMan_enca_Varainte1.csv", 
#"ALEA_enca_variante1.csv", "ALCyL_enca.csv" y "ALECant_enca.csv"
#Los archivos acabados en "_coords" contienen las coordenadas necesarias para hacer los mapas

#librerías
library(tidyverse)
library(magrittr)
'%ni%' <- Negate('%in%')
library(lme4)
library(broom.mixed)
library(extrafont)
library(grDevices)
library(ggtext)

##Cargar datos

#datos enca
alecyl_enca <- read_delim("ALCyL_enca.csv", delim = ";")
alecant_enca <- read_delim("ALECant_enca.csv", delim = ";")
#Las tablas contienen todas las variantes producidas, incluso si hay varias en una misma localidad
ALEICan_todo <- read_delim("aleican_casa_todas_variantes.csv", delim = ";")
ALEA_todo <- read_delim("ALEA_enca_todas_variantes.csv", delim = ";")
ALECMan_todo <- read_delim("ALECMan_enca_todas_variantes.csv", delim = ";")
ALPI_todo <- read_delim("35_Voy a casa del maestro_CdBM_todas_variantes.csv", delim = ";")

#uniformar aleican
ALEICan_todo <- ALEICan_todo %>%
  separate(Nombre, into = c("Localidad", "Provincia", "Comunidad"), sep = ",") 


##Atlas septentrionales
#juntar ALEC y ALECyL
atlas_norte <- rbind(alecant_enca, alecyl_enca)

#Frecuencia de formas por atlas
atlas_norte %>%
  filter(!is.na(Variantes_casa)) %>%
  group_by(Atlas) %>%
  count(Variantes_casa) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1))

##Atlas meridionales
#Juntar los tres
atlas_sur <- rbind(ALEICan_todo, ALEA_todo, ALECMan_todo)

#Número total de formas y cuáles son
sort(unique(atlas_sur$Variantes))

#Frecuencia de formas en conjunto
atlas_sur_formas <- atlas_sur %>%
  filter(!is.na(Variantes)) %>%
  count(Variantes, sort = T) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1),
         end = " %)", beginning = " (",
         n2 = n, percentage2 = percentage) %>%
  unite(article, n2, beginning, percentage2, end, sep = "")

#Frecuencia de formas por atlas
atlas_sur_formas_atlas <- atlas_sur %>%
  filter(!is.na(Variantes)) %>%
  group_by(Atlas) %>%
  count(Variantes) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1),
         end = " %)", beginning = " (",
         n2 = n, percentage2 = percentage) %>%
  unite(article, n2, beginning, percentage2, end, sep = "") %>%
  ungroup()

#ALPI
#Frecuencia de la preposicion "de" por forma de "casa" (tabla 1)
ALPI_todo %>%
  filter(!is.na(Prep_de_1)) %>%
  group_by(Casa_1) %>%
  count(Prep_de_1) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1),
         end = " %)", beginning = " (",
         n2 = n, percentage2 = percentage) %>%
  unite(article, n2, beginning, percentage2, end, sep = "")

#Atlas meridionales
#Frecuencia de la preposicion "de" por forma de "casa" (tabla 2)
atlas_sur_prep_de <- atlas_sur %>%
  filter(!is.na(Preposicion_posterior)) %>%
  group_by(Casa) %>%
  count(Preposicion_posterior) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1),
         end = " %)", beginning = " (",
         n2 = n, percentage2 = percentage) %>%
  unite(article, n2, beginning, percentage2, end, sep = "")

#Frecuencia de "de" con la forma "casa" por atlas
atlas_sur %>%
  filter(Casa == "casa") %>%
  group_by(Atlas) %>%
  count(Preposicion_posterior) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1),
         end = " %)", beginning = " (",
         n2 = n, percentage2 = percentage) %>%
  unite(article, n2, beginning, percentage2, end, sep = "")

##Atlas meridionales y ALPI: glmer (tabla 3)

#Añadir el factor lengua a los atlas meridionales
atlas_sur_de <- atlas_sur %>%
  select(Atlas, ID, Provincia, Casa, Preposicion_posterior) %>%
  mutate(Lengua = "Central")

#Añadir el factor lengua y atlas al ALPI
#Considero los enclaves que emplean el asturiano como "Centrales" (no están en la frontera)
sort(unique(enca_alpi$Cod_ALPI)) 

enca_alpi_de <- enca_alpi %>%
  mutate(Lengua = ifelse(Cod_ALPI %in% c(221, 225, 755, 756, 759, 763, 764, 766:768, 771, 773, 776, 777, 786, 789), "Central",
                         ifelse(Cod_ALPI %in% c(100:151, 1121, 200:293, 300:303, 323, 324, 333, 340, 341, 360, 362, 365), "Galllego-portugués",
                                ifelse(Cod_ALPI %in% c(700:803, 612:614, 628, 629, 633, 634), "Catalán", "Central")))) %>%
  mutate(ID = Cod_ALPI, 
         Casa = Casa_1,
         Preposicion_posterior = Prep_de_1, Atlas = "ALPI")%>%
  select(Atlas, ID, Provincia, Casa, Preposicion_posterior, Lengua)

#Juntarlos
atlas_todo_de <- rbind(atlas_sur_de, enca_alpi_de)

#Filtros para eliminar variantes muy minoritarias
#Solo consideramos las tres formas más frecuentes de "casa"
#Elimino dos casos de "de" que no pueden interpretarse correctamente
atlas_todo_de_glm <- atlas_todo_de %>%
  filter(!is.na(Preposicion_posterior)) %>%
  filter(Casa %in% c("ca", "cas", "casa")) %>%
  filter(Preposicion_posterior %in% c("de", "articulo_pleno", "no")) %>%
  mutate(Preposicion_posterior = ifelse(Preposicion_posterior == "articulo_pleno", "no", Preposicion_posterior))

unique(atlas_todo_de_glm$Preposicion_posterior)

#Factorizar para poder hacer el glmer
atlas_todo_de_glm$Preposicion_posterior <- as.factor(atlas_todo_de_glm$Preposicion_posterior)
atlas_todo_de_glm$Preposicion_posterior <- factor(atlas_todo_de_glm$Preposicion_posterior, levels = c("no", "de"))
levels(atlas_todo_de_glm$Preposicion_posterior) #Con este relevelling predecimos la probabilidad de "de"
atlas_todo_de_glm$Lengua <- factor(atlas_todo_de_glm$Lengua, levels = c("Central", "Catalán", "Galllego-portugués"))
atlas_todo_de_glm$Atlas <- factor(atlas_todo_de_glm$Atlas, levels = c("ALPI", "ALECMan", "ALEICan", "ALEA"))

#Modelo
atlas_glm <- glmer(Preposicion_posterior ~ Casa + Lengua + (1|Atlas), family = "binomial", data = atlas_todo_de_glm)
summary(atlas_glm)
hist(resid(atlas_glm))#parece una distribución bastante normal
#Pasarlo a formato tidy
atlas_glm_tidy <- tidy(atlas_glm, exponentiate = T, conf.int = T) #statistic es el z-value
atlas_glm_tidy
write_delim(atlas_glm_tidy, "atlas_glm_tidy.csv", delim = "\t")

#Atlas meridionales
#Preposición anterior por forma de "casa" (figura 1)

atlas_sur_prep_anterior <- atlas_sur %>%
  filter(!is.na(Preposicion_anterior)) %>%
  filter(Preposicion_anterior != "no?") %>% #la quitamos, parece un error de transcripción
  group_by(Casa) %>%
  count(Preposicion_anterior) %>%
  mutate(total = sum(n), 
         percentage = round(n/total*100, 1),
         end = " %)", beginning = " (",
         n2 = n, percentage2 = percentage) %>%
  unite(article, n2, beginning, percentage2, end, sep = "") %>% 
  ungroup() 

#Seleccionar solo las tres formas más frecuentes del sustantivo
atlas_sur_prep_anterior_selec <- atlas_sur_prep_anterior %>%
  filter(Casa %in% c("ca", "cas", "casa")) %>%
  arrange(Casa, desc(percentage)) %>% #ordenar para mejorar la visibilidad del gráfico
  mutate(order = row_number()) 

#Crear el gráfico
ggplot(atlas_sur_prep_anterior_selec, aes(x = order, y = percentage)) + 
  labs(title="Preposición espacial por forma de *casa* en los atlas meridionales", x="Preposicion direccional", y="%") + 
  geom_bar(stat = 'identity', position = 'dodge', colour = "black",
           fill = c('darkgrey', 'gray98', 'dimgray', 'black', 'lightgray', 'black', 'gray98', 'darkgrey',
                    'gray98', 'darkgrey', 'black', 'dimgray')) +
  geom_text(aes(label=n), position=position_dodge(width=1), hjust=0.5, vjust=-0.3, 
            size = 3, inherit.aes = TRUE) +
  facet_wrap(~ Casa, scales = "free") + 
  scale_x_continuous(   # Add categories to axis
    breaks = atlas_sur_prep_anterior_selec$order,
    labels = atlas_sur_prep_anterior_selec$Preposicion_anterior,
    expand = c(0,0)
  ) + 
  theme_bw(base_family = "Times New Roman", base_size = 12) + 
  theme(axis.text = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_markdown()) #con esto ponemos las etiquetas en diagonal
ggsave("atlas_sur_prep_anterior_selec.png", width = 12, height = 6) #saves the last plot


