#Enca: CORDIAM, CODEA y CHARTA
#13 de mayo de 2020
library(tidyverse)
'%ni%' <- Negate('%in%')
library(extrafont)
library(grDevices)
library(ggtext)
library(ggExtra)


##Cas en CORDAIM
CORDIAM <- read_delim("CORDIAM_todo_042020.csv", delim = ";")
#Limpiar tabla
CORDIAM_cas <- CORDIAM %>%
  filter(Casa == "cas") %>%
  filter(Articulo_enca %ni% c("Ambiguo", "Error?"))

#Ver franja temporal
sort(CORDIAM_cas$Fecha_procesada)

#Ver tipos textuales por tipo
CORDIAM_cas %>%
  count(Pais_actual, Tipo_textual)

#Gráfico de "cas" por país, año y tipo textual (Figura 5) 
yheight <- max(count(CORDIAM_cas, Fecha_procesada)["n"]) 
ggplot(CORDIAM_cas, aes(x=Fecha_procesada, fill=Tipo_textual)) + 
  geom_dotplot(binwidth =20, binpositions="all", stackgroups = T, dotsize = 0.5) + 
  labs(title="*Cas* en la historia del español americano (CORDIAM)", x="Año", fill = "Tipo textual", y = "Número de ocurrencias") + 
  coord_fixed(ratio=20*yheight) + #esto recorta el eje y
  scale_y_continuous(limits=c(0, 1), expand = c(0, 0), breaks = seq(0, 5, 5/yheight), labels=seq(0,yheight)*10) + 
  scale_fill_manual(values=c('darkgoldenrod1', 'white', 'darkcyan', 'darkgrey', 'black', 'red')) + 
  theme_bw(base_family = "Times New Roman", base_size = 12) + 
  theme(plot.title = element_markdown())


## CORDIAM + CODEA + CHARTA)

CODEA <- read_delim("CODEA_casa_cas_ca.csv", delim = ";")
CHARTA <- read_delim("CHARTA_casa_ca.csv", delim = ";")

#Uniformar las tablas
CORDIAM <- CORDIAM %>%
  select(Corpus, EjemploID, Búsqueda, DOCUMENTO, Fecha_procesada, Articulo_enca, Prep_anterior, Casa, Prep_posterior, Verbo)
CODEA <- CODEA %>%
  select(Corpus, EjemploID, Búsqueda, DOCUMENTO, Fecha_procesada, Articulo_enca, Prep_anterior, Casa, Prep_posterior, Verbo)
CHARTA <- CHARTA %>%
  select(Corpus, EjemploID, Búsqueda, DOCUMENTO, Fecha_procesada, Articulo_enca, Prep_anterior, Casa, Prep_posterior, Verbo)
#Unirlas
Corp_docs <- rbind(CORDIAM, CODEA, CHARTA)

#Limpiar la tabla
#Solo mantenemos los válidos y los de Otra_preposicion (es decir: prep/no + casa + de/no SN)
Corp_docs_valido <- Corp_docs %>%
  filter(Articulo_enca %in% c("Válida", "Otra_preposicion")) %>% #ejemplos válidos
  filter(Casa == "casa") %>% #casa
  filter(!is.na(Fecha_procesada)) %>%
  mutate(Siglo = ifelse(Fecha_procesada < 1251, "XIII (1/2)",
                        ifelse(Fecha_procesada < 1301, "XIII (2/2)", 
                               ifelse(Fecha_procesada < 1351, "XIV (1/2)",
                                      ifelse(Fecha_procesada < 1401, "XIV (2/2)",
                                             ifelse(Fecha_procesada < 1451, "XV (1/2)",
                                                    ifelse(Fecha_procesada < 1501, "XV (2/2)",
                                                           ifelse(Fecha_procesada < 1551, "XVI (1/2)",
                                                                  ifelse(Fecha_procesada < 1601, "XVI (2/2)",
                                                                         ifelse(Fecha_procesada < 1651, "XVII (1/2)",
                                                                                ifelse(Fecha_procesada < 1701, "XVII (2/2)",
                                                                                       ifelse(Fecha_procesada < 1751, "XVIII (1/2)",
                                                                                              ifelse(Fecha_procesada < 1801, "XVIII (2/2)",
                                                                                                     ifelse(Fecha_procesada < 1851, "XIX (1/2)", "XIX (2/2)"
                                                                  )))))))))))))) %>%
  mutate(Prep_anterior_proc = ifelse(Prep_anterior %in% c("en", "a", "enfrente de", "de", "hacia", "por", "junto a", "debajo de", "desde", "para"), "una preposición", 
                                     ifelse(Prep_anterior == "no", "sin preposición", "preposición doble (prep. + *en*)"))) %>%
  mutate(Prep_posterior = ifelse(Prep_posterior == "no", "sin preposición", Prep_posterior))


#Preposición anterior: ¿cuántas?
Corp_docs_valido %>%
  count(Prep_anterior_proc) %>%
  mutate(Total = sum(n), Perc = round(n/Total*100, 1))
#Preposición anterior: ¿cuántas?, por siglo y corpus
Corp_docs_valido %>%
  group_by(Corpus, Siglo) %>%
  count(Prep_anterior_proc) %>% 
  View()

#Preposición posterior: ¿aparece?
Corp_docs_valido %>%
  count(Prep_posterior) %>%
  mutate(Total = sum(n), Perc = round(n/Total*100, 1))
#Preposición posterior: ¿aparece?, por siglo y corpus
Corp_docs_valido_prep_post <- Corp_docs_valido %>%
  group_by(Corpus, Siglo) %>%
  count(Prep_posterior) %>% 
  View()

#Preposición anterior por tipo de verbo
Corp_docs %>% 
  distinct(Articulo_enca)
#Nueva tabla
Corp_docs_verbo <- Corp_docs %>%
  filter(Articulo_enca %in% c("Válida", "Sin término", "Incluible")) %>% 
  filter(Verbo %in% c("movimiento", "estado")) %>%
  filter(Prep_anterior %in% c("a", "en")) %>%
  filter(!is.na(Fecha_procesada)) %>%
  mutate(Siglo = ifelse(Fecha_procesada < 1251, "XIII (1/2)",
                        ifelse(Fecha_procesada < 1301, "XIII (2/2)", 
                               ifelse(Fecha_procesada < 1351, "XIV (1/2)",
                                      ifelse(Fecha_procesada < 1401, "XIV (2/2)",
                                             ifelse(Fecha_procesada < 1451, "XV (1/2)",
                                                    ifelse(Fecha_procesada < 1501, "XV (2/2)",
                                                           ifelse(Fecha_procesada < 1551, "XVI (1/2)",
                                                                  ifelse(Fecha_procesada < 1601, "XVI (2/2)",
                                                                         ifelse(Fecha_procesada < 1651, "XVII (1/2)",
                                                                                ifelse(Fecha_procesada < 1701, "XVII (2/2)",
                                                                                       ifelse(Fecha_procesada < 1751, "XVIII (1/2)",
                                                                                              ifelse(Fecha_procesada < 1801, "XVIII (2/2)",
                                                                                                     ifelse(Fecha_procesada < 1851, "XIX (1/2)", "XIX (2/2)"
                                                                                                     ))))))))))))))

Corp_docs_verbo %>%
  group_by(Verbo) %>% 
  count(Prep_anterior) %>%
  mutate(Total = sum(n), Perc = round(n/Total*100, 1))

Corp_docs_verbo %>%
  group_by(Verbo, Corpus, Siglo) %>%
  count(Prep_anterior) %>% 
  View()

