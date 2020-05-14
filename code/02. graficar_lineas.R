#' ----
#' Desafío #30díasdegráficos con R
#' ===============================
#' 
#' Día 2 - Gráficos de líneas
#' Evolución de las 10 causas de mortalidad más frecuentes en Argentina 2009-2018
#' @leokova - 13/05/2020
#' ---

library(tidyverse)
library(readxl)
library(here)
library(RColorBrewer)

# Leer los datos de defunciones 2018 - 2009
url_defunciones_2018  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2020/01/DefWeb18.csv"
url_defunciones_2017  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2019/01/DefWeb17.csv"
url_defunciones_2016  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb16.csv"
url_defunciones_2015  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb15.csv"
url_defunciones_2014  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb14.csv"
url_defunciones_2013  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb13.csv"
url_defunciones_2012  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb12.csv"
url_defunciones_2011  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb11.csv"
url_defunciones_2010  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb10.csv"
url_defunciones_2009  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb09.csv"

# Construir el dataset del todo el período
dfDefuncionesPeriodo <- data.frame()
for(anio in c(2009 : 2018)) {
  dfDefuncionesRaw <- readr::read_csv(get(paste0("url_defunciones_", anio))) %>% 
    mutate(anio = anio)
  dfDefuncionesPeriodo <- rbind(dfDefuncionesPeriodo, dfDefuncionesRaw)
  rm(dfDefuncionesRaw)
}

# Leer diccionario de variables
url_diccionario  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2019/01/DescDef1.xlsx"
tmp              <-  tempfile(fileext = ".xlsx")
download.file(url = url_diccionario, destfile = tmp, mode = "wb")
diccionario      <- readxl::read_excel(tmp, sheet = "CODMUER", col_names = TRUE)

# Top 10 causas del periodo
dfTop10causas <- dfDefuncionesPeriodo %>% 
  dplyr::group_by(CAUSA) %>% 
  dplyr::summarise( n = sum(CUENTA)) %>% 
  arrange( - n) %>% 
  head(., 10)

# Frecuencia causas por anio del Top 10 causas del periodo
df <- dfDefuncionesPeriodo %>% 
  dplyr::filter(CAUSA %in% dfTop10causas$CAUSA) %>% 
  dplyr::mutate(CAUSA = factor(CAUSA, levels = dfTop10causas$CAUSA)) %>% 
  dplyr::group_by(anio, CAUSA) %>% 
  dplyr::summarise( n  = sum(CUENTA))  %>%
  merge(., diccionario, by.x = "CAUSA", by.y = "CODIGO", all.x = TRUE) %>% 
  dplyr::group_by(CAUSA) %>% 
  mutate(sd = sd(n),
         cv = 100 * sd(n) / mean(n),
         etiquetas = stringr::str_wrap(VALOR, 35))

# Ordenar los niveles de las etiquetas
orden_etiquetas <- df %>% dplyr::filter(anio == 2018) %>% arrange( - n)
df <- df %>% 
  dplyr::mutate(etiquetas = 
                  factor(etiquetas, levels = orden_etiquetas$etiquetas) )

# Inputs para el gráfico
var             = "etiquetas"
tiempo          = "anio"
label_registros = "defunciones"
color_base      = "#f5b5b5" 
color_destacado = "#fc3f3f" 
titulo1         = "Evolución del número de defunciones de las 10 causas de mortalidad más frecuentes"
titulo2         = "en Argentina en el período 2009-2018"

# Grafico de líneas
ggplot(df, aes(x = get(tiempo), y = n, group = etiquetas)) +
  geom_line( aes(color = etiquetas), stat = "identity", size = rel(1.4)) + 
  #scale_color_brewer(palette = ggthemes::Red, direction = -1) +
  scale_color_manual(values = c(rev(brewer.pal(9, "Reds")), "#FFF5F0")) + 
  #  scale_fill_manual("") +
  scale_x_continuous(name = "Año") + 
  scale_y_continuous(name = paste0("Número de ", label_registros)) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "grey", size = .5) +
  labs(title    = titulo1,
       subtitle = titulo2,
       caption  = paste0("Fuente: http://www.deis.msal.gov.ar/ \n
                          Realizado por @leokova, 13 May 2020"))  +
  theme(
    panel.border     = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_blank(),
    plot.title       = element_text(size = rel(1.5)),
    plot.subtitle    = element_text(size = rel(1.3)),
    axis.text.y      = element_text(size = rel(1.3)),
    axis.text.x      = element_text(size = rel(1.4)),
    axis.title.x     = element_text(size = rel(1.2)),
    axis.title.y     = element_text(size = rel(1.2)),
    legend.title     = element_blank()
  )
