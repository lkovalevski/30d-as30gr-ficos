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
  cat(paste0("\n --- Anio ", anio, "\nEl df leído tiene ", nrow(dfDefuncionesRaw),
             "filas y ", nrow(dfDefuncionesRaw), " columnas.\n"))
  dfDefuncionesPeriodo       <- rbind(dfDefuncionesPeriodo, dfDefuncionesRaw)
  rm(dfDefuncionesRaw)
}

# Guardar el dataset en un .rds
# saveRDS(dfDefuncionesPeriodo, 
#        file = here::here("data", "01 raw", "defunciones2009-2018.rds"))

# Leer diccionario de variables
url_diccionario  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2019/01/DescDef1.xlsx"
tmp              <-  tempfile(fileext = ".xlsx")
download.file(url = url_diccionario, destfile = tmp, mode = "wb")
diccionario      <- readxl::read_excel(tmp, sheet = "CODMUER", col_names = TRUE)

# saveRDS(diccionario, 
#        file = here::here("data", "01 raw", "diccionario.rds"))


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
  merge(., diccionario, by.x = "CAUSA", by.y = "CODIGO", all.x = TRUE) 


#   dplyr::mutate(
#     destacado = factor(1 * (n == max(n) )),
#     etiquetas = stringr::str_wrap(VALOR, 35)
# )


var             = "etiquetas"
tiempo          = "anio"
label_registros = "defunciones"
color_base      = "#f5b5b5" 
color_destacado = "#fc3f3f" 
titulo1         = "Evolución de las 10 causas de mortalidad más frecuentes"
titulo2         = "Argentina 2009-2018"

degra <-colorRampPalette(c('lightgreen','lightblue'))(num.vars)

# Grafico de líneas
ggplot(df, aes(x = get(tiempo), y = n, group = CAUSA)) +
  geom_line( aes(color = CAUSA), stat = "identity", size = rel(1.4)) + 
  #  scale_fill_manual("") +
  #scale_x_discrete(name = "Año") + 
  scale_y_continuous(name = paste0("Número de ", label_registros)) + 
  theme_bw() +
  geom_hline(yintercept = 0, color = "grey", size = .5) +
  labs(title    = titulo1,
       caption  = paste0("Fuente: http://www.deis.msal.gov.ar/ \n
                          Realizado por @leokova, 13 May 2020"))  +
  theme(panel.border     = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title       = element_text(size = rel(1.5)),
        axis.text.y      = element_text(size = rel(1.3)),
        axis.text.x      = element_text(size = rel(1.4)),
        axis.title.x     = element_text(size = rel(1.2)),
        axis.title.y     = element_text(size = rel(1.2))
  )


ggplot(df, aes(x = get(tiempo), y = n, group = CAUSA)) +
  geom_line( aes(color = CAUSA), stat = "identity", size = rel(1.4)) #+"#f0c836") +
scale_fill_manual("") +
  scale_y_continuous(name = "Número de creditos otorgados") + 
  theme_bw() +   # Quitar el color de fondo
  geom_hline(yintercept = 0, color = "grey", size = .5) +
  theme(panel.border = element_blank(),  
        panel.grid.minor   = element_blank(), 
        panel.background   = element_blank(),
        axis.text.y        = element_text(size = rel(1.5)),
        axis.text.x        = element_text(size = rel(1.5)),
        axis.title.x       = element_text(size = rel(1.3)),
        axis.title.y       = element_text(size = rel(1.3))
  )


