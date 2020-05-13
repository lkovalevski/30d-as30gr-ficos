library(tidyverse)
library(readxl)

# Leer los datos de defunciones 2018
url_defunciones  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2020/01/DefWeb18.csv"
dfDefuncionesRaw <- readr::read_csv(url_defunciones)

# 2009
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb16.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb15.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb14.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb13.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb12.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb11.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb09.csv
http://www.deis.msal.gov.ar/wp-content/uploads/2018/06/DefWeb10.csv

# Leer diccionario de variables
url_diccionario  <- "http://www.deis.msal.gov.ar/wp-content/uploads/2019/01/DescDef1.xlsx"
tmp              <-  tempfile(fileext = ".xlsx")
download.file(url = url_diccionario, destfile = tmp, mode = "wb")
diccionario      <- readxl::read_excel(tmp, sheet = "CODMUER", col_names = TRUE)

# Contar las causas de defunciones
df <- dfDefuncionesRaw %>% 
  dplyr::group_by(CAUSA) %>% 
  dplyr::summarise( n = sum(CUENTA)) %>% 
  merge(., diccionario, by.x = "CAUSA", by.y = "CODIGO", all.x = TRUE) %>% 
  dplyr::mutate(
    destacado = factor(1 * (n == max(n) )),
    etiquetas = stringr::str_wrap(VALOR, 35)
) %>% 
  arrange( - n) %>% 
  head(., 10)

var             = "etiquetas"
label_registros = "defunciones"
color_base      = "#f5b5b5" 
color_destacado = "#fc3f3f" 
titulo1         = "Top 10 Causas de defunciones en Argentina en 2018"
titulo2         = ""

# Grafico de barras
ggplot(df, aes(x = factor(get(var), levels = rev(df[, var])), y = n,
               fill = destacado)) +           
  geom_bar(stat = "identity") +               
  scale_fill_manual(values = c(color_base, color_destacado)) +
  guides(fill = FALSE)+ 
  scale_x_discrete(name = "Causa de muerte (CIE-10)") + 
  scale_y_continuous(name = paste0("Número de ", label_registros)) + 
  theme_bw() + 
  coord_flip() + 
  geom_hline(yintercept = 0, color = "grey", size = .5) +
  labs(title    = titulo1,
       caption  = paste0("Fuente: http://www.deis.msal.gov.ar/")) +
  annotate("text", x = df[10, var], y = 27000, label = "@leokova",
          hjust = 0, vjust = 1.5, col = "grey70", cex= 3,
         fontface = "bold", alpha = 0.5) +
  theme(panel.border     = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(),
        plot.title       = element_text(size = rel(1.5)),
        axis.text.y      = element_text(size = rel(1.3)),
        axis.text.x      = element_text(size = rel(1.4)),
        axis.title.x     = element_text(size = rel(1.2)),
        axis.title.y     = element_text(size = rel(1.2))
  )
