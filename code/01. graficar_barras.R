library(tidyverse)

df <- data.frame(
  jugador           = c("JOSÉ SANFILIPPO", "LEOPOLDO LUQUE" , "DANIEL PASSARELLA", 
                        "LUIS ARTIME"    , "GONZALO HIGUAÍN", "DIEGO MARADONA", 
                        "HERNÁN CRESPO"  , "SERGIO AGÜERO"  , "GABRIEL BATISTUTA", 
                        "LIONEL MESSI"),
  cantidad_de_goles = c(21, 22, 22, 24, 32, 34, 35, 41, 54, 70)
) %>% arrange( - cantidad_de_goles)

var  = "jugador"
frec = "cantidad_de_goles"
color.grp = "#439430"

  ggplot(df, aes(x = factor(get(var), levels = rev(df[, var])), y = df[, frec])) + # Definir x e y, y re-ordenar los niveles
  geom_bar(stat = "identity", fill = color.grp) + #Indicar que es un Grafico de barras
  scale_fill_manual("") +
  scale_x_discrete(name = paste(var)) + # Nombre de la variable categorica
  scale_y_continuous(name = paste(frec)) + # Nombre de la variable respuesta
  theme_bw() + # Quitar el color de fondo
  coord_flip() + # Rotar el grafico
  #{if(! var %in% categoricas_ordinales)coord_flip()} + # Rotar el grafico
  geom_hline(yintercept = 0, color = "grey", size = .5) +
  theme(panel.border = element_blank(),  
        panel.grid.minor   = element_blank(), 
        panel.background   = element_blank(),
        axis.text.y        = element_text(size = rel(1.5)),
        axis.text.x        = element_text(size = rel(1.5)),
        axis.title.x       = element_text(size = rel(1.3)),
        axis.title.y       = element_text(size = rel(1.3))
  )
