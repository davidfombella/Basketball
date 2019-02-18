
# https://github.com/solmos/rfeb
# install.packages("devtools")
# devtools::install_github("solmos/rfeb")

# Los identificadores de los partidos en este caso van desde el 2010208 al 2010216.
# Usamos extract_shots() para obtener los datos recogidos de los tiros que se hicieron en estos partidos.
# Obtenemos un data frame en el que cada fila representa un tiro, de los cuales se muestran las siguientes variables:
  
# t = Tiempo desde el comienzo del partido
# x, y = Coordenadas del tiro
# team = Equipo del jugador que realizó el tiro
# player = Dorsal del jugador que realizó el tiro
# quarter = Cuarto en el que se realizó el tiro
# game = Identificador del partido
# made = Tiro fallado (Missed) o metido (Made)

library(rfeb)

game_ids <- 2010208:2010216

shots <- extract_shots(game_ids)

head(shots)

# Una vez tenemos los datos de los tiros, usamos la función plot_shotchart() para visualizar el gráfico de tiro.
# La función usa ggplot2 para producir el gráfico y es posible especificar los argumentos que queremos para el mapping de la función geom_point().
# Por ejemplo, podemos elegir el color de los puntos según fueron o no canasta.

# A continuación muestro todos los tiros de la jornada en un sólo gráfico:
  
plot_shotchart(shots, color = made)

# Seguramente no querramos mostrar todos los tiros. 
# Es posible usar otras funciones de ggplot2 para adaptar el gráfico a nuestras necesidades. 
# Por ejemplo, podemos usar facet_wrap() para obtener una matriz de gráficos según el partido:
  
library(ggplot2)

plot_shotchart(shots, color = made) + facet_wrap(~ game)

# Por último, utilizaremos dplyr para filtrar los tiros que queremos mostrar.
# Digamos que quiero ver qué tiros hice yo en el partido que disputé contra Palencia en esta jornada.
# Este partido se corresponde con el identificador 2010215 y el número que llevo en mi camiseta es el 41:
  
library(dplyr)

shots %>% 
  filter(game == 2010215 & team == "COVIRAN GRANADA" & player == "41") %>%
  plot_shotchart(color = made) +
  ggtitle("Sergio Olmos vs. Palencia")

#rfeb todavía está en proceso de desarrollo. 
# Mi idea es ampliar la funcionalidad de este paquete para poder analizar no solo datos de tiro, sino también datos jugada-a-jugada. Escribiré más entradas en el blog a medida que vaya añadiendo más funciones al paquete.








shots %>% 
  filter(team == "LIBERBANK OVIEDO BALONCESTO" ) %>%
  plot_shotchart(color = made) +
  geom_point(aes(size = 2))+
  ggtitle("LIBERBANK OVIEDO BALONCESTO")








# funcion redefinida
#' Plot shot charts
#'
#' @param df data frame with at least two coordinate columns named x and y
#' @param ... arguments specified to geom_point(aes(x, y, ...))
#'
#' @import ggplot2
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' library(ggplot2)
#' ids <- 2010208:2010216
#' week17_2018 <- extract_shots(ids)
#' plot_shotchart(week17_2018)
#' plot_shotchart(week17_2018, color = made) + facet_wrap(~game)
#' 

plot_shotchart2 <- function(df, ...) {
  
  court <- construct_court()
  
  plt <- ggplot(environment = environment()) +
    geom_point(data = df, aes(x, y, ...),size=2.5, alpha = 0.9) +
    geom_path(aes(x, y), size = 0.2, data = court$outer_lines) +
    geom_path(aes(x, y), size = 0.2, data = court$paint) +
    geom_path(aes(x, y), size = 0.2, data = court$ft_circle) +
    geom_path(aes(x, y), size = 0.2, data = court$arc3) +
    coord_fixed() +
    theme_void() +
    theme(panel.background = element_rect(fill = "whitesmoke"),
          legend.position = "bottom") +
    labs(color = "")
  plt
}


################################
oviedo <- shots %>% 
  filter(team == "LIBERBANK OVIEDO BALONCESTO") 


unique(oviedo$game)
 # http://baloncestoenvivo.feb.es/Game/2010212




jug_oviedo <-read.csv('oviedo_players.csv')

oviedo <- merge(x = oviedo , y = jug_oviedo, by ="player" , all.x = TRUE)


oviedo %>%
  plot_shotchart2(color = made) +
  ggtitle("LIBERBANK OVIEDO BALONCESTO MATCH 2010212")+
  facet_wrap(~ Playername)




# LIBERBANK OVIEDO BALONCESTO 	Pts 	REB 	AS 	FP 	VAL
# 6 	J. PUERTO 	                0 	 0 	0 	0 	0
# 7 	J. VAN ZEGEREN 	            6 	 6 	0 	4 	8
# 8 	R. AHONEN 	                0 	 0 	0 	0 	0
# 9 	V. PEREZ 	                  6 	 0 	0 	1 	4
# 12 	O. ARTEAGA 	               16 	 3 	0 	0 	19
# 13 	R. JAKSTAS 	               22 	 7 	2 	2 	29
# 14 	D. GEKS 	                  7 	 6 	2 	4 	6
# 15 	I. ROSA                    	7 	 4 	1 	2 	8
# 16 	A. MEANA 	                  3 	 1 	5 	2 	5
# 17 	A. BOUZAN 	                0 	 0 	0 	0 	-1
# 18 	S. LLORENTE 	             11 	 4 	10 	1 	18







