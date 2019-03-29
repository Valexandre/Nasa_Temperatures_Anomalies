#install.packages(c("rworldmap","geosphere","gpclib"))
library(rworldmap)
library(dplyr)
library(ggplot2)
library(geosphere)
library(gpclib)

# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group)) +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45)

worldmap

worldmap <- ggplot() + 
  geom_polygon(data = world.df, aes(x = long, y = lat, group = group), colour="white") +
  scale_y_continuous(breaks = (-2:2) * 30) +
  scale_x_continuous(breaks = (-4:4) * 45) +
  coord_map("ortho", orientation=c(61, 90, 0))
worldmap

temp <- read.table("https://data.giss.nasa.gov/tmp/gistemp/NMAPS/tmp_GHCNv4_ERSSTv5__1200km_Anom2_2019_2019_1951_1980_100__180_90_0__2_/amaps.txt", skip=1, header = TRUE, na.strings="9999.0000")

breaks <- c(-9 -5, -3, -1, -0.5, 0, 0.5, 1, 3, 5, 9, 13)
lesCouleurs <- cbind(
  val = levels(cut(range(breaks), breaks = breaks)),
  col = c("#8600FF", "#3F94FE", "#77CAFD", "#99EEFF", "#D9FFD9", 
          "#FFFF4C", "#FFCC00", "#FF7E00", "#FF0000","#BE0000", "#7E0000")
)
lesCouleurs <- data.frame(lesCouleurs, stringsAsFactors = FALSE)
colnames(lesCouleurs) <- list("val", "col")
# Colours in 8 digits, the last two digits are for transparency
lesCouleurs$col <- paste(lesCouleurs$col,"FF", sep = "")

temp$`Différence de température` <- cut(temp$array.i.j, breaks = breaks)

rotate_map <- function(angle = -60){
  ggplot() + 
    geom_tile(data = temp, aes(x = lon, y = lat, fill = `Différence de température`), alpha = 0.8) +
    scale_fill_manual("Différence de température", breaks = lesCouleurs$val, values = lesCouleurs$col) +
    geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
    coord_map("ortho", orientation=c(61, angle, 0))+
    theme_void()+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    labs(title="  Différence de températures entre le mois de février 2019
  et la moyenne des mois de février entre 1951 et 1980",
         caption="Source: Nasa  
     Crédits: @LeParisienData grâce à @3wen  ")+
    theme(legend.position = "bottom", text=element_text(size = 12, family="Graphik"))
}

rotate_map(1)

jpeg(filename = " Différence de températures entre le mois de février 2019.jpg", width=1200, height = 900, quality=100, units = "px",type="cairo")
ggplot() + 
    geom_tile(data = temp, aes(x = lon, y = lat, fill = `Différence de température`), alpha = 0.8) +
    scale_fill_manual("Différence de température", breaks = lesCouleurs$val, values = lesCouleurs$col) +
    geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
    coord_map("ortho", orientation=c(61, -75, 0))+
    theme_void()+
    guides(fill=guide_legend(nrow=2,byrow=TRUE))+
    labs(title="  Différence de températures entre le mois de février 2019
         et la moyenne des mois de février entre 1951 et 1980",
         caption="Source: Nasa  
         Crédits: @LeParisienData grâce à @3wen  ")+
    theme(legend.position = "bottom", text=element_text(size = 22, family="Graphik"))
dev.off()
  
jpeg(filename = "Différence de températures entre le mois de février 20192.jpg", width=1200, height = 1000, quality=100, units = "px",type="cairo")
ggplot() + 
     geom_tile(data = temp, aes(x = lon, y = lat, fill = `Différence de température`), alpha = 0.8) +
     scale_fill_manual("Différence de température", breaks = lesCouleurs$val, values = lesCouleurs$col) +
     geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
     coord_map("ortho", orientation=c(60, 2, 0))+
     theme_void()+
     guides(fill=guide_legend(nrow=2,byrow=TRUE))+
     labs(title="Différence de températures entre le mois de février 2019
 et la moyenne des mois de février entre 1951 et 1980",
                   caption="Source: Nasa  
          Crédits: @LeParisienData grâce à @3wen  ")+
   theme(legend.position = "bottom", text=element_text(size = 22, family="Graphik"))
dev.off()


jpeg(filename = "Différence de températures entre le mois de février 20193.jpg", width=1200, height = 1000, quality=100, units = "px",type="cairo")
ggplot() + 
  geom_tile(data = temp, aes(x = lon, y = lat, fill = `Différence de température`), alpha = 0.8) +
  scale_fill_manual("Différence de température", breaks = lesCouleurs$val, values = lesCouleurs$col) +
  geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
  coord_map("ortho", orientation=c(60, 2, 0))+
  theme_void()+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(title="Différence de températures entre le mois de février 2019
et la moyenne des mois de février entre 1951 et 1980",
       caption="Source: Nasa  
         Crédits: @LeParisienData grâce à @3wen  ")+
  theme(legend.position = "bottom", text=element_text(size = 22, family="Graphik"))
dev.off()


jpeg(filename = "Différence de températures entre le mois de février 20194.jpg", width=1200, height = 1000, quality=100, units = "px",type="cairo")
ggplot() + 
  geom_tile(data = temp, aes(x = lon, y = lat, fill = `Différence de température`), alpha = 0.8) +
  scale_fill_manual("Différence de température", breaks = lesCouleurs$val, values = lesCouleurs$col) +
  geom_path(data = world.df, aes(x = long, y = lat, group = group)) +
  coord_map("ortho", orientation=c(-30, 74, 0))+
  theme_void()+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  labs(title="Différence de températures entre le mois de février 2019
et la moyenne des mois de février entre 1951 et 1980",
       caption="Source: Nasa  
         Crédits: @LeParisienData grâce à @3wen  ")+
  theme(legend.position = "bottom", text=element_text(size = 22, family="Graphik"))
dev.off()
