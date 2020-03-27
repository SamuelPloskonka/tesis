############################################################################

# Tesis de licenciatura
# Samuel Pastor
# mail: samupastor27@gmail.com
# cell: (+591 78752513)

# ANALISIS EXPLORATORIO DE DATOS DE 35 PODOCARPUS 
############################################################################
# TO DO #
############################################################################


# 0 Subir datos

# 1 Analisis exploratorio
# 1.1 Analisis de correlacion
# 1.2 Regresiones lineales

# 2 Datos climáticos
# 2.1 WorldClim
# 2.2 MODIStolls
# 
# 3.



############################################################################
# 0. Cargar paquetes
############################################################################

# Definir directorio de trabajo
# En Mac (RStudio): session - Set Working Directory
# setwd("~/Desktop/TESIS_c/scriptsPODS/DIRECTORIO")

# TUTORIAL TIDYVERSE DE PAUL
# https://pftc5intrototidyverse.netlify.com/

 #    install.packages("devtools")
 #    install.packages("janitor")
 #    install.packages("tidyverse")
library(devtools) 
library(tidyverse)
library(conflicted)
 library(dplyr)
 library(janitor)
 library(broom)

 #definir preferencias de conflicted por dplyr
 conflict_prefer("select", "dplyr")
 conflict_prefer("extract", "dplyr")
 conflict_prefer("filter", "dplyr")
 #  raster::extract()
 # 0.1 Subir datos
 ############################################################################

 basecompleta <- read.table(file="base_completa_03.03.20.csv", header=TRUE, sep=",") %>% as_tibble()
# basecompleta %>% clean_names()
# basecompleta %>% dim()
basecompleta %>% names()
basecompleta %>% as_tibble()
#quitar valores individuales de rasgos funcionales 
podocarps <- basecompleta [, -c(12:16,18:22,24:28,30:34,36,37)] %>% 
  as_tibble()

# crear tibble con variables de interes
# 
podocarps <- basecompleta %>% select(-c(e1:e5,a1:a5,p1:p5,s1:s5,hojas_peso, extracto_peso ))
colnames(podocarps)
podocarps %>% filter(18)

podocarps %>% 
  summarise(min(sla, na.rm = TRUE), max(sla, na.rm = TRUE))

podocarps <- podocarps %>% 
  dplyr::filter(sla < 100)





head(podocarps)
summary(podocarps)
names(podocarps)

############################################################################
# 1 Analisis exploratorio
############################################################################


# PAQUETE DEL PAUL

#    library(esquisse)
#    esquisser(podocarps)


library(ggplot2)

ggplot(podocarps) +
 aes(x = elevacion, y = uv1, colour = exposicion, size = sla) +
 geom_point() +
 scale_color_hue() +
 theme_minimal()
# 1.1 regresion lineal de elevacion y uv

lm1 <- lm(uv1 ~ elevacion, data=podocarps)
plot(podocarps$elevacion, podocarps$uv1)
abline(lm1)
# lm1 %>% tidy()


# 1.2 GRAFICO DE DISTRIBUCION DE elevacion y uv1
  # 1.2.1 grafico 1
podocarps %>% 
  ggplot(aes(elevacion, uv1)) +
  geom_point()


# 1.2.2 grafico 2
podocarps %>% 
  ggplot(aes(elevacion, uv1)) +
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() 

# 1.2.3 grafico 3
podocarps %>% 
  ggplot(aes(elevacion, uv1)) +
  geom_point()+
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_y_continuous(breaks = seq(0, 3, .2))




############################################################################
# GRAFICO DE DISTRIBUCION DE uv Y sla


# CORREGIR


plot(podocarps$sla, podocarps$uv1)





############################################################################
# 1.1 Analisis de correlacion
############################################################################

colnames(podocarps)
correl<-cor(podocarps,use="complete.obs")
symnum(correl, corr=TRUE, cutpoint=c(.1,.6,.8,.9),symbols = c("0",".","+","*","S"), legend=TRUE)
correl
?cor
pairs(~elevacion+uv1+uv2+rendimiento+espesor+area+peso+sla+dap+altura+forma_copa, data=podocarps )

pairs(~elevacion+uv1+espesor+area+peso+sla+DAP+altura, data=podocarps )
hist(podocarps$sla)
hist(podocarps$sla[-18]) 
hist(log(podocarps$sla[-18]) )

############################################################################
rasgos <- podocarps [,11:14]
pcarasgos <- prcomp(rasgos, scale. = TRUE)
summary(pcarasgos) 
loadings(pcarasgos)

pcarasgos$rotation
plot(pcarasgos)
biplot(pcarasgos)
############################################################################

colnames(podocarps)





lm2 <- lm(uv1 ~ sla + DAP + espesor + area + posi, data=podocarps)
summary(lm2)

# MODELOS MIXTOS
############################################################################

# WORLD CLIM 
############################################################################

#DATOS CLIMATICOS
############################################################################

#               #EJEMPLO
#               library(raster)
#               library(sp)
#               
#               r <- getData("worldclim",var="bio",res=10) 
#               r <- r[[c(1,12)]]
#                names(r) <- c("Temp","Prec")
#               lats <- c(48.45435,48.45445 )
#               lons <- c(-2.04734, -2.04724)
#               coords <- data.frame(x=lons,y=lats)
#               points <- SpatialPoints(coords, proj4string = r@crs)
#               values <- extract(r,points)
#               df <- cbind.data.frame(coordinates(points),values)
#               df
#               plot(r[[1]])
#               plot(points,add=T)

############################################################################


# EXPERIMENTO PROPIO
library(raster)
library(sp)
?getData
# Bio 1 and Bio12 are mean anual temperature and anual precipitation:

rtmx <- getData("worldclim", var = "tmax", res = 0.5, lon = podocarps$longitud, lat = podocarps$latitud) 
rtmx %>% class()
rtmin <- getData("worldclim",var= "tmin",res=0.5, lon=podocarps$longitud, lat=podocarps$latitud) 
ralt <- getData("worldclim",var= "alt",res=0.5, lon=podocarps$longitud, lat=podocarps$latitud) 
r<- getData("worldclim",var= "bio",res=0.5, lon=podocarps$longitud, lat=podocarps$latitud) 

r
r <- r[[c(1,12)]]
names(r) <- c("Temp","Prec")
coords <- data.frame(x=podocarps$longitud,y=podocarps$latitud)
# ?data.frame()

points <- SpatialPoints(coords, proj4string = r@crs)
values <- extract(r,points)
df <- cbind.data.frame(coordinates(points),values)
df
plot(df)
plot()

plot(rtmx)


#################################################################################################################

#    https://gis.stackexchange.com/questions/264877/worldclim-2-0-solar-radiation-data
#    getData() only downloads tmean, tmin, tmax, prec, bio and alt from WorldClim v1.4. Check lines 252 to 325.
#    You can download files with utils::download.file() function using WorlClim data URLs, but it will download a full file, not a specific tiles as getData() does.

###############################################################################################################
###############################################################################################################


MODISTools:: 
  mt_batch_subset(df, product, band, start = "2000-01-01",
                  end = format(Sys.time(), "%Y-%m-%d"), km_lr = 0, km_ab = 0,
                  out_dir = tempdir(), internal = TRUE, ncores = "auto")
  
mt_*() 
  mt_bands()
  mt_products()
  head(mt_products()
  
  
  