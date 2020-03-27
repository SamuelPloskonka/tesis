# principales funciones

# importar datos




library(readr)
read_csv(".../")
read_tsv(".../")
read_delim(".../")

library(readxl)
read_xls(".../")
read_xlsx(".../")

# colnames

%>% names()

# limpiear nombres
clean_names()
##################################
#   tidyverse
##################################
# seleccionar columnas
select()

#seleccionar todo menos 
select(-c(, , )) 

#renombrar
select(nuevo_nombre1 = variable1, nuevo_nombre.2 = variable 2)

#seleccionar columnas que empiezan con "cn"
base de datos %>% 
  select(starts_with("cn")
         
# filtrar filas
filter()

# filtrar solo tratamientos que empiezan 

base_de_datos %>% 
  select(variable1, variable2) %>% 
  filter(variable1 == "OTC")
basecompleta %>%  names()


#ejemplo de filter
chem_trait %>% 
select(treatment, taxon) %>% 
  filter(treatment == "OTC",
         taxon == "Epilobium fangii")
# aumentar una columna de nueva variable con MUTATE

chem_trait %>% 
  select(site, elevation, c_percent) %>% 
  mutate(c_percent_prop = c_percent/10)

# summarize
# group_by genera columnas en base a filas que se indique

# chem_trait %>% 
group_by(site) %>% 
  summarise(n_samples = n(),
            n_species = n_distinct(taxon),
            mean_p_percent = mean(p_percent))


# al trabajar con raster
# Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
x tidyr::extract() masks raster::extract()
x dplyr::filter()  masks stats::filter()
x dplyr::lag()     masks stats::lag()
x dplyr::select()  masks raster::select()


filter <- dplyr::filter 
#  extract <- dplyr::extract   ???
# devtools::install_github("r-lib/conflicted")
