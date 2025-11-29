# 1. LIBRERÍAS
###############################################################################

library(tidyverse)
library(sf)
library(jsonlite)
library(readxl)
library(broom)
library(modelsummary)
library(gt)
library(kableExtra)
library(purrr)
library(stringi)
library(leaflet)

###############################################################################
# 2. DESCARGA Y UNIFICACIÓN DE DATOS SOCIOECONÓMICOS (2017–2022)
###############################################################################

cargar_geo <- function(url, year){
  read.csv(url) %>% mutate(year = year)
}

geografia.total <- bind_rows(
  cargar_geo("https://...2017_ARG.csv", 2017),
  cargar_geo("https://...2018_ARG.csv", 2018),
  cargar_geo("https://...2019_ARG.csv", 2019),
  cargar_geo("https://...2020_ARG.csv", 2020),
  cargar_geo("https://...2021_ARG.csv", 2021),
  cargar_geo("https://...2022_ARG.csv", 2022)
)

# Chequeo inicial
geografia.total %>% count(year)
head(geografia.total)

###############################################################################
# 3. DATOS DE POBREZA POR TRIMESTRE (2017–2022)
###############################################################################

agregar_tiempo <- function(df, year, quarter){
  df %>% mutate(year = year, quarter = quarter)
}

# Cargar todos los trimestres automáticamente
urls_poverty <- list(
  "2017" = c("...2017-02-15.csv","...2017-05-15.csv","...2017-08-15.csv","...2017-11-15.csv"),
  "2018" = c("...2018-02-15.csv","...2018-05-15.csv","...2018-08-15.csv","...2018-11-15.csv"),
  "2019" = c("...2019-02-15.csv","...2019-05-15.csv","...2019-08-15.csv","...2019-11-15.csv"),
  "2020" = c("...2020-02-15.csv","...2020-05-15.csv","...2020-08-15.csv","...2020-11-15.csv"),
  "2021" = c("...2021-02-15.csv","...2021-05-15.csv","...2021-08-15.csv","...2021-11-15.csv"),
  "2022" = c("...2022-02-15.csv","...2022-05-15.csv","...2022-08-15.csv","...2022-11-15.csv")
)

datos.agrup <- map2_dfr(
  names(urls_poverty),
  urls_poverty,
  ~ map2_dfr(.y, 1:4, function(url, q){
    agregar_tiempo(read.csv(url), as.numeric(.x), q)
  })
)

###############################################################################
# 4. UNIFICACIÓN DE DATOS SOCIOECONÓMICOS + POBREZA
###############################################################################

datos.completo <- datos.agrup %>%
  left_join(geografia.total, by = c("HOGAR_REF_ID","year"))

###############################################################################
# 5. CARGA Y UNIFICACIÓN DE GEOJSONs DE TODOS LOS CIRCUITOS
###############################################################################

# Función para cargar shapefiles sin repetir código
cargar_geojson <- function(path){
  st_read(path) %>% mutate(across(-geometry, as.character))
}

paths_json <- list.files(
  path = "C:/Users/Martín/OneDrive/Escritorio/Estudio/Data/jsons/",
  pattern = "*.geojson",
  full.names = TRUE
)

circuitos_sf <- map_dfr(paths_json, cargar_geojson)

###############################################################################
# 6. CÁLCULO DE POBREZA DEPARTAMENTAL
###############################################################################

agrupada <- datos.agrup %>%
  left_join(geografia.total, by = "HOGAR_REF_ID") %>%
  mutate(
    Pobreza_logica = case_when(
      Pobreza %in% c("True","TRUE","true",TRUE) ~ TRUE,
      Pobreza %in% c("False","FALSE","false",FALSE) ~ FALSE
    )
  )

dept_pobreza <- agrupada %>%
  group_by(NOMPROV, NOMDPTO) %>%
  summarise(
    total_hogares = n(),
    hogares_pobres = sum(Pobreza_logica, na.rm = TRUE),
    porcentaje_pobreza = hogares_pobres / total_hogares * 100,
    .groups = "drop"
  ) %>%
  mutate(NOMPROV = recode(NOMPROV,"Ciudad Autónoma de Buenos Aires" = "CABA"))

###############################################################################
# 7. MAPA DE POBREZA POR DEPARTAMENTO
###############################################################################

normalize_names <- function(x){
  x %>% str_to_lower() %>% str_trim() %>% stri_trans_general("Latin-ASCII")
}

secciones <- st_read("C:/.../geo_x_seccion_arg.geojson") %>%
  mutate(
    provincia_norm = normalize_names(provincia),
    departamento_norm = normalize_names(departamento)
  )

dept_pobreza <- dept_pobreza %>%
  mutate(
    provincia_norm = normalize_names(NOMPROV),
    departamento_norm = normalize_names(NOMDPTO)
  )

mapa_pobreza <- secciones %>% left_join(dept_pobreza, by = c("provincia_norm","departamento_norm"))

###############################################################################
# 8. DATOS ELECTORALES + POBREZA
###############################################################################

diputados  <- read_excel("C:/.../diputados.xlsx")
senadores  <- read_excel("C:/.../senadores.xlsx")
elecciones <- bind_rows(diputados, senadores)

datos <- elecciones %>%
  left_join(mapa_pobreza, by = "id") %>%
  mutate(across(c(Porcentaje, porcentaje_pobreza, Votos, Participacion, Electores, Votantes), as.numeric))

###############################################################################
# 9. MODELOS LLA Y FP (PROVINCIALES)
###############################################################################

# Estandarizar partidos
datos <- datos %>%
  mutate(
    Provincia = recode(Provincia,"CIUDAD AUTONOMA DE BUENOS AIRES" = "CABA"),
    partido_std = case_when(
      Partido %in% c("ALIANZA LA LIBERTAD AVANZA","LA LIBERTAD AVANZA") ~ "LLA",
      Partido %in% c("FUERZA PATRIA","ALIANZA FUERZA PATRIA","FRENTE DE LA VICTORIA",
                     "FUERZA JUSTICIALISTA MENDOZA","PARTIDO DE LA VICTORIA",
                     "FRENTE JUSTICIALISTA","FUERZA SANTACRUCENA","FUERZA SAN JUAN") ~ "FP",
      TRUE ~ "OTROS"
    )
  )

datos_prov_dept_2p <- datos %>%
  filter(partido_std %in% c("LLA","FP")) %>%
  group_by(Provincia, departamento_norm, partido_std) %>%
  summarise(
    voto_pct = mean(Porcentaje, na.rm = TRUE),
    pobreza_pct = mean(porcentaje_pobreza, na.rm = TRUE)
  )

# Modelos
mod_LLA <- lm(voto_pct ~ pobreza_pct, data = datos_prov_dept_2p %>% filter(partido_std == "LLA"))
mod_FP  <- lm(voto_pct ~ pobreza_pct, data = datos_prov_dept_2p %>% filter(partido_std == "FP"))

modelsummary(
  list("LLA" = mod_LLA, "FP" = mod_FP),
  statistic = "({std.error})",
  stars = TRUE,
  title = "Modelo de regresión pobreza → voto"
)

###############################################################################
# 10. ANÁLISIS DE RESIDUOS PROVINCIALES (SOBRE Y SUB-VOTO)
###############################################################################

modelos_prov <- datos_prov_dept_2p %>%
  group_by(Provincia, partido_std) %>%
  group_modify(~{
    mod <- lm(voto_pct ~ pobreza_pct, data = .x)
    augment(mod, data = .x)
  })

desvios_dept <- modelos_prov %>%
  select(
    Provincia, departamento_norm, partido_std,
    voto_pct, pobreza_pct, voto_esperado = .fitted,
    residuo = .resid, residuo_std = .std.resid
  )

top_sobrevoto <- desvios_dept %>% arrange(desc(residuo)) %>% head(20)
top_subvoto   <- desvios_dept %>% arrange(residuo) %>% head(20)

###############################################################################
# 10. LOOCV PARA PROVINCIAS PEQUEÑAS
###############################################################################

datos_prov_formosa <- datos_prov_dept_2p %>%
  filter(Provincia == "FORMOSA", partido_std == "LLA")

betas_loocv <- map_dbl(1:nrow(datos_prov_formosa), function(i){
  coef(lm(voto_pct ~ pobreza_pct, data = datos_prov_formosa[-i,]))[2]
})

tabla_loocv <- tibble(
  iteracion = 1:length(betas_loocv),
  pendiente_loocv = betas_loocv
)

kable(tabla_loocv, digits = 3)