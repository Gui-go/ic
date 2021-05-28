# R-script 05-eci_br.R


# References --------------------------------------------------------------

# https://grapher.network/blog/
# https://pacha.dev/blog/
# https://pacha.dev/
# https://cran.r-project.org/web/packages/economiccomplexity/economiccomplexity.pdf

# Setup -------------------------------------------------------------------

rm(list = ls())
gc()
options(stringsAsFactors = F)
ggplot2::theme_set(ggplot2::theme_minimal())
options(scipen = 666)


# Packages ----------------------------------------------------------------

if(!require(readr)){install.packages("readr")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(janitor)){install.packages("janitor")}
if(!require(mongolite)){install.packages("mongolite")}
if(!require(readxl)){install.packages("readxl")}
if(!require(reticulate)){install.packages("reticulate")}
if(!require(economiccomplexity)){install.packages("economiccomplexity")}


# Code --------------------------------------------------------------------

sg_uf_br <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")

source(file = "code/functions/data_loc.R")
br_loc <- data_loc(sg_uf_br) %>% 
  group_by(nm_uf, cd_uf) %>% 
  summarise()

exp <- readxl::read_excel("data/EXP_2000_2021_20210527.xlsx") %>% 
  dplyr::mutate(exp = purrr::reduce(select(., ends_with("(US$)")), `+`)) %>% 
  janitor::clean_names() %>% 
  dplyr::rename("nm_uf"="uf_do_produto", "cd_sh2"="codigo_sh2", "nm_sh2"="descricao_sh2") %>% 
  dplyr::select(nm_uf, cd_sh2, nm_sh2, exp)

shp_ufs <- sf::st_read("data/BR_UF_2020/") %>%
  janitor::clean_names() %>% 
  sf::st_set_crs(4326) %>% 
  dplyr::mutate(cd_uf = as.character(cd_uf))

df <- exp %>%
  dplyr::left_join(., br_loc, by = "nm_uf") %>% 
  dplyr::mutate(log_exp=log(exp)) %>%
  dplyr::rename("country"="nm_uf", "product"="cd_sh2", "value"="log_exp") %>% 
  na.omit()

bain <- economiccomplexity::balassa_index(data = df)
cmbain <- complexity_measures(balassa_index = bain)
# cmbain$complexity_index_country

dff <- df %>%
  dplyr::rename(
    "nm_uf"="country", 
    "cd_sh2"="product", 
    "log_exp"="value"
  )

ecidf <- data.frame(
  nm_uf=names(cmbain$complexity_index_country),
  eci=cmbain$complexity_index_country
)
# df_eci <- left_join(ecidf, dff, by="nm_uf") %>%
#   dplyr::arrange(dplyr::desc(eci))


dff_shp <- dplyr::left_join(ecidf, shp_ufs) %>% sf::st_sf()
class(dff_shp)
plot(dff_shp["eci"])

nb <- spdep::poly2nb(dff_shp, queen=TRUE)
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
m1 <- round(spdep::moran.test(dff_shp$eci, lw)$estimate[[1]], 2)

ggplot(dff_shp)+
  geom_sf(aes(fill=eci), color="black", size=.2)+
  scale_fill_gradient(low="white", high="blue")+
  annotate(x = -44.878609, y = -28.916854, geom = "text", label=paste0("M1=", "0.666"))+
  labs(title = "Gráfico teste", caption = "eci.app.br", y = "Latitude", x = "Longitude")

# source(file = "code/functions/fct_insertmongodb.R")
# fmongo_insert(df = ecidf, nm_db = "db1", nm_collec = "br_uf_raweci")
