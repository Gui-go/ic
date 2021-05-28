source(file = "code/functions/data_loc.R")
br_loc <- data_loc(sg_uf_br)

loc_options <- br_loc %>% 
  dplyr::group_by(cd_micro, nm_micro, cd_meso, nm_meso, cd_uf, sg_uf, nm_uf, cd_rg, sg_rg, nm_rg) %>% 
  summarise()
  
# names(br_loc)
# br_loc %>% filter(sg_rg=="N") %>% select(nm_uf) %>% unique() %>% pull() %>% sort()

br_loc2 <- br_loc %>% select(sg_rg, nm_uf, cd_uf) %>% distinct()
nm_estados <- br_loc %>% select(nm_uf) %>% unique() %>% pull() %>% sort()

df <- data.frame("nm_uf"=nm_estados)


rio::export(br_loc2, "data/option_estados.csv")



# option_stats ------------------------------------------------------------

ncm_sh <- readr::read_delim("data/NCM_SH.csv", ";") %>% 
  janitor::clean_names() %>% 
  dplyr::select(co_sh2, no_sh2_por, no_sh2_esp, no_sh2_ing, co_ncm_secrom, no_sec_por, no_sec_esp, no_sec_ing)

ncm_sh %>% 
  filter(co_sh2=="85") %>% 
  select(no_sh2_ing) %>% 
  unique()

df <- data.frame(
  "cd_tema"=c("eci", "eci", "eci", "eci", "edu", "exp", "exp", "trab"),
  "nm_tema"=c("ECI", "ECI", "ECI", "ECI", "Educação", "Exportações", "Exportações", "Trabalho"),
  "cd_stat"=c("eci", "log_eci", "log_eci_log", "eci_log", "edu", "85", "02", "trab"),
  "nm_stat"=c("ECI", "Log ECI", "Log ECI Log", "ECI Log", "edu", "85 - Maquinários elétricos", "02 - Carne", "trab")
)
rio::export(df, "data/option_stats.csv")
