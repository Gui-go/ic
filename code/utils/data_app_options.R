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

df <- data.frame(
  "tema"=c("eci", "eci", "eci", "edu", "exp", "exp", "trab"),
  "stat"=c("eci", "log_eci", "log_eci_log", "edu", "85", "02", "trab")
)
rio::export(df, "data/option_stats.csv")
