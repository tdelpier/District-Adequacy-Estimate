

cydata <- 
  tt_import_cy_data(8) %>%
  filter(FY == fiscal.year) %>% 
  clean_names(sep_out = ".") %>% 
  rename(icode = isd) %>% 
  mutate(dnum = as.numeric(dcode))
  
# 
# cydata_2021 <- read_csv(here("Adequacy Estimates/District Adequacy Estimate/2021_Data/2021_cydata.csv")) %>% 
#   clean_names(sep_out = ".") %>% 
#   mutate(dcode = as.numeric(dcode),
#          icode = as.numeric(isd))





