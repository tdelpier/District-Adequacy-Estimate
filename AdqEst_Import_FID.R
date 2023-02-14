

FID_Exp <- 
  tt_import_fid_E() %>%
  filter(FY == fiscal.year,
         fund %in% c(11, 22) ) 
  # # rename(icode = isdcode)%>% 
  # select(dcode, icode, fund, func, object, grant, amount.exp) %>% 



FID_Rev <- 
  tt_import_fid_R() %>%
  mutate(remove = ifelse(dnum == 82170 & fund == 22, 1, 0),
         remove = ifelse(dnum == 81020 & majorclass == 596, 1, 0)) %>% # unusual debt refinancing for Ypsilanti 
  ## Wyandotte has a SE center based program that serves all of Wayne ISD. I add it to ISD SE Rev and distribute across districts 
  filter(FY == fiscal.year,
         fund %in% c(11, 22),
         suffix != 250,
         remove != 1
         ) %>%   
  group_by(dnum, icode) %>% 
  summarise(fid.r.total = sum(amount))


ISD_SE_Rev <- 
  tt_import_fid_R() %>%
  tt_dnum_isd(dnum) %>% 
  filter(flag.isd == 1 | dnum == 82170,
         fund == 22,
         FY == fiscal.year) %>% 
  group_by(icode) %>% 
  summarise(ISD_SE_Rev = sum(amount)) %>% 
  ungroup() %>% 
  mutate(icode = as.numeric(icode))



# Copper Country ISD is missing from the 2020 FID data and I can't figure out why. I should probably email chris may about this. 


# GSRP funding
gsrp <- 
  tt_import_cy_allow(8) %>% 
  filter(icd == 238,
         FY == 2021) %>% 
  mutate(icode = as.numeric(str_sub(dcode, 1,2))) %>% 
  select(icode, amount) %>% 
  group_by(icode) %>% 
  summarise(amount.gsrp = sum(amount)) %>% 
  ungroup() %>% 
  mutate(isd.stu.gsrp = amount.gsrp / 7250)



ISD_GE_Rev <- 
  tt_import_fid_R() %>%
  tt_dnum_isd(dnum) %>% 
  filter(flag.isd == 1,
         fund == 11,
         FY == fiscal.year) %>% 
  group_by(icode) %>% 
  summarise(ISD_GE_Rev = sum(amount)) %>% 
  ungroup() %>% 
  mutate(icode = as.numeric(icode))

  # full_join(gsrp) %>% 
  # mutate(amount.gsrp = ifelse(is.na(amount.gsrp), 0, amount.gsrp),
  #        # ISD_GE_Rev = ISD_GE_Rev - amount.gsrp
  #         # in an earlier iteration I was removing GSRP from revenue
  #        ) %>% 
  # select(-amount.gsrp)
  # 
  
