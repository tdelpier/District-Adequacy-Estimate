

# ISD SE costs and GE costs
isd_costs <- 
  cydata %>% 
  mutate(dnum = as.numeric(dcode)) %>% 
  tt_dnum_isd(dnum) %>% 
  filter(flag.isd == 1) %>% 
  left_join(Add_Cost, by = c("dnum" = "dcode")) %>% 
  rename(isd.se.costs = se.costs) %>% 
  mutate(isd.ge.costs = (isdpupils * base.foundation),
         icode = as.numeric(icode)) %>% 
  select(icode, isd.ge.costs, isd.se.costs, isdpupils)


# district share of SE cots and GE enrollment
dist_isd_cost_share <- 
  cydata %>% 
  filter(is.na(isdflag)) %>% 
  group_by(icode) %>% 
  rename(dist.se.costs = se.costs) %>% 
  mutate(isd.se.costs.sum = sum(dist.se.costs),
         isd.pupil.sum = sum(pupilcnt)) %>% 
  ungroup() %>% 
  mutate(dist.isd.se.costs.pct = dist.se.costs / isd.se.costs.sum,
         dist.isd.pupil.pct = pupilcnt / isd.pupil.sum,
         icode = as.numeric(icode)) %>% 
  select(dcode, icode, dist.isd.se.costs.pct, dist.isd.pupil.pct) 
  
  
ISD_Rev <- 
  dist_isd_cost_share %>% 
  left_join(ISD_GE_Rev, by = "icode") %>% 
  left_join(ISD_SE_Rev, by = "icode") %>% 
  left_join(isd_costs, by = "icode") %>% 
  clean_names(sep_out = ".") %>% 
  mutate(dist.isd.ge.rev = (isd.ge.rev - isd.ge.costs) * dist.isd.pupil.pct,
         dist.isd.se.rev = (isd.se.rev - isd.se.costs) * dist.isd.se.costs.pct) %>% 
  mutate(dnum = as.numeric(dcode))
  

rm(dist_isd_cost_share, isd_costs)

# 
# # These are wrong
# 
#   # dist.isd.ge.rev 
#   # dist.isd.se.rev
# 
# ISD_Rev %>% 
#   summarise(isd.ge.rev = sum(isd.ge.rev) / 1000000000, 
#             isd.ge.costs = sum(isd.ge.costs) / 1000000000, 
#             dist.isd.pupil.pct = mean(dist.isd.pupil.pct))
# 
# 
# ISD_SE_Rev %>% summarise(ISD_SE_Rev = sum(ISD_SE_Rev))
