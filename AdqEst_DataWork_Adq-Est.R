


remove <- c('isd',	'refdist',	'cc',	'isdflag',	'psaflag',	'county',	'adult',	'adult.s',	'atriskper',	'hh.flag',	'setrncosts',	'freelunch',	's31app',	's20jamt',	's20mamt',	's32eamtpp',	'new.grades',	'se53nradj',	'se53nramt',	'sepupils.a',	'sepupils.s',	'hh.section',	'nresadj',	'found.adj',	'se52blend',	'se52found',	'se52nradj',	'se52nramt',	'se53blend',	's23a',	'hldharm038',	's20.4rev',	'ftecount',	'speced',	'sec52',	'sec53',	'newgrdfte',	'nrtot.a',	'senrtot.a',	'ftecount.s',	'psageded.s',	'speced.s',	'sec52.s',	'sec53.s',	'psaseded.s',	'newgrdftes',	'nrtot.s',	'senrtot.s',	'lea.blend',	'hs.sev',	'tif.hs',	'rz.hs',	'nhs.sev',	'tif.nhs',	'rz.nhs',	'com.sev',	'tif.com',	'rz.com',	'ind.sev',	'tif.ind',	'rz.ind')

Adq_Data <- 
  raw_enroll %>% 
  left_join(Add_Cost, by = "dcode") %>%
  left_join(FID_Rev %>% select(-icode), by = c("dcode" = "dnum")) %>%
  left_join(ISD_Rev %>% select(-icode), by = c("dcode" = "dnum")) %>% 
  left_join(cydata %>% select(-icode, -dname, -isdpupils,), by = c("dcode" = "dnum")) %>% 
  left_join(gsrp, by = "icode") %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         refdist = as.numeric(refdist),
         refdist = ifelse(is.na(refdist), dcode, refdist)) %>% 
  group_by(refdist) %>% 
  mutate(refdist.tot.need.ed = sum(stu.need.ed)) %>% 
  ungroup() %>% 
  mutate(refdist.need.ed.pct = stu.need.ed / refdist.tot.need.ed) %>% 
  # left_join(High_need_pov, by = c("refdist" = "dcode")) %>% 
  # mutate(stu.need.high.pov = refdist.need.ed.pct * acs.poverty.5_17,
  #        stu.need.high.pov = ifelse(stu.need.high.pov > stu.need.ed, stu.need.ed,stu.need.high.pov)) %>% 
  
  mutate(adq.cost.base = stu.total.k12 * base.foundation,
         
         adq.cost.ed = stu.need.ed * base.foundation * 0.35,
         
         # adq.cost.high.pov = stu.need.high.pov * base.foundation * 0.15,
         
         
         adq.cost.el.1 = (stu.need.el.wida.0_1.9 + stu.need.el.wida.2_2.9) * base.foundation * 0.7,
         adq.cost.el.2 = stu.need.el.wida.3_3.9 * base.foundation * 0.5,
         adq.cost.el.3 = (stu.need.el 
                          - stu.need.el.wida.0_1.9 
                          - stu.need.el.wida.2_2.9 
                          - stu.need.el.wida.3_3.9) 
                          * base.foundation * 0.35,
         adq.cost.total.el = adq.cost.el.1 + adq.cost.el.2 + adq.cost.el.3,
         
         
         adq.cost.se.mild = stu.need.se.mild * base.foundation * 0.7,
         adq.cost.se.mod = stu.need.se.mod * base.foundation * 1.15,
         adq.cost.se.sev = stu.need.se.sev * base.foundation * 3.0,
         adq.cost.total.se = adq.cost.se.mild + adq.cost.se.mod + adq.cost.se.sev,
         
         # PK
         stu.gsrp = isd.stu.gsrp * dist.isd.pupil.pct, 
         
         stu.total = stu.total.k12 + stu.gsrp,
         
         Age_3_4_not.in.gsrp = Age_3_4 - stu.gsrp, 
         
         adq.cost.pk.current = stu.gsrp * base.foundation * 1.45, 
           
         adq.cost.pk.expansion = Age_3_4_not.in.gsrp * 0.8 * base.foundation * 1.45, 
         
         adq.cost.pk = adq.cost.pk.current + adq.cost.pk.expansion, 
         
         
         # Adequacy Total 
         
         adq.cost.total            = adq.cost.base + adq.cost.ed + adq.cost.total.el + adq.cost.total.se + adq.cost.pk,
         adq.cost.total.pk.current = adq.cost.total - adq.cost.pk.expansion, 
         
         adq.rev                   = adq.cost.total + add.cost.total,
         adq.rev.pk.current        = adq.cost.total.pk.current + add.cost.total,
         
         adq.rev.pp                = adq.rev / stu.total,
         adq.rev.pp.pk.current     = adq.rev.pk.current / stu.total, 
         
         actual.rev                = fid.r.total + dist.isd.ge.rev + dist.isd.se.rev,
         actual.rev.pp             = actual.rev / stu.total,
         
         adq.gap                   = actual.rev - adq.rev,
         adq.gap.pp                = adq.gap / stu.total,
         adq.gap.pk.current        = actual.rev - adq.rev.pk.current,
         adq.gap.pp.pk.current     = adq.gap.pk.current / stu.total,
         
         adq.pct                   = actual.rev / adq.rev,
         adq.pct.pk.current        = actual.rev/ adq.rev.pk.current
         
         ) %>% 
  
  # cleaning
  select(-x) %>% 
  
  mutate(stu.need.pct.ed = stu.need.ed / stu.total, 
         stu.need.pct.se = stu.need.se / stu.total,
         stu.need.pct.el = stu.need.el / stu.total,
         stu.race.pct.black = stu.race.black / stu.total) %>% 
  
  select(dcode, dname, icode, entity.type, locale.name, 
         adq.pct, adq.gap.pp, adq.gap, adq.rev, adq.rev.pp , actual.rev, actual.rev.pp, everything()) %>% 
  filter(entity.type != "ISD District",
         fid.r.total > 1,
         stu.total > 1,
         !is.na(adq.rev)) 



Adq_Data %>% write_csv(here("Adequacy Estimates/District-Adequacy-Estimate/2021_Adq_Data.csv"))


