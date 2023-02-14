

FID_Total_Retirement <- 
  FID_Exp %>% 
  filter(object.2 == 2800) %>% 
  group_by(dcode) %>%
  summarise(add.cost.retire.total = sum(amount.exp))

FID_Total_Salaries <- 
  FID_Exp %>% 
  filter(object.2 %in% c(1100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900)) %>% 
  group_by(dcode) %>% 
  summarise(add.cost.salary.total = sum(amount.exp))

FID_Total_Transport <- 
  FID_Exp %>% 
  filter(func == 271) %>% 
  group_by(dcode) %>% 
  summarise(add.cost.transport = sum(amount.exp))



Add_Cost <- 
  FID_Total_Retirement %>% 
  full_join(FID_Total_Salaries, by = "dcode") %>% 
  full_join(FID_Total_Transport, by = "dcode") %>% 
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% 
  mutate(add.cost.retire.covered = (add.cost.salary.total * (0.046 + 0.062 + 0.0145 + 0.006)),
         add.cost.retire.uncovered = add.cost.retire.total - add.cost.retire.covered,
         add.cost.total = add.cost.transport + add.cost.retire.uncovered,
         dcode = as.numeric(dcode)) 


rm(FID_Total_Retirement, FID_Total_Salaries, FID_Total_Transport)





