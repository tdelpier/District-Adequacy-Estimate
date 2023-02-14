

# Student Enrollment 
student_enrollment <- 
  read_csv(here("Adequacy Estimates/District Adequacy Estimate/2021_Data/2021_District-Student-Enrollment.csv")) %>% 
  clean_names(sep_out = ".") %>% 
  rename(dcode = district.code)

# SE
SE_enrollment <- 
  read_csv(here("Adequacy Estimates/District Adequacy Estimate/2021_Data/2021_SE-Enrollment.csv")) %>% 
  clean_names(sep_out = ".")


# EL
EL_enrollment <- read_excel(here("Adequacy Estimates/District Adequacy Estimate/2021_Data/2021_ELL.xlsx"))%>% 
  mutate(dcode = as.numeric(dcode)) %>%
  rename(stu.need.el.wida.0_1.9 = WIDA_0_1.9,
         stu.need.el.wida.2_2.9 = WIDA_2.0_2.9, 
         stu.need.el.wida.3_3.9 = WIDA_3.0_3.9) %>% 
  select(dcode, stu.need.el.wida.0_1.9, stu.need.el.wida.2_2.9, stu.need.el.wida.3_3.9)


# PK


  ### Downloading original PK data
  # ID_crosswalk <- read.csv(here("Identifiying Data/ID_crosswalk.csv"))
  # 
  # pk <- get_acs(geography = "school district (unified)",
  #               table = "B01001",
  #               state = "Michigan",
  #               year = (fiscal.year - 1),
  #               key = CENSUS_KEY,
  #               geometry = FALSE,
  #               cache_table = TRUE) %>%
  #   filter(variable %in% c("B01001_003", "B01001_027")) %>%
  #   group_by(GEOID) %>%
  #   summarise(Age_0_5 = sum(estimate)) %>%
  #   mutate(leaid = as.numeric(GEOID),
  #          Age_3_4 = Age_0_5 * 0.4) %>%
  #   left_join(ID_crosswalk, by = "leaid") %>%
  #   rename(dcode = stid_ccdlea) %>%
  #   select(dcode, Age_0_5, Age_3_4)
  # write.csv(pk, "2021_PK.csv")

pk <- read.csv("2021_PK.csv")



# High Needs Poverty 

    ## Downloading original high needs poverty data 
    # High_need_pov <- tt_import_census_school("B17020", year = 2019)
    # write.csv(High_need_pov, "High_need_pov.csv")

# High_need_pov <- 
#   read_csv("High_need_pov.csv") %>% 
#   filter(variable %in% c("B17020_003", "B17020_004", "B17020_005"),
#          GEOID != "2699999") %>% 
#   pivot_wider(id_cols = "GEOID", values_from = "estimate", names_from = "variable") %>% 
#   mutate(acs.poverty.5_17 = (B17020_003 / 3) + B17020_004 + B17020_005,
#          nces.code = as.character(GEOID)) %>% 
#   left_join(district_id, by = "nces.code") %>% 
#   select(dcode, acs.poverty.5_17) %>% 
#   mutate(dcode = as.numeric(dcode),
#          dcode = ifelse(dcode == 82010, 82015, dcode)) # recoding Detroit
  





# Merge and Clean
raw_enroll <- 
  student_enrollment %>% 
  left_join(SE_enrollment, by = "dcode") %>% 
  left_join(EL_enrollment, by = "dcode") %>% 
  left_join(pk, by = "dcode") %>% 
  # left_join(High_need_pov, by = "dcode") %>% 
  rename_with(~str_remove(.,".enrollment")) %>% 
  rename_with(~str_replace(., "grade.", "stu.grade.")) %>% 
  rename(stu.grade.k = kindergarten) %>% 
  mutate(stu.total.k12 =
           stu.grade.k +
           stu.grade.1 +
           stu.grade.2 +
           stu.grade.3 +
           stu.grade.4 +
           stu.grade.5 +
           stu.grade.6 +
           stu.grade.7 +
           stu.grade.8 +
           stu.grade.9 +
           stu.grade.10 +
           stu.grade.11 +
           stu.grade.12) %>% 

  rename(stu.sex.male = male, 
         stu.sex.female = female,
         stu.race.native = american.indian,
         stu.race.asian = asian,
         stu.race.black = african.american,
         stu.race.hispanic = hispanic,
         stu.race.hawaiian = hawaiian,
         stu.race.white = white,
         stu.race.twomore = two.or.more.races,
         
         stu.need.ed = economic.disadvantaged,
         stu.need.se = special.education,
         stu.need.el = english.language.learners,
         
         stu.need.se.mild = gen.ed80, 
         stu.need.se.mod = gen.ed40.79,
         stu.need.se.sev = gen.ed.lt40
         ) %>% 

  select(dcode, icode, dname, entity.type, locale.name, starts_with("stu."), Age_0_5, Age_3_4)


rm(SE_enrollment, EL_enrollment, student_enrollment, pk)


