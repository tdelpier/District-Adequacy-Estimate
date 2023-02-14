


pk_adq_cost <- 
  Adq_Data %>% 
  summarise(adq.cost.pk = sum(adq.cost.pk)) %>% 
  pull()


gsrp_funding <- gsrp %>% summarise(amount.gsrp = sum(gsrp)) %>% pull()



pk_adq_gap <- gsrp_funding - pk_adq_cost


pk_cost <- tribble(~gsrp.funding, ~pk.adq.cost, ~pk.adq.gap,
                   gsrp_funding, pk_adq_cost, pk_adq_gap)

write.csv(pk_cost, "PK_cost.csv")

