breakpoints_size <- tables_data %>%
  drop_na(size) %>% 
  mutate(mes = yearmonth(mes)) %>% 
  as_tsibble(key = gvkey_iid, index = mes) %>%
  index_by(mes) %>% 
  summarise(
    p30 = quantile(size, probs = 0.30, na.rm = TRUE),
    p70 = quantile(size, probs = 0.70, na.rm = TRUE),
  ) 
  
  
size_sorted <- tables_data %>% 
  mutate(mes = yearmonth(mes)) %>% 
  as_tsibble(key = gvkey_iid, index = mes) %>%
  index_by(mes) %>% 
  nest() %>% 
  left_join(breakpoints_size, by = "mes")
  
for (i in seq(nrow(size_sorted))) {
  size_sorted$data[[i]]$pf <-
    if_else(
      size_sorted$data[[i]]$size < size_sorted$p30[i],
      "Small Size",
      if_else(
        size_sorted$data[[i]]$size > size_sorted$p30[i] & size_sorted$data[[i]]$size < size_sorted$p70[i],
        "Medium Size",
        "Large Size"
      )
    )
}

size_sorted[, 1:2] %>% 
  unnest(data) %>% 
  group_by(mes, pf) %>% 
  count(pf) %>% 
  pivot_wider(names_from = pf, values_from = n)
  
