#Tables

Results_HIV_df%>%
  pivot_wider(
    names_from = Year,
    values_from = c(Numerator, Denominator, Percentage))%>%
  select(Characteristic, 
         Numerator_2017, Denominator_2017, Percentage_2017, 
         Numerator_2022, Denominator_2022, Percentage_2022)%>%
  mutate(Characteristic=factor(Characteristic,
                               levels=c("active 50mile","active 100mile", "active 200mile", "active 60min",
                                        "HIV 50mile","HIV 100mile", "HIV 200mile", "HIV 60min",
                                        "HOPE 50mile","HOPE 100mile", "HOPE 200mile", "HOPE 60min"),
                               labels = c("Active kidney center 50 mile radius",
                                          "Active kidney center 100 mile radius", 
                                          "Active kidney center 200 mile radius", 
                                          "Active kidney center 60 min radius",
                                          "Active HIV R+ kidney center 50 mile radius",
                                          "Active HIV R+ kidney center 100 mile radius", 
                                          "Active HIV R+ kidney center 200 mile radius", 
                                          "Active HIV R+ kidney center 60 min radius",
                                          "Active HOPE D+ kidney center 50 mile radius",
                                          "Active HOPE D+ kidney center 100 mile radius",
                                          "Active HOPE D+ kidney center 200 mile radius",
                                          "Active HOPE D+ kidney center 60 min radius"
                                          
                                          
                               )))%>%
  arrange(Characteristic)%>%
  gt() %>%
  tab_spanner(
    label = "2017", 
    columns = vars(Numerator_2017, Denominator_2017, Percentage_2017)
  ) %>%
  tab_spanner(
    label = "2022", 
    columns = vars(Numerator_2022, Denominator_2022, Percentage_2022)
  )%>%
  fmt_number(
    columns = vars(Numerator_2017, Denominator_2017, 
                   Numerator_2022, Denominator_2022),
    decimals = 0,  
    use_seps = TRUE
  )%>%
  tab_header(
    title = "Access of PLWH to transplant centers in 2017 and 2022"  # Title of your table
  )%>%
  cols_label(
    Characteristic = "Characteristic",
    Numerator_2017 = "Numerator",
    Denominator_2017 = "Denominator",
    Percentage_2017 = "Percentage",
    Numerator_2022 = "Numerator",
    Denominator_2022 = "Denominator",
    Percentage_2022 = "Percentage"
  )%>%
  # Style for 2017 columns (data cells)
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(columns = vars(Numerator_2017, Denominator_2017, Percentage_2017))
  ) %>%
  # Style for 2022 columns (data cells)
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = vars(Numerator_2022, Denominator_2022, Percentage_2022))
  ) %>%
  # Add borders to separate the two groups (2017 and 2022)
  tab_style(
    style = cell_borders(sides = c("right"), weight = px(2), color = "black"),
    locations = cells_body(columns = vars(Numerator_2017, Denominator_2017, Percentage_2017))
  ) %>%
  tab_style(
    style = cell_borders(sides = c("left"), weight = px(2), color = "black"),
    locations = cells_body(columns = vars(Numerator_2022, Denominator_2022, Percentage_2022))
  )%>%
  gtsave("Kidney analysis results/Table 1.docx")

Results_nonHIV_df%>%
  pivot_wider(
    names_from = Year,
    values_from = c(Numerator, Denominator, Percentage))%>%
  select(Characteristic, 
         Numerator_2017, Denominator_2017, Percentage_2017, 
         Numerator_2022, Denominator_2022, Percentage_2022)%>%
  mutate(Characteristic=factor(Characteristic,
                               levels=c("active 50mile","active 100mile", "active 200mile", "active 60min",
                                        "HIV 50mile","HIV 100mile", "HIV 200mile", "HIV 60min",
                                        "HOPE 50mile","HOPE 100mile", "HOPE 200mile", "HOPE 60min"),
                               labels = c("Active kidney center 50 mile radius",
                                          "Active kidney center 100 mile radius", 
                                          "Active kidney center 200 mile radius", 
                                          "Active kidney center 60 min radius",
                                          "Active HIV R+ kidney center 50 mile radius",
                                          "Active HIV R+ kidney center 100 mile radius", 
                                          "Active HIV R+ kidney center 200 mile radius", 
                                          "Active HIV R+ kidney center 60 min radius",
                                          "Active HOPE D+ kidney center 50 mile radius",
                                          "Active HOPE D+ kidney center 100 mile radius",
                                          "Active HOPE D+ kidney center 200 mile radius",
                                          "Active HOPE D+ kidney center 60 min radius"
                                          
                                          
                               )))%>%
  arrange(Characteristic)%>%
  gt() %>%
  tab_spanner(
    label = "2017", 
    columns = vars(Numerator_2017, Denominator_2017, Percentage_2017)
  ) %>%
  tab_spanner(
    label = "2022", 
    columns = vars(Numerator_2022, Denominator_2022, Percentage_2022)
  )%>%
  fmt_number(
    columns = vars(Numerator_2017, Denominator_2017, 
                   Numerator_2022, Denominator_2022),
    decimals = 0,  
    use_seps = TRUE
  )%>%
  tab_header(
    title = "Access of people without HIV to transplant centers in 2017 and 2022"  # Title of your table
  )%>%
  cols_label(
    Characteristic = "Characteristic",
    Numerator_2017 = "Numerator",
    Denominator_2017 = "Denominator",
    Percentage_2017 = "Percentage",
    Numerator_2022 = "Numerator",
    Denominator_2022 = "Denominator",
    Percentage_2022 = "Percentage"
  )%>%
  # Style for 2017 columns (data cells)
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(columns = vars(Numerator_2017, Denominator_2017, Percentage_2017))
  ) %>%
  # Style for 2022 columns (data cells)
  tab_style(
    style = cell_fill(color = "lightblue"),
    locations = cells_body(columns = vars(Numerator_2022, Denominator_2022, Percentage_2022))
  ) %>%
  # Add borders to separate the two groups (2017 and 2022)
  tab_style(
    style = cell_borders(sides = c("right"), weight = px(2), color = "black"),
    locations = cells_body(columns = vars(Numerator_2017, Denominator_2017, Percentage_2017))
  ) %>%
  tab_style(
    style = cell_borders(sides = c("left"), weight = px(2), color = "black"),
    locations = cells_body(columns = vars(Numerator_2022, Denominator_2022, Percentage_2022))
  )%>%
  gtsave("Kidney analysis results/Table 2.docx")
