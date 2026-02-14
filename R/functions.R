

#Define a function that takes an SF data object and returns 60 minute isochrones
set_60_min_isochrone_at_7AM<-function(SFObject, year) {
  
  if (nrow(SFObject) == 0) {
    return(
      SFObject %>%
        st_transform(5070)
    )
  }else{
    tempDF<-SFObject%>%
      mutate(TimeZone=tz_lookup(., method="accurate"))%>%
      st_transform(4326)
    
    FinalDF <- vector("list", length(timezones))
    
    
    for(i in 1:length(timezones))
    {
      TZ<-converted_times[[year]][[i,1]]
      
      UTCTime<-format(converted_times[[year]][[i,3]], "%Y-%m-%dT%H:%M:%SZ")
      
      if (nrow(filter(tempDF,TimeZone==TZ))>0)
      {
        temp<-tempDF%>%
          filter(TimeZone==TZ)%>%
          mb_isochrone(time = 60, 
                       profile = "driving-traffic",
                       depart_at = UTCTime,
                       id_column = "OTCCode")%>%
          rename(OTCName=id)
        FinalDF[[i]]<-temp
      }
    }
    
    FinalDF%>%
      bind_rows()%>%
      st_transform(5070)%>%
      return()
  }
}

#Function that converts results df into a gt format table

convert_resultsdf_to_table<-function(resultsdf, 
                                     year_list_fn=year_list,
                                     row_values,
                                     row_labels,
                                     table_title){
  
  
  resultsdf%>%
    
    #Reshape data set
    pivot_wider(
      names_from = Year,
      values_from = c(Numerator, Denominator, Percentage),
      names_vary = "slowest"
    )%>%
    
    #Use the table defined above to turn Characteristic into a factor variable
    mutate(Characteristic=factor(Characteristic,
                                 levels = row_values,
                                 labels = row_labels))%>%
    arrange(Characteristic)%>%
    
    #Convert to a gt format table
    gt()%>%
    
    #Groups columns by year with a spanner label
    {
      purrr::reduce(
        year_list_fn,
        ~ .x %>%
          tab_spanner(
            label = .y,
            columns = matches(paste0("_", .y, "$"))
          ),
        .init = .
      )
    } %>%
    
    #Sets coluns to round to 0 decimal places for numbers and 1 decimal place for percentages
    fmt_number(
      columns = matches(paste0("Numerator_|Denominator_")),
      decimals = 0,
      use_seps = TRUE
    ) %>%
    fmt_number(
      columns = matches("Percentage_"),
      decimals = 1
    )%>%tab_header(
      title = table_title
    )%>%
    
    #Removes "_year" from columns
    cols_label_with(~ "Numerator",   columns = matches("^Numerator_")) %>%
    cols_label_with(~ "Denominator", columns = matches("^Denominator_")) %>%
    cols_label_with(~ "Percentage",  columns = matches("^Percentage_"))%>%
    
    #Shades columns by year for easier reading
    
    {
      purrr::reduce(
        seq_along(year_list_fn),
        ~ .x %>%
          tab_style(
            style = cell_fill(
              color = colorRampPalette(c("lightgray","lightblue"))(
                length(year_list_fn)
              )[.y]            ),
            locations = cells_body(
              columns = matches(paste0("_", year_list_fn[.y], "$"))
            )
          ),
        .init = .
      )
    }%>%
    
    #Add separators between years
    {
      purrr::reduce(
        seq_len(length(year_list_fn) - 1),
        ~ .x %>%
          tab_style(
            style = cell_borders(
              sides = "right",
              weight = px(2),
              color = "black"
            ),
            locations = cells_body(
              columns = matches(paste0("Percentage_", year_list_fn[.y], "$"))
            )
          ) %>%
          tab_style(
            style = cell_borders(
              sides = "left",
              weight = px(2),
              color = "black"
            ),
            locations = cells_body(
              columns = matches(paste0("Numerator_", year_list_fn[.y + 1], "$"))
            )
          ),
        .init = .
      )
    }
  
  
}