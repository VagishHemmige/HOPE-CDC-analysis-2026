# Figures


#----Plot model of data derivation and calculations----

g <- grViz("
digraph hope_access {

graph [
  rankdir = TB
  ranksep = 1.5
  nodesep = 1.2
]

node [
  fontname = Helvetica
  fontsize = 20
]

# ----------------
# DATA SOURCES
# ----------------

node [
  shape = ellipse
  style = filled
  fillcolor = '#E0E0E0'
  margin = '0.35,0.2'
  fontsize = 22
]

tiger  [label = 'US Census TIGER\ngeographic data']
srtr   [label = 'SRTR transplant\nregistry data']
mapbox [label = 'Mapbox\nrouting API']
cdc    [label = 'CDC HIV\nsurveillance data']
census [label = 'US Census\npopulation (ACS)']

{rank = same; tiger; srtr; mapbox; cdc; census}

# ----------------
# DERIVED DATA
# ----------------

node [
  shape = box
  style = 'rounded,filled'
  fillcolor = '#E8F1FF'
  margin = '0.25,0.2'
  fontsize = 20
]

centers [
label = <
<TABLE BORDER='0' CELLBORDER='0' CELLPADDING='8'>
<TR><TD><B>Transplant center</B></TD></TR>
<TR><TD><B>classification</B></TD></TR>
<TR><TD ALIGN='LEFT'>• Active centers</TD></TR>
<TR><TD ALIGN='LEFT'>• HIV transplant centers</TD></TR>
<TR><TD ALIGN='LEFT'>• HOPE centers</TD></TR>
</TABLE>
>
]

dist [
label = <
<TABLE BORDER='0' CELLBORDER='0'>
<TR><TD>Distance buffers</TD></TR>
<TR><TD>(50, 100, 200 miles)</TD></TR>
</TABLE>
>
]

iso [
label = <
<TABLE BORDER='0' CELLBORDER='0'>
<TR><TD>Travel-time</TD></TR>
<TR><TD>isochrones</TD></TR>
<TR><TD>(60 minutes)</TD></TR>
</TABLE>
>
]

tract [
label = <
<TABLE BORDER='0' CELLBORDER='0'>
<TR><TD>Tract HIV</TD></TR>
<TR><TD>burden estimates</TD></TR>
</TABLE>
>
]

{rank = same; dist; iso; tract}

# ----------------
# MODEL
# ----------------

node [
  shape = box
  style = 'rounded,filled'
  fillcolor = '#F4E1D2'
  margin = '0.3,0.25'
]

model [
width = 3.6
label = <
<TABLE BORDER='0' CELLBORDER='0' CELLPADDING='8'>
<TR><TD><B>Geographic access model</B></TD></TR>
<TR><TD>Intersect access zones with</TD></TR>
<TR><TD>tract-level HIV population</TD></TR>
</TABLE>
>
]

# ----------------
# RESULT
# ----------------

node [
  shape = box
  style = 'rounded,filled'
  fillcolor = '#DFF2DF'
  fontsize = 22
]

result [
label = <
<TABLE BORDER='0' CELLBORDER='0' CELLPADDING='8'>
<TR><TD>Population with</TD></TR>
<TR><TD>geographic access</TD></TR>
<TR><TD>to HIV transplant centers</TD></TR>
</TABLE>
>
]

# ----------------
# INVISIBLE EDGES (layout alignment)
# ----------------

tiger -> dist [style=invis]
srtr -> centers [style=invis]
mapbox -> iso [style=invis]
cdc -> tract [style=invis]

# ----------------
# EDGES
# ----------------

srtr -> centers

centers -> dist
tiger -> dist

centers -> iso
mapbox -> iso

cdc -> tract
census -> tract

dist -> model
iso -> model
tract -> model

model -> result

}
")

# Export diagram
svg <- export_svg(g)

writeLines(svg, "figures/hope_cdc_access_model.svg")

rsvg_png(
  charToRaw(svg),
  "figures/hope_cdc_access_model.png",
  width = 2600
)


#----Create dot plot sf data frames----

county_dots<-list()
county_dots_transformed<-list()


for (year_loop in year_list)
  
{
  
  county_dots[[year_loop]]<-as_dot_density(Merged_Counties[[year_loop]], 
                                           "county_cases", 
                                           values_per_dot =100)
  
  county_dots_transformed[[year_loop]]<-county_dots[[year_loop]]%>%
    st_transform(5070)
  
}

#Transform us state map
states_sf_transformed<-states_sf%>%
  st_transform(5070)


#----Maps of active transplant centers----
Activemap<-list()
HIV_map<-list()

for (organ_loop in organ_list) {
  for (distance_loop in distance_list){
    for (year_loop in year_list) {
      
      #Map all transplant centers and a 50-mile catchment area upon the dot map of HIV
      Activemap[[organ_loop]][[year_loop]]<-ggplot() +
        geom_sf(data = county_dots_transformed[[year_loop]], size = 0.3, color = "navy", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste("Radius around",organ_loop, "transplant centers,", year_loop),
             subtitle = "(1 dot = 100 HIV cases)")+
        geom_sf(data=Transplant_centers_active_buffer[[organ_loop]][[distance_loop]][[year_loop]], color="red", fill=NA)+
        geom_sf(data=states_sf_transformed, fill=NA)
      ggsave(paste0("figures/",organ_loop,"/Active ", organ_loop," ",distance_loop," map", year_loop,".svg"))
      
      
      
      #Map transplant centers with HIV R+ and a 60 minute catchment area upon the dot map of HIV, 2013-2017
      HIV_map[[organ_loop]][[year_loop]]<-ggplot() +
        geom_sf(data = county_dots_transformed[[year_loop]], size = 0.3, color = "navy", alpha = 0.5) +
        theme_minimal() +
        labs(title = paste0(as.numeric(year_loop)-4, " - ", year_loop))+
        geom_sf(data=Transplant_centers_HIV_buffer[[organ_loop]][[distance_loop]][[year_loop]], color="red", fill=NA)+
        geom_sf(data=states_sf_transformed, fill=NA)
      ggsave(paste0("figures/",organ_loop,"/HIV ", organ_loop," ",distance_loop," map", year_loop,".svg"))
      
      
    }
  }
}


#----Paired plots for PLWH comparing years----

for (organ_loop in organ_list) {
  for (distance_loop in distance_list){
    
    
    
    #Create distance label
    distance_label <- dplyr::case_when(
      stringr::str_detect(distance_loop, "mile$") ~ 
        paste0(stringr::str_remove(distance_loop, "mile"), " miles"),
      stringr::str_detect(distance_loop, "min$") ~ 
        paste0(stringr::str_remove(distance_loop, "min"), " mins")
    )
    
    
    #Create paired plot for active, no HIV
    paired_plot<-make_paired_plot(organ=organ_loop,
                                  distance=distance_loop,
                                  outcome="active",
                                  buffer_list=Transplant_centers_active_buffer,
                                  year1="2017",
                                  year2="2022",
                                  plottitle=glue::glue("Active {organ_loop} transplant center, {distance_label}")
    )
    
    
    
    
    ggsave(glue("figures/{organ_loop}/Combined {organ_loop} plot 2026-2-14 {distance_loop} active.svg"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           bg = "white")
    
    ggsave(glue("figures/{organ_loop}/Combined {organ_loop} plot 2026-2-14 {distance_loop} active.png"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           dpi = 150,
           device = ragg::agg_png,
           bg = "white")

    
    #Create paired plot for active, comparing HIV to non-HIV
    paired_plot<-make_paired_plot(organ=organ_loop,
                                  distance=distance_loop,
                                  outcome="active",
                                  buffer_list=Transplant_centers_active_buffer,
                                  year1="2017",
                                  year2="2022",
                                  plottitle=glue::glue("Active {organ_loop} transplant center, {distance_label}"),
                                  include_nonHIV = TRUE
    )
    
    
    
    
    ggsave(glue("figures/{organ_loop}/Active{organ_loop}CenterCombinedPlot{distance_loop}.svg"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           bg = "white")
    
    ggsave(glue("figures/{organ_loop}/Active{organ_loop}CenterCombinedPlot{distance_loop}.png"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           dpi = 150,
           device = ragg::agg_png,
           bg = "white")
    
    
    
    #Create paired plot for HIV
    paired_plot<-make_paired_plot(organ=organ_loop,
                                  distance=distance_loop,
                                  outcome="HIV",
                                  buffer_list=Transplant_centers_HIV_buffer,
                                  year1="2017",
                                  year2="2022",
                                  plottitle=glue::glue("Active HIV R+ {organ_loop} transplant center, {distance_label}"))
    
    
    
    
    ggsave(glue("figures/{organ_loop}/Combined {organ_loop} plot 2026-2-14 {distance_loop} HIV.svg"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           bg = "white")
    
    ggsave(glue("figures/{organ_loop}/Combined {organ_loop} plot 2026-2-14 {distance_loop} HIV.png"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           dpi = 150,
           device = ragg::agg_png,
           bg = "white")
    
    
    #Create paired plot
    paired_plot<-make_paired_plot(organ=organ_loop,
                                  distance=distance_loop,
                                  outcome="HOPE",
                                  buffer_list=Transplant_centers_HOPE_buffer,
                                  year1="2017",
                                  year2="2022",
                                  plottitle=glue::glue("Active HOPE D+/R+ {organ_loop} transplant center, {distance_label}"))
    
    
    
    
    ggsave(glue("figures/{organ_loop}/Combined {organ_loop} plot 2026-2-14 {distance_loop} HOPE.svg"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           bg = "white")
    
    ggsave(glue("figures/{organ_loop}/Combined {organ_loop} plot 2026-2-14 {distance_loop} HOPE.png"), 
           paired_plot, 
           width = 12, height = 6, units = "in",
           dpi = 150,
           device = ragg::agg_png,
           bg = "white")
    
    
    
  }
}

# ---- Plot the number of people with HIV in the catchment area of each transplant center----

for (organ_loop in organ_list) {
  for (distance_loop in distance_list){
    
    
    catchment_plot<-plot_center_HIV_catchment(organ=organ_loop,
                              distance=distance_loop,
                              buffer_list=tracts_in_buffers_active[[organ_loop]][[distance_loop]],
                              year1="2017",
                              year2="2022")
    
    
    
    ggsave(glue("figures/{organ_loop}/Beeswarm {organ_loop} plot 2026-2-14 {distance_loop}.svg"), 
           catchment_plot, 
   #        width = 12, height = 6, units = "in",
           bg = "white")
    
    ggsave(glue("figures/{organ_loop}/Beeswarm {organ_loop} plot 2026-2-14 {distance_loop}.png"), 
           catchment_plot, 
  #         width = 12, height = 6, units = "in",
           dpi = 150,
           device = ragg::agg_png,
           bg = "white")
  }
}