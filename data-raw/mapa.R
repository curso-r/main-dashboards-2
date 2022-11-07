# tab_sf <- sf::read_sf("arquivo.shp")

tab_sf <- readr::read_rds("dados/geo_estados.rds")

library(leaflet)

tab_sf |> 
  leaflet() |> 
  addTiles() |> 
  addPolygons(color = "black", weight = 1)



# EXTRA

# library(ggplot2)
# 
# tab_sf |> 
#   # sf::st_simplify(dTolerance = 0) |> 
#   ggplot(aes(fill = abbrev_state)) +
#   geom_sf()