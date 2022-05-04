## code to prepare `geo_estados` dataset goes here

geo_estados <- geobr::read_state()

saveRDS(geo_estados, "dados/geo_estados.rds")
