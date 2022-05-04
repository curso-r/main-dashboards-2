## code to prepare `pnud_min` dataset goes here

pnud_min <- abjData::pnud_min

readr::write_rds(pnud_min, "dados/pnud_min.rds")

# https://www.br.undp.org/content/brazil/pt/home/about-us.html