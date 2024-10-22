
make_Total_nidos <- function(seabird_pairs, island) {
  seabird_pairs |>
    filter_by_species_and_island(islet = island) |>
    dplyr::transmute(Temporada = Season, Total_nidos = Maximum_number_of_nests)
}

filter_by_species_and_island <- function(seabird_pairs, islet) {
  seabird_pairs |> dplyr::filter(Species_name == "Brown Pelican", Island == islet)
}
