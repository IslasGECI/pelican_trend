describe("Get version of the module", {
  it("The version is 0.1.0", {
    expected_version <- c("0.1.0")
    obtained_version <- packageVersion("templater")
    version_are_equal <- expected_version == obtained_version
    expect_true(version_are_equal)
  })
})
seabird_pairs <- readr::read_csv("/workdir/tests/data/parejas_aves_marinas_islas_del_pacifico.csv", show_col_types = FALSE)

describe("Filter by species and island", {
  expected_Total_nidos <- tibble::tibble(
    "Species_name" = "Brown Pelican",
    "Island" = "Coronado",
    "Season" = c(2014, 2015, 2016, 2017, 2018, 2019),
    "Maximum_number_of_nests" = c(283, 126, 395, 344, 921, 847)
  )
  obtained_Total_nidos <- filter_by_species_and_island(seabird_pairs, islet = "Coronado")
  it(" has correct species name", {
    expected_species <- expected_Total_nidos$Species_name
    obtained_species <- obtained_Total_nidos$Species_name
    expect_equal(obtained_species, expected_species)
  })
  it(" has the correct island:Coronado", {
    expected_island <- expected_Total_nidos$Island
    obtained_island <- obtained_Total_nidos$Island
    expect_equal(obtained_island, expected_island)
  })
  it(" has the correct island:Todos Santos", {
    obtained_Total_nidos <- filter_by_species_and_island(seabird_pairs, islet = "Todos Santos")
    expected_island <- "Todos Santos"
    obtained_island <- obtained_Total_nidos$Island |> unique()
    expect_equal(obtained_island, expected_island)
  })
  it(" has the correct seasons", {
    expected_seasons <- expected_Total_nidos$Season
    obtained_seasons <- obtained_Total_nidos$Season
    expect_equal(obtained_seasons, expected_seasons)
  })
})

describe("Gold", {
  it("It has the right name column: Coronado", {
    expected_Total_nidos <- tibble::tibble(
      "Temporada" = c(2014, 2015, 2016, 2017, 2018, 2019),
      "Total_nidos" = c(283, 126, 395, 344, 921, 847)
    )
    obtained_Total_nidos <- make_Total_nidos(seabird_pairs, island = "Coronado")
    expect_equal(obtained_Total_nidos, expected_Total_nidos)
  })

  it("It has the right name column: Todos Santos", {
    expected_Total_nidos <- tibble::tibble(
      "Temporada" = c(2014, 2015, 2016, 2017, 2018, 2019),
      "Total_nidos" = c(513, 239, 727, 529, 395, 723)
    )
    obtained_Total_nidos <- make_Total_nidos(seabird_pairs, island = "Todos Santos")
    expect_equal(obtained_Total_nidos, expected_Total_nidos)
  })
})
