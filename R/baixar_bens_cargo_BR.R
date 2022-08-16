baixar_bens_cargo_BR <- function(
          ano,
          cod_cargo){

  uf <- c("AC", "AL", "AM", "AP",
          "BA", "CE", "DF", "ES",
          "GO", "MA", "MG", "MS",
          "MT", "PA", "PB", "PE",
          "PI", "PR", "RJ", "RN",
          "RO", "RR", "RS", "SC",
          "SE", "SP", "TO")
  df <- purrr::map_dfr(uf, ~ baixar_bens_cargo(
    ano = ano,
    uf = .,
    cod_cargo = cod_cargo
  ))

  df
}
