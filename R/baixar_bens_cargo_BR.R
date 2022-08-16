#' baixa bens de todos os candidatos do cargo desejado no brasil
#'
#' @param ano ano da eleicao
#' @param cod_cargo codigo do cargo
#'
#' @return
#' @export
#'
#' @examples
baixar_bens_cargo_BR <- function(
          ano,
          cod_cargo){

  ifelse (cod_cargo ==7,
          uf <- c("AC", "AL", "AM", "AP",
                  "BA", "CE",  "ES",
                  "GO", "MA", "MG", "MS",
                  "MT", "PA", "PB", "PE",
                  "PI", "PR", "RJ", "RN",
                  "RO", "RR", "RS", "SC",
                  "SE", "SP", "TO"),
          uf <- c("AC", "AL", "AM", "AP",
                  "BA", "CE", "DF", "ES",
                  "GO", "MA", "MG", "MS",
                  "MT", "PA", "PB", "PE",
                  "PI", "PR", "RJ", "RN",
                  "RO", "RR", "RS", "SC",
                  "SE", "SP", "TO"))
  df <- purrr::map_dfr(uf,
                       purrr::possibly(~baixar_bens_cargo(
    ano = ano,
    uf = .,
    cod_cargo = cod_cargo),
    otherwise = NULL
  ))

  df
}

