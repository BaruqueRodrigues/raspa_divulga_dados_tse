#' pega informacoes dos candidatos do cargo desejado de todo brasil
#'
#' @param ano ano da eleicao
#' @param cod_cargo codigo cargo
#'
#' @return
#' @export
#'
#' @examples
pega_info_cand_BR <- function(ano, cod_cargo){

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

  df <- purrr::map_dfr(uf, ~pega_info_cand(
    ano = ano,
    sigla_estado = .,
    cod_cargo = cod_cargo

  ))

  df
}

