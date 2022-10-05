#' baixa um dataset com os dados do brasil de votacao do g1
#'
#' @param turno turno desejado default 1
#' @param cargo cargo desejado default governador
#'
#' @return dataset com dados de votacao do brasil
#' @export
#'
#' @examples
#' raspa_votacao_g1_br(tunro = 1, cargo = "GOVERNADOR")
raspa_votacao_g1_br <- function(turno = 1,
                                cargo = "GOVERNADOR"){
  uf <- c("AC", "AL", "AM", "AP",
          "BA", "CE", "DF", "ES",
          "GO", "MA", "MG", "MS",
          "MT", "PA", "PB", "PE",
          "PI", "PR", "RJ", "RN",
          "RO", "RR", "RS", "SC",
          "SE", "SP", "TO")

  dataset<- uf %>%
    purrr::map_dfr(~raspa_votacao_g1(turno = turno,
                                     cargo = toupper(cargo),
                                     uf = .

    ))
  dataset
}
