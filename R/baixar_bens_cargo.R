#' baixa bens dos candidatos por cargo
#'
#' @param ano ano da eleicao
#' @param uf uf dos candidatos
#' @param cod_cargo codigo do cargo dos candidatos
#'
#' @return
#' @export
#'
#' @examples
baixar_bens_cargo<- function(ano,
                             uf,
                             cod_cargo){

  df <- raspa.divulga.dados.tse::pega_info_cand(ano = ano,
                                          sigla_estado = uf,
                                          cod_cargo = cod_cargo)


  dados_cand <- purrr::map_dfr(df$id,
                 purrr::possibly(
                   raspa.divulga.dados.tse::captura_bens,
                   otherwise = NULL)
  )

  dados_cand
}


