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

  ano_eleicao <- ano
  uf_desejada <- uf
  cod_cargo_desejado <- cod_cargo

  df <- raspa.divulga.dados.tse::pega_info_cand(ano = ano_eleicao,
                                          sigla_estado = uf_desejada,
                                          cod_cargo = cod_cargo_desejado)


  dados_cand <- df %>%
    dplyr::pull(id) %>%
    purrr::map_dfr(
      purrr::possibly(~captura_bens(id_candidato = .x,
                                    uf = uf_desejada,
                                    ano = ano_eleicao),
                      otherwise = NULL)
    )
  # dados_cand <- purrr::map_dfr(df$id,
  #                purrr::possibly(
  #                  raspa.divulga.dados.tse::captura_bens(uf = uf_desejada,
  #                                                        ano = ano_eleicao),
  #                  otherwise = NULL)
  # )

  dados_cand
}


