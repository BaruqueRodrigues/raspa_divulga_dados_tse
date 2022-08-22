#' baixa_receitas_candidatos
#'
#' @param ano_eleicao ano da eleicao
#' @param uf uf desejada
#' @param cod_cargo codigo cargo desejado
#' @param n_partido n partido
#' @param n_cand n_cand
#' @param cod_cand cod_cand
#'
#' @return
#' @export
#'
#' @examples
baixa_receitas_cand <- function(ano_eleicao,
                                uf,
                                cod_cargo,
                                n_partido,
                                n_cand,
                                cod_cand){


  url <- paste("https://divulgacandcontas.tse.jus.br/divulga/rest/v1/prestador/consulta/2040602022",
               ano_eleicao,
               uf,
               cod_cargo,
               n_partido,
               n_cand,
               cod_cand,
               sep = "/")

  dados_brutos <- jsonlite::read_json(url)

  df_resumido <-dados_brutos %>%
    tibble::enframe() %>%
    tidyr::spread(name, value) %>%
    janitor::clean_names() %>%
    tidyr::unnest_wider(dados_consolidados) %>%
    tidyr::unnest_wider(despesas) %>%
    dplyr::mutate(
      dplyr::across(id_candidato:numero_de_controle_entrega, as.character)
    ) %>%
    dplyr::mutate(
      dplyr::across(sg_ue:tp_prestador, as.character)
    )

  df_resumido




}
