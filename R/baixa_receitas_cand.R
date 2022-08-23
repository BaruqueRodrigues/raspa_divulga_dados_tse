#' baixa_receitas_candidatos
#'
#' @param ano_eleicao ano da eleicao
#' @param uf uf desejada
#' @param cod_cargo codigo cargo desejado
#'
#' @return
#' @export
#'
#' @examples
baixa_receitas_cand <- function(ano_eleicao,
                                uf,
                                cod_cargo){

  df <- pega_info_cand(ano = ano_eleicao,
                       sigla_estado = uf,
                       cod_cargo = cod_cargo) %>%
    dplyr::select(ano_eleicao = ano_eleicao,
                  uf = estado,
                  cod_cargo = codigo,
                  n_partido = numero_urna,
                  n_cand = numero_urna,
                  cod_cand = id,
                  nome_urna) %>%
    dplyr::mutate(n_partido = stringr::str_extract(n_partido, "([1-9][0-9])"),
                  cod_cand = as.character(cod_cand))

  pega_url <- function(ano_eleicao,
                       uf,
                       cod_cargo,
                       n_partido,
                       n_cand,
                       cod_cand,
                       sep){

    lista_url <- paste("https://divulgacandcontas.tse.jus.br/divulga/rest/v1/prestador/consulta/2040602022",
                       ano_eleicao,
                       uf,
                       cod_cargo,
                       n_partido,
                       n_cand,
                       cod_cand,
                       sep = "/")
    lista_url
  }

  lista_url <- df %>%
    pega_url(ano_eleicao = df$ano_eleicao,
             uf = df$uf,
             cod_cargo = df$cod_cargo,
             n_partido = df$n_partido,
             n_cand = df$n_cand,
             cod_cand = df$cod_cand)

  trata_dados <- function(l_url){
    df_tratado <- l_url %>%
      jsonlite::read_json() %>%
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


  }
  dados_final <- lista_url %>%
    purrr::map_dfr(~trata_dados(.))

  dados_final <- dados_final %>%
    dplyr::left_join(df %>% dplyr::select(cod_cand, nome_urna),
                     by = c("id_candidato" = "cod_cand")) %>%
    dplyr::relocate(nome_urna, .before = ano) %>%
    dplyr::relocate(id_candidato, .after = nome_urna) %>%
    dplyr::mutate(
      dplyr::across(ano:cnpj, as.character)
    )

  dados_final

}
