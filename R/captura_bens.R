#' captura_bens
#'
#' @param id_candidato string de ids do candidato
#' @param uf uf do candidato
#' @param ano ano da eleicao
#'
#' @return
#' @export
#'
#' @examples
captura_bens <- function(id_candidato,
                       uf = "PE",
                       ano = 2022){
  url <- paste("https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/buscar/",
               ano,
               "/",
               uf,
               "/2040602022/candidato/",
               id_candidato, sep = "")

  dados_brutos <- jsonlite::read_json(url)

  df_final <- dados_brutos %>%
    tibble::enframe() %>%
    tidyr::spread(name, value) %>%
    janitor::clean_names() %>%
    dplyr::select(nome_urna, id, cpf, descricao_cor_raca,
           descricao_estado_civil, descricao_sexo, emails,
           grau_instrucao, numero, nome_coligacao, partido, ocupacao, bens) %>%
    tidyr::unnest_longer(bens) %>%
    tidyr::unnest_wider(bens) %>%
    dplyr::mutate(uf_candidato = uf)  %>% #Alteração aqui
    dplyr::select(-numero) %>%
    tidyr::unnest_wider(partido) %>%
    dplyr::mutate(
      dplyr::across(
        nome_urna:ocupacao, as.character),
    )


  df_final

}
