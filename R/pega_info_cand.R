#' pega info candidatos
#'
#' @param ano ano eleicao
#' @param sigla_estado sigla uf
#' @param cod_cargo codigo cargo
#'
#' @return
#' @export
#'
#' @examples
pega_info_cand <- function(ano = 2022,
                           sigla_estado = "PE",
                           cod_cargo = 7){
  url <- paste(
    "https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/listar",
    ano,
    sigla_estado,
    "2040602022",
    cod_cargo,
    "candidatos",
    sep = "/"
  )

  dados_brutos <- jsonlite::read_json(url)

  dados <- dados_brutos$candidatos %>%
    dplyr::tibble(data =.) %>%
    tidyr::unnest_wider(data) %>%
    janitor::clean_names() %>%
    dplyr::select(id, nome_urna, numero, id_candidato_superior,
           cpf, descricao_cor_raca, descricao_situacao, grau_instrucao,
           ocupacao, nome_coligacao, cargo, partido, eleicoes_anteriores, bens, total_de_bens) %>%
    tidyr::unnest_wider(cargo) %>%
    dplyr::rename(numero_urna = numero, desc_cargo = nome, sigla_a = sigla) %>%
    tidyr::unnest_wider(partido) %>%
    dplyr::rename(numero_partido = numero, sigla_partido = sigla, nome_partido = nome) %>%
    janitor::clean_names() %>%
    dplyr::mutate(ano_eleicao = ano,
           estado = sigla_estado)

  message("dados da UF: ", sigla_estado, " baixados")

  dados


}
