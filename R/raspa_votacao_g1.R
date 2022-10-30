#' baixa um dataset com os dados de votacao do g1
#'
#' @param turno turno da eleicao
#' @param cargo cargo ou governador ou presidente
#' @param uf uf desejada
#'
#' @return dataset com dados de votacao do g1
#' @export
#'
#' @examples
raspa_votacao_g1 <- function(turno = 1,
         cargo = "GOVERNADOR",
         uf = "SP"){

  url <- paste0("https://s3.glbimg.com/v1/AUTH_fa32656b5fa140c0b66ae29bf6742b13/infografico-g1/2022/",
                turno,
  "-turno/",
  toupper(cargo),
  "_",
  toupper(uf),".JSON")



dataset <- jsonlite::read_json(url) %>%
  tibble::enframe() %>%
  tidyr::spread(name, value) %>%
  tidyr::unnest_wider(atualizacao) %>%
  dplyr::select(-c(1:6)) %>%
  tidyr::unnest_longer(municipios) %>%
  tidyr::unnest_wider(municipios, names_sep = "_") %>% #dplyr::glimpse()
  #dplyr::select(-c(municipios_brancos:municipios_total_percentual)) %>%
  tidyr::unnest_longer(municipios_candidatos) %>%
  tidyr::unnest_wider(municipios_candidatos) %>%
  dplyr::select(-c(candidatos, total,
                   municipios_brancos,municipios_brancos_percentual)) %>%
  dplyr::relocate(id_candidato, .before = municipios_municipio )#%>%
  #dplyr::mutate(total_votos_validos = as.numeric(total_votos_validos))

dataset
}


