
#' compara_votos_turno
#'
#' @param cargo cargo da disputa
#'
#' @return df comparando os votos no 1 e no 2 turno
#' @export
#'
#' @examples
compara_votos_turno<- function(cargo = "PRESIDENTE"){
  dataset_2_turno <- raspa.divulga.dados.tse::raspa_votacao_g1_br(
                                              turno = 2, cargo = cargo)

  dataset_1_turno_presidente <- raspa.divulga.dados.tse::dataset_1_turno_presidente
  dataset_2_turno <- dataset_2_turno %>%
    dplyr::left_join(
  dataset_1_turno_presidente %>%
    dplyr::select(estado, municipios_municipio, id_candidato, total_percentual_1t = total_percentual,
           total_votos_validos_1t = total_votos_validos,
           municipios_nulos_1t = municipios_nulos,
           municipios_nulos_percentual_1t = municipios_nulos_percentual,
           municipios_id)
  )

  dataset_2_turno
}
