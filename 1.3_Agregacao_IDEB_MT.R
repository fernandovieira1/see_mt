############################ AGREGAÇÃO POR MUNICÍPIOS MT #################################
# Médias das notas por município de MT, matidas as características
# Dados QEdu
cat('\014')

# Realizando a agregação ####
ideb_mun_mt <- ideb %>%
  group_by(ano_q, ibge_id_q, dependencia_id_q, ciclo_id_q) %>%
  summarise(
    media_nota_mt_q = mean(nota_mt_q, na.rm = TRUE),
    media_nota_lp_q = mean(nota_lp_q, na.rm = TRUE),
    media_ideb_q = mean(ideb_q, na.rm = TRUE),
    .groups = 'drop'  # Remover agrupamentos restantes
  )

ideb_mun_mt

# Qtde municípios ####
ideb_mun_mt %>%
  distinct(ibge_id_q) %>%
  nrow()
