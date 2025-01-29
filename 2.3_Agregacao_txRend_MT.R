############################ AGREGAÇÃO POR MUNICÍPIOS MT #################################
# Médias das aprovações e abandonos por município de MT, matidas as características
# Dados QEdu
cat('\014')

# Realizando a agregação ####
txRend_mt <- txRend %>%
  group_by(ano_q, ibge_id_q, dependencia_id_q, ciclo_id_q) %>%
  summarise(
    media_aprovacao_mt_q = mean(aprovacao_q, na.rm = TRUE),
    media_abandono_mt_q = mean(abandono_q, na.rm = TRUE),
    .groups = 'drop'  # Remover agrupamentos restantes
  )

txRend_mt
summary(txRend_mt)

# Qtde municípios ####
txRend %>%
  distinct(ibge_id_q) %>%
  nrow()
