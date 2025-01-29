############################ DESCRITIVA FINAIS #################################
# IDEB municípios MT - anos iniciais (AF)
cat('\014')

# **Anos Finais (AF) ####
if (descritivas == 1) {
  cat('\014')  # Limpa o console
  cat('\nDESCRITIVAS IDEB // ANOS FINAIS:\n')
  cat('\n============================\n')
  glimpse(ideb_AF)
  
  # Função auxiliar para cálculo de estatísticas
  calcular_estatisticas <- function(data, prefixo) {
    data %>%
      group_by(ano_q) %>%
      summarise(across(starts_with(prefixo), list(
        Min = ~min(.x, na.rm = TRUE),
        Q1 = ~quantile(.x, 0.25, na.rm = TRUE),
        Media = ~mean(.x, na.rm = TRUE),
        Mediana = ~median(.x, na.rm = TRUE),
        Q3 = ~quantile(.x, 0.75, na.rm = TRUE),
        Max = ~max(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE),
        NAs = ~sum(is.na(.x))
      ), .names = '{.fn}'))
  }
  
  # GERAL
  cat('\n========== ESTATÍSTICAS GERAFS ==========\n')
  cat('\nEstatísticas de Matemática por Ano (GERAL):\n')
  print(calcular_estatisticas(ideb_AF, 'nota_mt_q'))
  
  cat('\nEstatísticas de Português por Ano (GERAL):\n')
  print(calcular_estatisticas(ideb_AF, 'nota_lp_q'))
  
  cat('\nEstatísticas do IDEB por Ano (GERAL):\n')
  print(calcular_estatisticas(ideb_AF, 'ideb_q'))
  
  # ESTADUAL
  cat('\n========== ESTATÍSTICAS ESTADUAFS ==========\n')
  ideb_estadual_AF <- ideb_AF %>% filter(dependencia_id_q == 'Estadual')
  
  cat('\nEstatísticas de Matemática por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(ideb_estadual_AF, 'nota_mt_q'))
  
  cat('\nEstatísticas de Português por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(ideb_estadual_AF, 'nota_lp_q'))
  
  cat('\nEstatísticas do IDEB por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(ideb_estadual_AF, 'ideb_q'))
  
  # MUNICIPAL
  cat('\n========== ESTATÍSTICAS MUNICIPAFS ==========\n')
  ideb_municipal_AF <- ideb_AF %>% filter(dependencia_id_q == 'Municipal')
  
  cat('\nEstatísticas de Matemática por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(ideb_municipal_AF, 'nota_mt_q'))
  
  cat('\nEstatísticas de Português por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(ideb_municipal_AF, 'nota_lp_q'))
  
  cat('\nEstatísticas do IDEB por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(ideb_municipal_AF, 'ideb_q'))
  
  if (imp_categ == 1) {
    # Variáveis categóricas
    cat('\nVARIÁVEIS CATEGÓRICAS (GERAL):\n')
    ideb_AF %>%
      select(where(is.factor)) %>%
      lapply(function(col) {
        abs_freq <- table(col)
        rel_freq <- prop.table(abs_freq) * 100
        sapply(names(abs_freq), function(label) {
          paste0(label, ': ', abs_freq[label], ' (', sprintf('%.2f', rel_freq[label]), '%)')
        })
      }) %>%
      print()
    
    cat('\nQtde. de municípios:\n')
    print(length(levels(ideb_AF$ibge_id_q)))
    cat('\n|| FIM *** IDEB *** AF ||')
  } else {
    cat('\n(!) Variáveis categóricas não rodadas.\n')
    cat('\n|| FIM *** IDEB *** AF ||')
  }
  
} else {
  cat('\014')  # Limpa o console
  cat('\n(!) Descritivas IDEB AF não rodadas.')
}
