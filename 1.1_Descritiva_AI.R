############################ DESCRITIVA INICIAIS #################################
# IDEB municípios MT - anos iniciais (AI)
cat('\014')

# **Anos Iniciais (AI) ####
if (descritivas == 1) {
  cat('\014')  # Limpa o console
  cat('\nDESCRITIVAS IDEB // ANOS INICIAIS:\n')
  cat('\n============================\n')
  glimpse(ideb_AI)
  
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
  cat('\n========== ESTATÍSTICAS GERAIS ==========\n')
  cat('\nEstatísticas de Matemática por Ano (GERAL):\n')
  print(calcular_estatisticas(ideb_AI, 'nota_mt_q'))
  
  cat('\nEstatísticas de Português por Ano (GERAL):\n')
  print(calcular_estatisticas(ideb_AI, 'nota_lp_q'))
  
  cat('\nEstatísticas do IDEB por Ano (GERAL):\n')
  print(calcular_estatisticas(ideb_AI, 'idebq'))
  
  # ESTADUAL
  cat('\n========== ESTATÍSTICAS ESTADUAIS ==========\n')
  ideb_estadual_AI <- ideb_AI %>% filter(dependencia_id_q == 'Estadual')
  
  cat('\nEstatísticas de Matemática por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(ideb_estadual_AI, 'nota_mt_q'))
  
  cat('\nEstatísticas de Português por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(ideb_estadual_AI, 'nota_lp_q'))
  
  cat('\nEstatísticas do IDEB por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(ideb_estadual_AI, 'idebq'))
  
  # MUNICIPAL
  cat('\n========== ESTATÍSTICAS MUNICIPAIS ==========\n')
  ideb_municipal_AI <- ideb_AI %>% filter(dependencia_id_q == 'Municipal')
  
  cat('\nEstatísticas de Matemática por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(ideb_municipal_AI, 'nota_mt_q'))
  
  cat('\nEstatísticas de Português por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(ideb_municipal_AI, 'nota_lp_q'))
  
  cat('\nEstatísticas do IDEB por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(ideb_municipal_AI, 'idebq'))
  
  if (imp_categ == 1) {
    # Variáveis categóricas
    cat('\nVARIÁVEIS CATEGÓRICAS (GERAL):\n')
    ideb_AI %>%
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
    print(length(levels(ideb_AI$ibge_id_q)))
    cat('\n|| FIM *** IDEB *** AI ||')
  } else {
    cat('\n(!) Variáveis categóricas não rodadas.\n')
    cat('\n|| FIM *** IDEB *** AI ||')
  }
  
} else {
  cat('\014')  # Limpa o console
  cat('\n(!) Descritivas IDEB AI não rodadas.')
}
