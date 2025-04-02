############################ DESCRITIVA INICIAIS #################################
cat('\014')

# **Anos Iniciais (AI) ####
if (descritivas == 1) {
  cat('\014')  # Limpa o console
  cat('\nDESCRITIVAS TAXA DE RENDIMENTO // ANOS INICIAIS:\n')
  cat('\n============================\n')
  glimpse(txRend_AI)
  
  # Função auxiliar para cálculo de estatísticas
  calcular_estatisticas <- function(data, coluna) {
    data %>%
      group_by(ano_q) %>%
      summarise(
        Min = min(.data[[coluna]], na.rm = TRUE),
        Q1 = quantile(.data[[coluna]], 0.25, na.rm = TRUE),
        Media = mean(.data[[coluna]], na.rm = TRUE),
        Mediana = median(.data[[coluna]], na.rm = TRUE),
        Q3 = quantile(.data[[coluna]], 0.75, na.rm = TRUE),
        Max = max(.data[[coluna]], na.rm = TRUE),
        sd = sd(.data[[coluna]], na.rm = TRUE),
        NAs = sum(is.na(.data[[coluna]]))
      )
  }
  
  # GERAL
  cat('\n========== ESTATÍSTICAS GERAIS ==========\n')
  cat('\nEstatísticas de Aprovados por Ano (GERAL):\n')
  print(calcular_estatisticas(txRend_AI, 'aprovacao_q'))
  
  cat('\nEstatísticas de Abandono por Ano (GERAL):\n')
  print(calcular_estatisticas(txRend_AI, 'abandono_q'))
  
  # ESTADUAL
  cat('\n========== ESTATÍSTICAS ESTADUAIS ==========\n')
  txRend_estadual_AI <- txRend_AI %>% filter(dependencia_id_q == 'Estadual')
  
  cat('\nEstatísticas de Aprovados por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(txRend_estadual_AI, 'aprovacao_q'))
  
  cat('\nEstatísticas de Abandono por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(txRend_estadual_AI, 'abandono_q'))
  
  # MUNICIPAL
  cat('\n========== ESTATÍSTICAS MUNICIPAIS ==========\n')
  txRend_municipal_AI <- txRend_AI %>% filter(dependencia_id_q == 'Municipal')
  
  cat('\nEstatísticas de Aprovados por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(txRend_municipal_AI, 'aprovacao_q'))
  
  cat('\nEstatísticas de Abandono por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(txRend_municipal_AI, 'abandono_q'))
  
  # Contagem de elementos por ibge_id_q
  cat('\n========== CONTAGEM DE ELEMENTOS POR MUNICÍPIO (IBGE_ID) ==========\n')
  tabela_ibge <- txRend_AI %>%
    group_by(ibge_id_q) %>%
    summarise(quantidade = n()) %>%
    arrange(desc(quantidade))  # Ordena do maior para o menor
  
  print(tabela_ibge, n = nrow(tabela_ibge))
  print(summary(tabela_ibge))
  
  if (imp_categ == 1) {
    # Variáveis categóricas
    cat('\nVARIÁVEIS CATEGÓRICAS (GERAL):\n')
    txRend_AI %>%
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
    print(length(levels(txRend_AI$ibge_id_q)))
    cat('\n|| FIM *** TAXA DE RENDIMENTO *** AI ||')
  } else {
    cat('\n(!) Variáveis categóricas não rodadas.\n')
    cat('\n|| FIM *** TAXA DE RENDIMENTO *** AI ||')
  }
  
} else {
  cat('\014')  # Limpa o console
  cat('\n(!) Descritivas TAXA DE RENDIMENTO AI não rodadas.')
}
