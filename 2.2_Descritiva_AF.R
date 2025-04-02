############################ DESCRITIVA FINAIS #################################
cat('\014')

# **Anos Finais (AF) ####
if (descritivas == 1) {
  cat('\014')  # Limpa o console
  cat('\nDESCRITIVAS TAXA DE RENDIMENTO // ANOS FINAIS:\n')
  cat('\n============================\n')
  glimpse(txRend_AF)
  
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
  cat('\n========== ESTATÍSTICAS GERAFS ==========\n')
  cat('\nEstatísticas de Aprovados por Ano (GERAL):\n')
  print(calcular_estatisticas(txRend_AF, 'aprovacao_q'))
  
  cat('\nEstatísticas de Abandono por Ano (GERAL):\n')
  print(calcular_estatisticas(txRend_AF, 'abandono_q'))
  
  # ESTADUAL
  cat('\n========== ESTATÍSTICAS ESTADUAFS ==========\n')
  txRend_estadual_AF <- txRend_AF %>% filter(dependencia_id_q == 'Estadual')
  
  cat('\nEstatísticas de Aprovados por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(txRend_estadual_AF, 'aprovacao_q'))
  
  cat('\nEstatísticas de Abandono por Ano (ESTADUAL):\n')
  print(calcular_estatisticas(txRend_estadual_AF, 'abandono_q'))
  
  # MUNICIPAL
  cat('\n========== ESTATÍSTICAS MUNICIPAFS ==========\n')
  txRend_municipal_AF <- txRend_AF %>% filter(dependencia_id_q == 'Municipal')
  
  cat('\nEstatísticas de Aprovados por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(txRend_municipal_AF, 'aprovacao_q'))
  
  cat('\nEstatísticas de Abandono por Ano (MUNICIPAL):\n')
  print(calcular_estatisticas(txRend_municipal_AF, 'abandono_q'))
  
  # Contagem de elementos por ibge_id_q
  cat('\n========== CONTAGEM DE ELEMENTOS POR MUNICÍPIO (IBGE_ID) ==========\n')
  tabela_ibge <- txRend_AF %>%
    group_by(ibge_id_q) %>%
    summarise(quantidade = n()) %>%
    arrange(desc(quantidade))  # Ordena do maior para o menor
  
  print(tabela_ibge, n = nrow(tabela_ibge))
  print(summary(tabela_ibge))
  
  if (imp_categ == 1) {
    # Variáveis categóricas
    cat('\nVARIÁVEIS CATEGÓRICAS (GERAL):\n')
    txRend_AF %>%
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
    print(length(levels(txRend_AF$ibge_id_q)))
    cat('\n|| FIM *** TAXA DE RENDIMENTO *** AF ||')
  } else {
    cat('\n(!) Variáveis categóricas não rodadas.\n')
    cat('\n|| FIM *** TAXA DE RENDIMENTO *** AF ||')
  }
  
} else {
  cat('\014')  # Limpa o console
  cat('\n(!) Descritivas TAXA DE RENDIMENTO AF não rodadas.')
}
