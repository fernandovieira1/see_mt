############################ DESCRITIVA IDEB #################################
cat('\014')

## Tratamento Prévio ####
# Verificar estrutura dos dados
str(ideb_mun_mt)

# Remover possíveis valores NaN
ideb_mun_mt <- ideb_mun_mt %>%
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), NA, .)))

## Descritivas ####
## Descritiva das variáveis numéricas
describe(ideb_mun_mt %>% 
           select(where(is.numeric)) %>% 
           drop_na())

## Descritiva das variáveis categóricas
ideb_mun_mt %>%
  select(ano_q, ibge_id_q, dependencia_id_q) %>%
  summary()

{
  ## Notas de Matemática por Ano e Dependência Administrativa ####
  # Garantir que ano_q seja um fator para evitar distorções no eixo X
  ideb_mun_mt <- ideb_mun_mt %>%
    mutate(ano_q = factor(ano_q, levels = unique(ano_q)))
  
  # Calcular as medianas por ano e dependência
  medianas_mt <- ideb_mun_mt %>%
    group_by(ano_q, dependencia_id_q) %>%
    summarise(mediana_mt = median(media_nota_mt_q, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = dependencia_id_q, values_from = mediana_mt) %>%
    mutate(diferenca = Estadual - Municipal)
  
  medianas_lp <- ideb_mun_mt %>%
    group_by(ano_q, dependencia_id_q) %>%
    summarise(mediana_lp = median(media_nota_lp_q, na.rm = TRUE), .groups = 'drop') %>%
    pivot_wider(names_from = dependencia_id_q, values_from = mediana_lp) %>%
    mutate(diferenca = Estadual - Municipal)
  
  # Ajustar valores máximos para exibir os rótulos das diferenças de medianas
  y_max_mt <- max(ideb_mun_mt$media_nota_mt_q, na.rm = TRUE) + 1
  y_max_lp <- max(ideb_mun_mt$media_nota_lp_q, na.rm = TRUE) + 1
  
  # Gráfico para Matemática
  notas_mt_ano <- ggplot(ideb_mun_mt, aes(x = ano_q, y = media_nota_mt_q, fill = dependencia_id_q)) +
    geom_boxplot(alpha = 0.6) +
    geom_text(data = medianas_mt, aes(x = ano_q, y = y_max_mt, label = paste0('Δ', round(diferenca, 2))),
              inherit.aes = FALSE, color = 'black', fontface = "plain", vjust = -0.2, size = 3) +  # Reduz fonte e remove negrito
    theme_minimal() +
    labs(x = '', y = 'Matemática') +
    theme(
      axis.text = element_text(face = "bold"),   # Eixos X e Y em negrito
      axis.title.y = element_text(face = "bold") # Rótulo do eixo Y em negrito
    ) +
    scale_fill_manual(name = "", values = c('Estadual' = '#0154a4', 'Municipal' = '#e966ac'))  # Remove título da legenda
  
  # Gráfico para Português
  notas_lp_ano <- ggplot(ideb_mun_mt, aes(x = ano_q, y = media_nota_lp_q, fill = dependencia_id_q)) +
    geom_boxplot(alpha = 0.6) +
    geom_text(data = medianas_lp, aes(x = ano_q, y = y_max_lp, label = paste0('Δ', round(diferenca, 2))),
              inherit.aes = FALSE, color = 'black', fontface = "plain", vjust = -0.2, size = 3) +  # Reduz fonte e remove negrito
    theme_minimal() +
    labs(x = '', y = 'Português') +
    theme(
      axis.text = element_text(face = "bold"),   # Eixos X e Y em negrito
      axis.title.y = element_text(face = "bold") # Rótulo do eixo Y em negrito
    ) +
    scale_fill_manual(name = "", values = c('Estadual' = '#0154a4', 'Municipal' = '#e966ac'))  # Remove título da legenda
  }
# Exibir os gráficos lado a lado
grid.arrange(notas_mt_ano, notas_lp_ano, nrow = 2, ncol = 1)




## Histograma das notas de matemática ####
ggplot(ideb_mun_mt, aes(x = media_nota_mt_q, fill = dependencia_id_q)) +
  geom_histogram(alpha = 0.6, bins = 30, position = 'dodge') +
  theme_minimal() +
  labs(title = 'Histograma das Notas de Matemática', x = 'Nota', y = 'Frequência')

## Teste de normalidade das notas (Shapiro-Wilk) ####
shapiro.test(na.omit(ideb_mun_mt$media_nota_mt_q))
shapiro.test(na.omit(ideb_mun_mt$media_nota_lp_q))
# Os dados não são normais

## Teste t para comparação de médias entre escolas estaduais e municipais ####
t.test(media_nota_mt_q ~ dependencia_id_q, data = ideb_mun_mt)
t.test(media_nota_lp_q ~ dependencia_id_q, data = ideb_mun_mt)
# Não é possível afirmar que as médias são diferentes

## Matriz de correlação entre as variáveis numéricas ####
cor_matrix <- cor(ideb_mun_mt %>% select(where(is.numeric)), use = 'complete.obs')
print(cor_matrix)

## Gráfico de dispersão entre matemática e português por dependência ####
ggplot(ideb_mun_mt, aes(x = media_nota_mt_q, y = media_nota_lp_q, color = dependencia_id_q)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = 'lm', se = FALSE) +
  theme_minimal() +
  labs(title = 'Relação entre Notas de Matemática e Português', x = 'Nota de Matemática', y = 'Nota de Português')


{
  # Função para gerar gráficos filtrando por ciclo
  plot_ideb <- function(data, ciclo_filtro, titulo) {
    ggplot(data %>% filter(ciclo_id_q %in% ciclo_filtro), 
           aes(x = ano_q, y = media_ideb_q, color = dependencia_id_q, group = dependencia_id_q)) +
      geom_point(alpha = 0.7) +  
      geom_line(stat = "summary", fun = mean, size = 1) +  
      geom_text(stat = "summary", fun = mean, 
                aes(label = round(..y.., 2)), 
                data = data %>% filter(ciclo_id_q %in% ciclo_filtro, dependencia_id_q == "Municipal"),
                vjust = -1, hjust = 1.2, angle = 0, 
                size = 3, fontface = "plain", color = "#e966ac") +  # Reduz fonte e remove negrito
      geom_text(stat = "summary", fun = mean, 
                aes(label = round(..y.., 2)), 
                data = data %>% filter(ciclo_id_q %in% ciclo_filtro, dependencia_id_q == "Estadual"),
                vjust = 1.5, hjust = 1.2, angle = 0, 
                size = 3, fontface = "plain", color = "#0154a4") +  # Reduz fonte e remove negrito
      theme_minimal() +
      labs(title = titulo, x = "", y = "") +
      scale_color_manual(name = "", values = c("Estadual" = "#0154a4", "Municipal" = "#e966ac")) +  # Remove o título da legenda
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            legend.position = "bottom",
            axis.text.x = element_text(face = "bold"),  # Eixo X em negrito
            plot.title = element_text(size = rel(0.8)))  # Reduz o tamanho do título em 40%
  }
  
  # Criar os três gráficos
  grafico_todos <- plot_ideb(ideb_mun_mt, c("AI", "AF"), "Anos Iniciais e Finais")
  grafico_ai <- plot_ideb(ideb_mun_mt, "AI", "Anos Iniciais")
  grafico_af <- plot_ideb(ideb_mun_mt, "AF", "Anos Finais")
}
# Criar a grid 3x1
grid.arrange(grafico_todos, grafico_ai, grafico_af, ncol = 1)




# Resultados diferentes do que o governo de MT fala:
# https://www.gazetadigital.com.br/editorias/cidades/educacao-de-mato-grosso-alcanca-8-melhor-resultado-do-pais/780625