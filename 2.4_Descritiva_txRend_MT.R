# Remover NaNs e NAs para análise estatística ####
txRend_mt <- txRend_mt %>%
  mutate(
    media_aprovacao_mt_q = ifelse(is.nan(media_aprovacao_mt_q), NA, media_aprovacao_mt_q),
    media_abandono_mt_q = ifelse(is.nan(media_abandono_mt_q), NA, media_abandono_mt_q)
  )

# Resumo estatístico geral ####
stargazer(as.data.frame(txRend_mt), type = "text", summary.stat = c("mean", "sd", "min", "max", "median", "n"))

# Histogramas das taxas de aprovação e abandono ####
p1 <- ggplot(txRend_mt, aes(x = media_aprovacao_mt_q)) + 
  geom_histogram(bins = 30, fill = "steelblue", alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribuição das Taxas de Aprovação", x = "Taxa de Aprovação (%)", y = "Frequência")

p2 <- ggplot(txRend_mt, aes(x = media_abandono_mt_q)) + 
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) + 
  theme_minimal() + 
  labs(title = "Distribuição das Taxas de Abandono", x = "Taxa de Abandono (%)", y = "Frequência")

grid.arrange(p1, p2, ncol = 2)

# Boxplot da taxa de aprovação por dependência ####
ggplot(txRend_mt, aes(x = dependencia_id_q, y = media_aprovacao_mt_q, fill = dependencia_id_q)) +
  geom_boxplot(alpha = 0.6) + 
  theme_minimal() + 
  labs(title = "Comparação das Taxas de Aprovação por Dependência", x = "Tipo de Escola", y = "Taxa de Aprovação (%)")

# Matriz de correlação entre as variáveis numéricas ####
txRend_mt_numeric <- txRend_mt %>% select(media_aprovacao_mt_q, media_abandono_mt_q) %>% drop_na()
ggpairs(txRend_mt_numeric)

# Teste t-student: diferenças entre escolas estaduais e municipais ####
t_test_result <- t.test(media_aprovacao_mt_q ~ dependencia_id_q, data = txRend_mt, na.rm = TRUE)
print(t_test_result)

# Teste ANOVA para analisar variação da taxa de aprovação entre anos ####
anova_model <- aov(media_aprovacao_mt_q ~ ano_q, data = txRend_mt, na.rm = TRUE)
summary(anova_model)

# Identificar diferenças entre os anos ####
TukeyHSD(anova_model)

ggplot(txRend_mt, aes(x = ano_q, y = media_aprovacao_mt_q, fill = dependencia_id_q)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribuição da Taxa de Aprovação por Ano",
       x = "Ano",
       y = "Taxa de Aprovação (%)")



# Evolução da taxa de aprovação ao longo dos anos
ggplot(txRend_mt, aes(x = ano_q, y = media_aprovacao_mt_q, group = dependencia_id_q, color = dependencia_id_q)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Evolução das Taxas de Aprovação ao Longo dos Anos", x = "Ano", y = "Taxa de Aprovação (%)")

# Evolução da taxa de abandono ao longo dos anos
ggplot(txRend_mt, aes(x = ano_q, y = media_abandono_mt_q, group = dependencia_id_q, color = dependencia_id_q)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Evolução das Taxas de Abandono ao Longo dos Anos", x = "Ano", y = "Taxa de Abandono (%)")