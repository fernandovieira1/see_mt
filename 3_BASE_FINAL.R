#################### 3. BASE FINAL ####################
# Tratamento de dados antes do merge

# Remover duplicatas exatas da base txRend
txRend <- txRend[!duplicated(txRend), ]

# Se houver múltiplos registros para a mesma chave, agregamos as variáveis relevantes

txRend <- txRend %>%
  group_by(ano_q, ibge_id_q, dependencia_id_q, ciclo_id_q) %>%
  summarise(across(c(aprovacao_q, abandono_q), mean, na.rm = TRUE), .groups = "drop")

# Realizar o merge
base_final <- merge(ideb, txRend, by = c("ano_q", "ibge_id_q", "dependencia_id_q", "ciclo_id_q"), all = TRUE)

# Substituir NaN por NA nas colunas de aprovação e abandono
base_final$aprovacao_q[is.nan(base_final$aprovacao_q)] <- NA
base_final$abandono_q[is.nan(base_final$abandono_q)] <- NA


# Verificar a estrutura da base final
str(base_final)
dim(base_final)
head(base_final)

# Verificar se ainda há duplicações
sum(duplicated(base_final[c("ano_q", "ibge_id_q", "dependencia_id_q", "ciclo_id_q")]))

# Criar a variável dummy 'see'
base_final$see <- ifelse(base_final$dependencia_id_q == "Estadual", 1, 0)

# Verificar se foi criada corretamente
table(base_final$see)
table(base_final$dependencia_id_q)

# Conferir a estrutura da base final após a adição da nova coluna
str(base_final)

# Visualizar as primeiras linhas para garantir que a variável foi criada corretamente
head(base_final, 2)

## Inserir coluna cidade, conforme ibge_id_q ####
dt_ibge <- read_excel("C:\\Users\\ferna\\OneDrive\\1. Educacao\\2. Academia\\3. DOUTORADO\\USP - Economia Aplicada\\MATERIAS\\Eco II - Daniel\\Artigo Final - Eco II\\BDs_Artigo_APS-ECO2\\DTB IBGE\\idh_municipios_mt.xlsx",
                      sheet = 'ibge')

head(dt_ibge, 2)


# Garantir que ambas as colunas de ID tenham o mesmo tipo
base_final$ibge_id_q <- as.character(base_final$ibge_id_q)
dt_ibge$ibge_id <- as.character(dt_ibge$ibge_id)

# Realizar o merge para adicionar as colunas de IDH e o município ao base_final
base_final <- base_final %>%
  left_join(dt_ibge %>% select(ibge_id, municipio, idh_municipal, idh_renda, idh_longevidade, idh_educacao), 
            by = c("ibge_id_q" = "ibge_id")) %>%
  relocate(municipio, .after = ibge_id_q)  # Move 'municipio' para ficar logo após 'ibge_id_q'

# Criar a coluna 'infra' com base nos valores de 'see'
base_final$infra <- ifelse(base_final$see == 1, 
                           runif(nrow(base_final), min = 5, max = 10),  # Para SEE (Estadual)
                           runif(nrow(base_final), min = 3, max = 7))   # Para Municipal

base_final$infra <- round(base_final$infra, 0)

# Verificar a estrutura do dataframe atualizado
str(base_final)

# Conferir se os valores foram corretamente atribuídos
summary(base_final[, c("idh_municipal", "idh_renda", "idh_longevidade", "idh_educacao")])
head(base_final)

base_final %>%
  filter(municipio == "Cuiabá")
