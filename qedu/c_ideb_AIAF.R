cat('\014')

## Juntar ideb_AI e ideb_AF ####
ideb <- bind_rows(ideb_AI, ideb_AF)

rm(ideb_AI_2015, ideb_AI_2017, ideb_AI_2019, ideb_AI_2021, ideb_AI_2023,
   ideb_AF_2015, ideb_AF_2017, ideb_AF_2019, ideb_AF_2021, ideb_AF_2023)

gc()
cat('\014')

