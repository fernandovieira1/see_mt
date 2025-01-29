cat('\014')

## txRend_AI e txRend_AF ####
txRend <- bind_rows(txRend_AI, txRend_AF)

rm(txRend_AI_2015, txRend_AI_2017, txRend_AI_2019, txRend_AI_2021, txRend_AI_2023,
   txRend_AF_2015, txRend_AF_2017, txRend_AF_2019, txRend_AF_2021, txRend_AF_2023)

gc()
cat('\014')
