# linkage
#SCRIPT LINKAGE SINAN E PEP - TUBERCULOSE
#JULIA OLIVEIRA COMONIAN
#ADPTADO DE: https://wtassinari.github.io/linkage/exemplo/exemplo.html#vinculando-as-bases-utilizando-o-hash
#21/08/2024
#-------------------------------------------------------------------------------

# Instale o pacote se ele n?o estiver instalado
install.packages("fasteR")
install.packages("RecordLinkage")
install.packages("tidyverse")
install.packages("DT")
install.packages("digest")
install.packages("stringdist")
install.packages("readxl")
install.packages("reclin2")
install.packages("lava")
install.packages("Rtools")
install.packages("stringdist")
install.packages("dplyr")

# Carregue o pacote
library(dplyr)
library(fasteR)
library(RecordLinkage)
library(lava)
library(tidyverse)
library(DT)
library(digest)
library(stringdist)
library(readxl)
library(reclin2)

#LIMPEZA DE BANCO---------------------------------------------------------------
limpa_str <- function(x) {
  # Converter para UTF-8 
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT//IGNORE")
  # Remover acentos
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  # Converter para caixa alta (mai?sculas)
  x <- toupper(x)
  # Substituir "DESCONHECIDO" por um registro em branco
  x <- gsub("DESCONHECIDO", "", x, ignore.case = TRUE)
  # Remover caracteres n?o alfanum?ricos
  x <- gsub("[^[:alnum:]]", " ", x)
  # Remover preposi??es
  x <- gsub("\\bE\\b|\\bDO\\b|\\bDA\\b|\\bDOS\\b|\\bDAS\\b|\\bDE\\b", "", x, ignore.case = TRUE)
  # Remover espa?os extras
  x <- gsub("\\s+", " ", x)
  # Remover espa?os no in?cio e no fim da string
  x <- trimws(x)
  
  return(x)
}

#IMPORTANDO BANCOS--------------------------------------------------------------
SEAPAT <- read_excel("SEAPAT_21082024.xls")

SEAPPR <- read_excel("SEAPPR_21082024.xls")

TUBENET<- read_excel("TUBENET_27092024_PRISIONAL.xlsx")

#------------------#ORGANIZAR BANCO SINAN------------------------------------------

print(names(TUBENET))

TUBENET <- TUBENET %>%
  mutate(TRATAMENTO = recode(TRATAMENTO, '1' = "Caso Novo", '2' = "Recidiva",
                             '3' = "Reingresso após abandono", '4' = "Não sabe",
                             '5' = "Transferência", '6' = "Pós-óbito"))
TUBENET <- TUBENET %>%
  mutate(FORMA = recode(FORMA, '1' = "Pulmonar", '2' = "Extrapulmonar",
                        '3' = "Pulmonar + Extrapulmonar"))

TUBENET <- TUBENET %>%
  mutate(HIV = recode(HIV, '1' = "Positivo", '2' = "Negativo",
                      '3' = "Em andamento", '4' = "Não realizado"))

TUBENET <- TUBENET %>%
  mutate(SITUA_ENCE = recode(SITUA_ENCE, '1' = "Cura", '2' = "Abandono",
                             '3' = "Óbito por TB", '4' = "Óbito por outras causas",
                             '5' = "Transferência", '6' = "Mudança de diagnóstico",
                             '7' = "TB-DR", '8' = "Mudança de esquema", '9' = "Falência",
                             '10' = "Abandono Primário"))

#CRIANDO COLUNA CONFIRMACAO LABORATORIAL
TUBENET$CONF_LAB <- ifelse(
  TUBENET$BACILOSC_E == 1 | 
    TUBENET$CULTURA_ES == 1 | 
    TUBENET$TEST_MOLEC %in% c(1, 2), 
  "SIM", 
  "NÃO")

TUBENET <- TUBENET %>%
  mutate(BACILOSC_E = recode(BACILOSC_E, '1' = "Positiva", '2' = "Negativa",
                             '3' = "Não realizada", '4' = "Não se aplica"))
TUBENET <- TUBENET %>%
  mutate(CULTURA_ES = recode(CULTURA_ES, '1' = "Positiva", '2' = "Negativa",
                             '3' = "Em andamento", '4' = "Não realizada"))
TUBENET <- TUBENET %>%
  mutate(TEST_MOLEC = recode(TEST_MOLEC, '1' = "Detectávelsensível à Rifampicina", 
                             '2' = "Detectável resistente à Rifampicina ",
                             '3' = "Não detectável", '4' = "Inconclusivo",
                             '5' = " Não realizado"))



#UNINDO CADASTROS ATIVOS--------------------------------------------------------
#ORGANIZANDO BANCOS
SEAPAT <- SEAPAT[-c(1:2), ]
colnames(SEAPAT) <- as.character(SEAPAT[1, ])
SEAPAT <- SEAPAT[-1, ]

SEAPPR <- SEAPPR[-c(1:2), ]
colnames(SEAPPR) <- as.character(SEAPPR[1, ])
SEAPPR <- SEAPPR[-1, ]
#APARTIR DAQUI
SEAPBM_22082024 <- SEAPBM_22082024[-c(1:2), ]
colnames(SEAPBM_22082024) <- as.character(SEAPBM_22082024[1, ])
SEAPBM_22082024 <- SEAPBM_22082024[-1, ]

SEAPBS_22082024 <- SEAPBS_22082024[-c(1:2), ]
colnames(SEAPBS_22082024) <- as.character(SEAPBS_22082024[1, ])
SEAPBS_22082024 <- SEAPBS_22082024[-1, ]

SEAPDO_22082024 <- SEAPDO_22082024[-c(1:2), ]
colnames(SEAPDO_22082024) <- as.character(SEAPDO_22082024[1, ])
SEAPDO_22082024 <- SEAPDO_22082024[-1, ]

SEAPEB_22082024<- SEAPEB_22082024[-c(1:2), ]
colnames(SEAPEB_22082024) <- as.character(SEAPEB_22082024[1, ])
SEAPEB_22082024 <- SEAPEB_22082024[-1, ]

SEAPGC_22082024<- SEAPGC_22082024[-c(1:2), ]
colnames(SEAPGC_22082024) <- as.character(SEAPGC_22082024[1, ])
SEAPGC_22082024 <- SEAPGC_22082024[-1, ]

SEAPJL_22082024 <- SEAPJL_22082024[-c(1:2), ]
colnames(SEAPJL_22082024) <- as.character(SEAPJL_22082024[1, ])
SEAPJL_22082024 <- SEAPJL_22082024[-1, ]

SEAPJS_22082024 <- SEAPJS_22082024[-c(1:2), ]
colnames(SEAPJS_22082024) <- as.character(SEAPJS_22082024[1, ])
SEAPJS_22082024 <- SEAPJS_22082024[-1, ]

SEAPNH_22082024 <- SEAPNH_22082024[-c(1:2), ]
colnames(SEAPNH_22082024) <- as.character(SEAPNH_22082024[1, ])
SEAPNH_22082024 <- SEAPNH_22082024[-1, ]

SEAPPC_22082024 <- SEAPPC_22082024[-c(1:2), ]
colnames(SEAPPC_22082024) <- as.character(SEAPPC_22082024[1, ])
SEAPPC_22082024 <- SEAPPC_22082024[-1, ]

SEAPSN_22082024 <- SEAPSN_22082024[-c(1:2), ]
colnames(SEAPSN_22082024) <- as.character(SEAPSN_22082024[1, ])
SEAPSN_22082024 <- SEAPSN_22082024[-1, ]

SEAPTB_22082024 <- SEAPTB_22082024[-c(1:2), ]
colnames(SEAPTB_22082024) <- as.character(SEAPTB_22082024[1, ])
SEAPTB_22082024 <- SEAPTB_22082024[-1, ]

SEAPVP_22082024 <- SEAPVP_22082024[-c(1:2), ]
colnames(SEAPVP_22082024) <- as.character(SEAPVP_22082024[1, ])
SEAPVP_22082024 <- SEAPVP_22082024[-1, ]

X1_1_ <- X1_1_[-c(1:2), ]
colnames(X1_1_) <- as.character(X1_1_[1, ])
X1_1_ <- X1_1_[-1, ]

X1_2_ <- X1_2_[-c(1:2), ]
colnames(X1_2_) <- as.character(X1_2_[1, ])
X1_2_ <- X1_2_[-1, ]

X1_3_ <- X1_3_[-c(1:2), ]
colnames(X1_3_) <- as.character(X1_3_[1, ])
X1_3_ <- X1_3_[-1, ]

X1_4_ <- X1_4_[-c(1:2), ]
colnames(X1_4_) <- as.character(X1_4_[1, ])
X1_4_ <- X1_4_[-1, ]

X1_5_ <- X1_5_[-c(1:2), ]
colnames(X1_5_) <- as.character(X1_5_[1, ])
X1_5_ <- X1_5_[-1, ]

X1_6_ <- X1_6_[-c(1:2), ]
colnames(X1_6_) <- as.character(X1_6_[1, ])
X1_6_ <- X1_6_[-1, ]

X1_7_ <- X1_7_[-c(1:2), ]
colnames(X1_7_) <- as.character(X1_7_[1, ])
X1_7_ <- X1_7_[-1, ]

X1_8_ <- X1_8_[-c(1:2), ]
colnames(X1_8_) <- as.character(X1_8_[1, ])
X1_8_ <- X1_8_[-1, ]

X1_9_ <- X1_9_[-c(1:2), ]
colnames(X1_9_) <- as.character(X1_9_[1, ])
X1_9_ <- X1_9_[-1, ]

X1_10_ <- X1_10_[-c(1:2), ]
colnames(X1_10_) <- as.character(X1_10_[1, ])
X1_10_ <- X1_10_[-1, ]

X1_12_ <- X1_12_[-c(1:2), ]
colnames(X1_12_) <- as.character(X1_12_[1, ])
X1_12_ <- X1_12_[-1, ]

X1_13_ <- X1_13_[-c(1:2), ]
colnames(X1_13_) <- as.character(X1_13_[1, ])
X1_13_ <- X1_13_[-1, ]

# Combinar os dois data frames em um Unico data frame
LISTA_TB <- rbind(SEAPAT, SEAPPR,SEAPBM_22082024, SEAPBS_22082024, SEAPDO_22082024, 
                  SEAPEB_22082024,SEAPGC_22082024, SEAPJL_22082024, SEAPJS_22082024, 
                  SEAPNH_22082024, SEAPPC_22082024, SEAPSN_22082024,SEAPTB_22082024,
                  SEAPVP_22082024,X1_1_,X1_2_,X1_3_,X1_4_,X1_5_,X1_6_,X1_7_,X1_8_,
                  X1_9_,X1_10_,X1_12_,X1_13_)
names(LISTA_TB)

#ALTERANDO NOME DAS VARI?VEIS
# Renomeando as 6 primeiras variáveis diretamente
names(LISTA_TB)[1:6] <- c("UNIDADE","NPRONT","NU_NOTIFIC","NM_PACIENT","DT_NASC","IDADE")

#############RODANDO A FUNCAO LIMPEZA-------------------------------------------------------
LISTA_TB$NM_PACIENT <- limpa_str(LISTA_TB$NM_PACIENT)
TUBENET$NM_PACIENT <- limpa_str(TUBENET$NM_PACIENT)

#TRANSFORMANDO DATA
LISTA_TB$DT_NASC <- as.character(LISTA_TB$DT_NASC)
TUBENET$DT_NASC <- as.character(TUBENET$DT_NASC)

#REMOVENDO DUPLICADOS
LISTA_TB <- distinct(LISTA_TB)
TUBENET <- distinct(TUBENET)

#PREPARANDO BANCO TUBENET
# Substituir NAs por strings vazias
TUBENET$NM_PACIENT[is.na(TUBENET$NM_PACIENT)] <- ""
TUBENET$NM_MAE_PAC[is.na(TUBENET$NM_MAE_PAC)] <- ""
TUBENET$CS_SEXO[is.na(TUBENET$CS_SEXO)] <- ""
TUBENET$DT_NASC[is.na(TUBENET$DT_NASC)] <- ""
# Converter colunas para caracteres, se necess?rio
TUBENET$NM_PACIENT <- as.character(TUBENET$NM_PACIENT)
TUBENET$NM_MAE_PAC <- as.character(TUBENET$NM_MAE_PAC)
TUBENET$CS_SEXO <- as.character(TUBENET$CS_SEXO)
TUBENET$DT_NASC <- as.character(TUBENET$DT_NASC)
# Certifique-se de que a coluna 'DT_NASC' est? no formato Date
TUBENET$DT_NASC <- as.Date(TUBENET$DT_NASC, format = "%Y-%m-%d")

# Converter a data para o formato de string desejado (por exemplo, DD/MM/YYYY)
TUBENET$DT_NASC <- format(TUBENET$DT_NASC, "%d/%m/%Y")
#ARRUMANDO SEXO
TUBENET <- TUBENET %>%
  mutate(CS_SEXO = recode(CS_SEXO,
                          "Feminino" = "F",
                          "Masculino" = "M"))

#LINKAGE1 - SINAN---------------------------------------------------------------

# Vinculando os dois data frames com base na coluna NUM DE NOTIFIC
LINKAGE1_SINAN <- merge(LISTA_TB, TUBENET, by = "NU_NOTIFIC")
NAO_LOCALIZADOS <- anti_join(LISTA_TB, LINKAGE1_SINAN, by = "NU_NOTIFIC")
NAO_LOCALIZADOS2 <- anti_join(TUBENET, LINKAGE1_SINAN, by = "NU_NOTIFIC")
# Exibindo as dimens?es do data frame resultante
print(dim(LINKAGE1_SINAN))

# Exibindo uma tabela interativa para visualizar os dados (primeiras 5 linhas por p?gina)
datatable(LINKAGE1_SINAN, options = list(pageLength = 5))

#ORGANIZAR A TABELA-------------------------------------------------------------
library(data.table)

# Converter o data frame para data.table
LINKAGE1_SINAN <- as.data.table(LINKAGE1_SINAN)

# Reorganizar as colunas
LINKAGE1_SINAN <- LINKAGE1_SINAN[, c("NM_PACIENT.x", "NM_PACIENT.y", "SITUA_ENCE", "Motivo do encerramento","UNIDADE","NM_COMPLEM","HIV.x", "HIV.y",
                                   setdiff(names(LINKAGE1_SINAN), c("NM_PACIENT.x", "NM_PACIENT.y", "SITUA_ENCE", "Motivo do encerramento","UNIDADE","NM_COMPLEM","HIV.x", "HIV.y"))), with = FALSE]

# Verificar a nova ordem das colunas
names(LINKAGE1_SINAN)

#LINKAGE COM CHAVE SLK----------------------------------------------------------
LISTA_TB$hash <- paste0(LISTA_TB$NM_PACIENT, LISTA_TB$IDADE)
LISTA_TB$hash <- sapply(LISTA_TB$hash, digest, algo="md5")

# Criar o hash
TUBENET$hash <- paste0(TUBENET$NM_PACIENT, TUBENET$IDADE)
TUBENET$hash <- sapply(TUBENET$hash, digest, algo = "md5")
head(TUBENET$hash)
# Vinculando os dois data frames com base na coluna 'hash'
LINKAGE2_HASH <- merge(LISTA_TB, TUBENET, by = "hash")

# Exibindo as dimens?es do data frame resultante
print(dim(LINKAGE2_HASH))

# Exibindo uma tabela interativa para visualizar os dados (primeiras 5 linhas por p?gina)
datatable(LINKAGE2_HASH, options = list(pageLength = 5))
names(LINKAGE2_HASH)

# Converter o data frame para data.table
LINKAGE2_HASH <- as.data.table(LINKAGE2_HASH)

# Reorganizar as colunas
LINKAGE2_HASH <- LINKAGE2_HASH[, c("NM_PACIENT.x", "NM_PACIENT.y", "SITUA_ENCE", "Motivo do encerramento", "UNIDADE","NM_COMPLEM","HIV.x", "HIV.y","NU_NOTIFIC.x","NU_NOTIFIC.y",
                                     setdiff(names(LINKAGE2_HASH), c("NM_PACIENT.x", "NM_PACIENT.y", "SITUA_ENCE", "Motivo do encerramento", "UNIDADE","NM_COMPLEM","HIV.x", "HIV.y","NU_NOTIFIC.x","NU_NOTIFIC.y"))), with = FALSE]

# Verificar a nova ordem das colunas
names(LINKAGE2_HASH)

# Substituir "-" por NA na coluna NU_NOTIFIC
LINKAGE2_HASH$NU_NOTIFIC.x[LINKAGE2_HASH$NU_NOTIFIC.x == "-"] <- NA

# Selecionar apenas as linhas onde NU_NOTIFIC é NA
LINKAGE2_HASH <- LINKAGE2_HASH[is.na(LINKAGE2_HASH$NU_NOTIFIC.x), ]

#salvando
library(openxlsx) 
library(writexl)

write_xlsx(LINKAGE2_HASH, path = "LINKAGE2_HASH.xlsx")
write_xlsx(LINKAGE1_SINAN, path = "LINKAGE1_SINAN.xlsx")
write_xlsx(NAO_LOCALIZADOS, path = "NAO_LOCALIZADOS.xlsx")


