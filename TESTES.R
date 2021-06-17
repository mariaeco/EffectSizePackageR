#======================================================================================================================
#       SCRIPT PARA TESTES COM OS DATAFRAMES
#
#======================================================================================================================

# NECESSÁRIO INSTALAR OS SEGUINTES PACOTES
library(simpleboot)# IC calculation
library(boot)
library(ggplot2)
library(RColorBrewer)


# BANCOS DE DADOS PARA TESTE
setwd("E:/ANALISE_DE_DADOS/MEUS ARTIGOS/PaperOmnivoryStabilizesPlankton/FIGURES, SCRIPTS & DATA")

#====================================================================================================================
#EXEMPLO 1 ==========================================================================================================
#====================================================================================================================

dataphyto = read.table("data_phyto.txt", header=T, sep="", na.strings = "NA")
names(dataphyto)

# effect size
result = effectSize(Treatment,treatments=c("O","Z","ON","ZN"),controls=c("C","N"), data=dataphyto)
result 

# mean and confidence interval of the effects
intervals = ciEffect(r=1000,result)  # r is the number of randomizations to do
intervals




#====================================================================================================================
#EXEMPLO 2 ============ COM NAs =====================================================================================
#====================================================================================================================

datazoo = read.table("data_zoo.txt", header=T, sep="", na.strings = NA)
names(datazoo)

# effect size
result = effectSize(Treatment,treatments=c("O","Z"),controls=c("C"), data=datazoo)
result 

# mean and confidence interval of the effects
intervals = ciEffect(r=1000,result)
intervals 
