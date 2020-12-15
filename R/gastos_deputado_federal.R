
library(tidyverse)
library(readxl)
library(ggplot2)


df <- read.csv("C:\\Users\\mcaio\\Desktop\\Nova pasta\\Ano-2019.csv", sep = ";", encoding = "UTF-8")

df2 <- df %>% rename(NomeParlamentar = X.U.FEFF.txNomeParlamentar,
                     Fornecedor = txtFornecedor,
                     Descrição = txtDescricao,
                     Valor_liquido = vlrLiquido,
                     Valor_glosa = vlrGlosa,
                     Valor_documento = vlrDocumento,
                     Data_emissão = datEmissao)

df <- df %>% filter(!is.na(cpf))

#Gastos por partido
gasto_partido <- df2 %>%
  filter(numMes > 1 & !is.na(cpf)) %>%
  group_by(sgPartido) %>%
  summarize(valor_milhoes = sum(Valor_liquido)/1e06) %>%
  arrange(desc(valor_milhoes)) %>%
  mutate(percentual = round(valor_milhoes/sum(valor_milhoes)*100,2)) %>%
  rename(partido = sgPartido)


# gráfico dos partidos que mais gastaram
gasto_partido %>%
  ggplot(aes(fct_reorder(partido, valor_milhoes), valor_milhoes)) +
  geom_col()+
  coord_flip()+
  theme_classic()+
  ylab("VALOR GASTO EM MILHÕES")+
  xlab("")+
  geom_label(aes(label = round(valor_milhoes,0)))+
  ggtitle("PARTIDOS QUE MAIS GASTARAM NO ANO DE 2019")


#gastos por atividades
gasto_atividades <-  df2 %>% filter(!Descrição == "PASSAGEM AÉREA - SIGEPA" & numMes > 1 & !is.na(cpf)) %>%
  group_by(Descrição) %>%
  summarize(valor = sum(Valor_liquido)/1e06) %>%
  arrange(desc(valor))


#grafico gastos por atividades
gasto_atividades %>%
  ggplot(aes(fct_reorder(Descrição, valor), valor))+
  geom_col()+
  coord_flip()+
  geom_label(aes(label = round(valor,0)))+
  theme_classic()+
  ylab("ATIVIDADES")+
  xlab("VALOR GASTO(MILHÕES)")+
  ggtitle("GASTOS PARLAMENTARES EM 2019 POR ATIVIDADES")





df2 %>%  filter(numMes > 1) %>%
  group_by(NomeParlamentar) %>%
  summarize(valor_milhoes = sum(Valor_liquido)/1e03) %>%
  arrange(desc(valor_milhoes)) %>%
  top_n(10, valor_milhoes) %>%
  ggplot(aes(fct_reorder(NomeParlamentar, valor_milhoes), valor_milhoes))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  geom_label(aes(label = round(valor_milhoes,0)))+
  xlab("VALOR GASTO(MIL REAIS)")+
  ylab("PARLAMENTAR")+
  ggtitle("OS 10 DEPUTADOS FEDERAIS QUE MAIS GASTARAM EM 2019")


gastos_deputados<- df2 %>%
  filter(numMes > 1 & !is.na(cpf)) %>%
  group_by(sgPartido, NomeParlamentar) %>%
  summarize(valor = sum(Valor_liquido/1e03)) %>%
  arrange(desc(valor)) %>%
  select(NomeParlamentar, sgPartido, valor)

gastos_deputados %>%
  filter(valor > 485.0000)%>%
  ggplot(aes(fct_reorder(NomeParlamentar, valor), valor, fill = sgPartido))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  geom_label(aes(label = round(valor,0)))+
  xlab("")+
  ylab("VALOR GASTO(MIL REAIS)")+
  ggtitle("OS 10 DEPUTADOS FEDERAIS QUE MAIS GASTARAM EM 2019")


# GRAFICO 10 DEPUTADOS QUE MENOS GASTARAM EM 2019

gastos_deputados %>%
  filter(valor < 10.000) %>%
  ggplot(aes(fct_reorder(NomeParlamentar,valor), valor, fill = sgPartido))+
  geom_col()+
  coord_flip()+
  theme_classic()+
  geom_text(aes(label = round(valor,2)),color = "black")+
  ylab("VALOR GASTO(MIL REAIS)")+
  xlab("")+
  ggtitle("OS 10 DEPUTADOS FEDERAIS QUE MENOS GASTARAM EM 2019")


gastos_estados <- df2 %>%
  filter(numMes > 1 & !is.na(cpf)) %>%
  group_by(sgUF) %>%
  summarize(valor = sum(Valor_liquido/1e06)) %>%
  arrange(desc(valor)) %>%
  select(sgUF, valor)

# grafico de gastos dos estados
gastos_estados  %>%
  ggplot(aes(fct_reorder(sgUF , valor), valor, ymin =0, ymax = valor))+
  geom_point()+
  geom_linerange()+
  coord_flip()+
  theme_classic()+
  #geom_label(aes(label = round(valor,0)))+
  xlab("")+
  ylab("VALOR GASTO (MILHOES)")+
  ggtitle("OS ESTADOS QUE MAIS GASTARAM EM 2019")


