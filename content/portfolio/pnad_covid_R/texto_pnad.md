---
date: "2020-07-16T19:47:09+02:00"
description: This is the description of our sample project
jobDate: 16/07/2020
techs:
- Rstats
thumbnail: pnad_covid_R/tidyverse.png
title: Trabalhando com os microdados da PNAD COVID19 no R
work:
- Rstats
- COVID-19
- PNAD
---


### Introdução

Os microdados da PNAD são, na minha opinião, uma das formas mais preciosas para compreender as condições socioeconômicas e demográficas do Brasil. Trata-se de uma base com ampla capilaridade e milhões de linhas repleta de informações da população brasileira. O indivíduo trabalha? Qual o valor da remuneração? Estudou até quando? Sabe ler? Como são as condições na sua residência? Nela tem água tratada? Esgoto? São questões abordadas na PNAD Continua e que servem de base para desenvolver e analisar políticas públicas diversas.

Em um período de pandemia, cuja solução momentânea que se mostra mais eficiente é o isolamento social, levar pesquisadores à campo para aplicar questionários da maneira habitual se mostrou uma tarefa pouco adequada. Pensando nisso e para manter a sociedade informada, o IBGE buscou contornar a situação via entrevistas realizadas por telefone. Com as informações coletadas foi gerada a PNAD COVID19. O IBGE frisa que [os dados são experimentais](https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=o-que-e&utm_source=covid19&utm_medium=hotsite&utm_campaign=covid_19) e devem ser usadas com cautela, isso porque essa é uma PNAD diferente das outras, ou seja, é arriscado comparar os dados dessa PNAD com a PNADC de 2019, por exemplo. Todavia, isso não quer dizer que a informação seja inválida, longe disso, na verdade, ela fornece um olhar para o período atípico que nos encontramos.

Dentro do R existe uma gama de opções para analisar amostras, geralmente o pacote Survey é o predileto dos pesquisadores. Porém, optei pelo srvyr, por ser um pacote que fornece opções para trabalhar com o dplyr. Foquei em analisar o que se passa em Salvador, mas é possível reproduzir os resultados para outras capitais, UF’s ou simplesmente para o Brasil todo, a curiosidade de cada um é o que faz com que tenhamos novas descobertas e inovações.

Então, depois de toda essa explicação, vamos ao que é mais legal: botar a mão na massa!


### Carregando pacotes e os microdados

Primeira coisa: faça o download dos [microdados no site do IBGE](https://www.ibge.gov.br/estatisticas/investigacoes-experimentais/estatisticas-experimentais/27946-divulgacao-semanal-pnadcovid1?t=microdados&utm_source=covid19&utm_medium=hotsite&utm_campaign=covid_19). Depois de descompactar, verá que existe um arquivo CSV (que são os dados) e o dicionário. Tudo bem usual pra quem já trabalhou com PNAD antes.

Com os dados no computador, vamos instalar os pacotes e carregar os dados no RStudio.

``` r
library(dplyr)
library(srvyr) 
library(readr)
library(ggplot2)
library(Cairo)

# Pasta de Trabalho
setwd("C:\\Users\\pc\\pnad_covid")

# Carregando dataset
pnad_covid <- read_csv("PNAD_COVID_062020.csv",
                       col_types = cols(.default = "d"))
```

O dataset completo tem 381.270 linhas e 114 colunas (woooow).


### Colocando os pesos e criando variáveis

Se você abrir o dicionário da PNAD COVID19 vai notar uma variável chamada V1032. É essa variável que define o peso do domicílio e das pessoas. Precisamos ligar isso no nosso dataset. Também vou filtrar a capital para Salvador.

``` r
# ligando Pesos e filtrando Salvador
pnad_com_pesos <- pnad_covid %>%
                      as_survey_design(ids = UPA,
                                       strata = Estrato,
                                       weights = V1032,
                                       nest = TRUE) %>% 
                      filter(CAPITAL == "29")
```
Agora vamos criar algumas colunas com as informações que queremos trabalhar.

``` r
# Criando colunas com Variáveis 
pnad_com_pesos <- pnad_com_pesos %>% mutate(one = 1,
Sexo = ifelse(A003 == 1, "Homem", "Mulher"), 
Idade = case_when(
   A002 %in% 15:24 ~ "15-24",
   A002 %in% 25:34 ~ "25-34", 
   A002 %in% 35:49 ~ "35-49", 
   A002 %in% 50:64 ~ "50-64", 
   A002 > 64 ~ "65+"),
Cor = case_when(
   A004 == 1 ~ "Branca", 
   A004 == 2 ~ "Preta", 
   A004 == 4 ~ "Parda"),
Escolaridade = factor(case_when( 
   A005 %in% 1:2 ~ "Sem Instrução ou Fundamental Incompleto", 
   A005 %in% 3:4 ~ "Fundamental completo ou Médio Incompleto", 
   A005 %in% 5:6 ~ "Médio completo ou Superior Incompleto", 
   A005 == 7 ~ "Superior completo", 
   A005 == 8 ~ "Pós-graduação"), 
     levels = c( "Sem Instrução ou Fundamental Incompleto",
                 "Fundamental completo ou Médio Incompleto", 
                 "Médio completo ou Superior Incompleto",
                 "Superior completo",
                 "Pós-graduação")), 
Tipo_emprego = factor(case_when(
   C007 == 1 ~ 
"Trabalhador doméstico (empregado doméstico, cuidados, babá)",
   C007 == 2 ~ "Militar",
   C007 == 3 ~ "Policial ou Bombeiro",
   C007 == 4 ~ "Setor privado",
   C007 == 5 ~ "Setor público",
   C007 == 6 ~ "Empregador",
   C007 == 7 ~ "Autônomo (Conta própria)"),
     levels = c(
"Trabalhador doméstico (empregado doméstico, cuidados, babá)",
"Militar", 
"Policial ou Bombeiro",
"Setor privado",
"Setor público",
"Empregador",
"Autônomo (Conta própria)")), 
Faixa_salario = factor(case_when(
  C01012 <= 1044 ~ "Menos de um salário mínimo",
  C01012 %in% c(1045:2090) ~ "Entre 1 e 2",
  C01012 %in% c(2091:3135) ~ "Entre 2 e 3",
  C01012 %in% c(3136:4180) ~ "Entre 3 e 4",
  C01012 %in% c(4181:5225) ~ "Entre 4 e 5",
  C01012 >= 5226 ~ "Mais de 5"),
    levels = c("Menos de um salário mínimo",
               "Entre 1 e 2",
               "Entre 2 e 3",
               "Entre 3 e 4",
               "Entre 4 e 5",
               "Mais de 5")),
domicilio_situacao = factor(case_when(
  F001 == 1 ~ "Próprio - já pago",
  F001 == 2 ~ "Próprio - ainda pagando" ,                                  
  F001 == 3 ~ "Alugado",
  F001 %in% 4:6 ~
   "Cedido (Por empregador, Familiar ou outro)"),
      levels = c(
      	"Próprio - já pago",
        "Próprio - ainda pagando",
        "Alugado", 
        "Cedido (Por empregador, Familiar ou outro)")),
home_office = ifelse(C013 == 1, "Home Office", "Presencial"),
auxilio_emergencial = ifelse(D0051 == 1, "Auxílio",
                                         "Sem auxílio")
)
```

Ficaria um texto muuuuito longo se eu fosse detalhar cada etapa, mas em resumo: Foram criadas novas colunas e nelas eu apliquei um rótulo baseado nas colunas que já existem, ou seja, ao invés de uma coluna chamada A003, contendo apenas os valores 1 e 2, eu criei uma nova coluna chamada “Sexo” com as informações “Homem” e “Mulher”, assim fica mais fácil de interpretar. Para fazer sua análise, confira o dicionário que veio junto com os dados (O dicionário é muito importante, não se esqueça dele).


### Gerando gráficos e analisando resultados
Um ponto importante sobre os gráficos a seguir: Cada percentual é referente ao total daquele grupo, não tendo uma relação com o percentual ao lado. Por exemplo, quando eu digo que 31,91% das mulheres de cor branca estão em home office, isso significa que 68,09% das mulheres de cor branca não estão em home office. Ou seja, cada grupo tem seu percentual próprio. Optei por fazer isso por achar mais interessante comparar cada grupo com o total dele na sociedade. Maaas, caso queiram trabalhar com gráficos em que a soma dos percentuais atinjam 100%, fiz um outro script que está disponível aqui. Lá no git também tem uma pasta com os gráficos desse outro script, você poderá encontrar aqui. Dito tudo isso, vamos partir para as análises!

Primeiro vamos olhar o sexo e a cor das pessoas que estão em Home Office em Salvador.

``` r
############### Home office - Por sexo e cor ##################

# Criando dataset para conferir pessoas em Home Office

home_sexo_cor <- pnad_com_pesos %>%
  group_by(Sexo, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

# gráfico
home_sexo_cor_ssa <- ggplot(home_sexo_cor,
                            aes(fill = Cor,
                                y = trab_home_office,
                                x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)), 
            size = 3, position = position_dodge(width=0.9),
            vjust=-0.5, color = 'black', fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill = "ghostwhite",
                                         size=0.7,
                                         linetype="blank")) +
  labs(x = "Sexo", fill = "Cor/Raça: ",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title = 
"Pessoas em home office, por cor/raça e sexo - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675",
  	                           "#0984e3","#6c5ce7")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando
ggsave(plot = home_sexo_cor_ssa, "home_sexo_cor_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```


{{< figure src="/images/grafico_1.png" caption="Pessoas em home office, por sexo e cor" >}}

Podemos ver que mulheres de cor branca são o grupo predominante em home office, 31,91% dos indivíduos com essas duas características se encontram trabalhando em casa. Já o menor grupo são o de homens de cor preta, com um singelo percentual de 9,14%.


Mas e quanto ao nível de escolaridade? Como ele se comporta? Vamos observar.

``` r
############ Home office - Por Cor e Escolaridade ##########

home_edu_cor <- pnad_com_pesos %>%
  group_by(Escolaridade, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)
  ) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()


# gráfico
home_edu_cor_ssa <- ggplot(home_edu_cor,
                           aes(fill = Escolaridade,
                           	   y = trab_home_office,
                           	   x = Cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),
            size = 3,
            position = position_dodge(width=0.9),
            vjust=-0.5, color = 'black', fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text = element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.7,
                                         linetype="blank")) +
  labs(x = "Cor/Raça", fill = "Escolaridade: ",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
"Pessoas em home office, por cor/raça e escolaridade - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3",
  	                           "#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")


# Salvando em PNG
ggsave(plot = home_edu_cor_ssa, "home_edu_cor_ssa.png",
       width = 14, height = 7, dpi = 150,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_2.png" caption="Pessoas em home office, por cor/raça e escolaridade" >}}

Podemos ver algumas coisas interessantes com esse gráfico:

1. Pessoas sem instrução ou com o fundamental incompleto não aparecem em home office ou aparecem com um percentual muito pequeno. Isso pode estar acontecendo pelas condições econômicas dessas pessoas, que não possuem um emprego formal que sustente a condição de fazer um home office ou porque essas pessoas no geral se encontram em algum tipo de trabalho informal. Um outra olhar é de que a pesquisa não conseguiu alcançar essas pessoas, dado que a coleta dos dados ocorreu por meio de entrevista por telefone. Cabe uma análise mais profunda a posteriori.

2. Existe um pequeno grupo de pessoas de cor parda e que tem fundamental completo ou médio incompleto que estão em home office. Também existe um pequeno grupo de pessoas de cor preta sem instrução ou com fundamental incompleto que estão em home office.

3. O nível de escolaridade é um fator preponderante para estar ou não em home office, isso pode ser notado pela evolução do nível de escolaridade e aumento no percentual de pessoas em home office. Pessoas com pós-graduação tem mais chances de estar trabalhando remotamente do que alguém com apenas o ensino médio.

Vamos ver agora como está a relação Sexo/Idade.

``` r
############# Home office - Por Sexo e Idade #############

home_sexo_idade <- pnad_com_pesos %>%
  group_by(Sexo, Idade) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)
  ) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

# gráfico
home_sexo_idade_ssa <- ggplot(home_sexo_idade,
                              aes(fill = Idade,
                                  y = trab_home_office,
                                  x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),
            size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill = "ghostwhite",
                                         size=0.7,
                                         linetype="blank")) +
  labs(x = "Sexo", fill = "Faixa Etária: ",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
"Pessoas em home office, por sexo e faixa etária - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675","#0984e3",
  	                           "#6c5ce7","#fdcb6e")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando em PNG
ggsave(plot = home_sexo_idade_ssa, "home_sexo_idade_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_3.png" caption="Pessoas em home office, por sexo e faixa etária" >}}

Empiricamente é tentador pensar que quanto maior a idade, maior a chance de estar em home office. Talvez porque imagina-se que pessoas com mais idade tenderiam a ocupar cargos mais altos e que estariam aptas a exercer suas funções em casa. Mas não foi isso que os dados mostraram para Salvador. Os valores são muitos semelhantes em todas as faixas. Com exceção no grupo das mulheres, onde 70,32% daquelas com mais de 65 anos estão em home office.

E quanto ao emprego dessas pessoas? Qual vinculo empregatício mantém mais pessoas trabalhando em casa?

``` r
############ Home office - Por trabalho ##############

home_emprego <- pnad_com_pesos %>%
  group_by(Tipo_emprego) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

# ordenando eixo X
legenda_trabalhos <- c(
"Trabalhador doméstico\n (empregado doméstico,\n cuidados, babá)",
"Militar", 
"Policial ou\n Bombeiro",
"Setor privado",
"Setor público",
"Empregador",
"Autônomo\n (Conta própria)")

# Gráfico
home_emprego_ssa <- ggplot(home_emprego,
                           aes(fill = Tipo_emprego,
                               y = trab_home_office,
                               x = Tipo_emprego)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%1.2f%%",trab_home_office)),
            size = 3, position =position_dodge(width=0.9),
            vjust = -0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text = element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=8),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "none") +
  labs(x = "Tipo de Ocupação",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
"Pessoas em home office, por tipo de ocupação - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675",
  	                           "#0984e3","#6c5ce7",
                               "#fdcb6e","#636e72",
                               "#55efc4")) +
  scale_x_discrete(labels = legenda_trabalhos) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando em PNG
ggsave(plot = home_emprego_ssa, "home_emprego_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_4.png" caption="Pessoas em home office, por tipo de ocupação" >}}

Podemos ver que é no setor público onde mais pessoas estão em home office. Trabalhadores autônomos são os que mais exercem suas atividades fora de casa. Para categorias da segurança pública, militares ou trabalhadores domésticos não foram encontrados resultados na capital baiana.

Como está a distribuição entre cor/raça e salários das pessoas em home office?

``` r
########## Home office - Por faixa salarial e cor ############

home_renda <- pnad_com_pesos %>%
  group_by(Faixa_salario, Cor) %>%
  summarise(
    home_office = survey_total(C013 == 1, na.rm = TRUE),
    mao_de_obra = survey_total(C001 == 1, na.rm = TRUE)) %>%
  mutate(trab_home_office = (home_office/mao_de_obra)*100) %>%
  drop_na()

# gráfico
home_renda_ssa <- ggplot(home_renda,
                         aes(fill = Faixa_salario,
                             y = trab_home_office,
                             x = Cor)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",trab_home_office)),
            size = 2.5, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text = element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.7,
                                         linetype="blank")) +
  labs(x = "Cor/Raça",
       fill = "Faixa Salarial:\n(Salários mínimos) ",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
"Pessoas em home office, por cor/raça e faixa salarial - Salvador/BA ") +
  scale_fill_manual(values = c("#fad390","#e55039",
  	                           "#4a69bd","#60a3bc",
  	                           "#78e08f","#079992")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando em PNG
ggsave(plot = home_renda_ssa, "home_renda_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_5.png" caption="Pessoas em home office, por cor/raça e faixa salarial" >}}

Podemos observar um percentual alto de pessoas que recebem 4, 5 ou mais salários mínimos em home office. Mas o mais interesse é ver que entre pessoas brancas que recebem menos de um salário mínimo, 23,20% estão mantendo suas atividades em home office. Olhando o mesmo rendimento para pardos e pretos, o percentual encontrado é bem menor, 12,89% e 9,05% respectivamente.

Mas vamos mudar o foco agora. Deixar de olhar apenas trabalhadores em home office e passar a ver como o auxilio emergencial tem sido direcionado; qual o reflexo desse auxilio na população.

Vamos começar olhando quem recebeu o auxilio.

``` r
############ Auxilio - Sexo e Cor ##################

auxilio_cor_sexo <- pnad_com_pesos %>%
  group_by(Cor, Sexo) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)
  ) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100) %>%
  drop_na()

# gráfico
auxilio_cor_sexo_ssa <- ggplot(auxilio_cor_sexo,
                               aes(fill = Cor,
                                   y = pessoas_auxilio,
                                   x = Sexo)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%1.2f%%",pessoas_auxilio)),
            size = 3, position = position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "bottom",
        legend.background = element_rect(fill="ghostwhite",
                                         size=0.7,
                                         linetype="blank")) +
  labs(fill = "Cor: ", x = "Sexo",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
"Pessoas que receberam auxílio emergencial, por cor/raça e sexo -\n Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675",
  	                           "#0984e3")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando em PNG
ggsave(plot = auxilio_cor_sexo_ssa,
       "auxilio_cor_sexo_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_6.png" caption="Pessoas que receberam o auxílio emergencial, por cor/raça e sexo" >}}

Pardos e Pretos são os grupos que mais receberam o auxilio. Entre os gêneros, os valores são muitos próximos. 50,60% dos homens de cor parda receberam o auxilio, já entre as mulheres de mesma cor, 50,90%. Brancos são o menor grupo em ambos os sexos.

E como anda a renda dessas pessoas que receberam o auxílio?

``` r
############ Auxilio - Faixa Salarial ###############

auxilio_renda <- pnad_com_pesos %>%
  group_by(Faixa_salario) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio = (auxilio/total)*100) %>%
  drop_na()

# gráfico
auxilio_renda_ssa <- ggplot(auxilio_renda,
                            aes(fill = Faixa_salario,
                                y = pessoas_auxilio,
                                x = Faixa_salario)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = sprintf("%1.2f%%",pessoas_auxilio)),
            size = 3, position = position_dodge(width=0.9),
            hjust=-0.1, color = 'black',fontface='bold') +
  theme_classic() +
  coord_flip() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text = element_text(size=6,
                                 face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "none") +
  labs(x = "Faixa Salarial",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
"Pessoas que receberam auxílio emergencial, por renda - Salvador/BA") +
  scale_fill_manual(values = c("#00b894","#ff7675",
  	                           "#0984e3","#6c5ce7",
  	                           "#fdcb6e","#636e72")) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando em PNG
ggsave(plot = auxilio_renda_ssa, "auxilio_renda_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_7.png" caption="Pessoas que receberam o auxílio emergencial, por renda" >}}

Nota-se que 73,90% das pessoas que viviam com menos de um salário mínimo receberam o auxílio. Dentre aqueles cuja renda permeiam entre 1 e 2 salários, uma parcela expressiva, 45,39%, recebeu o dinheiro. Uma pequena parcela da população, com renda entre 4 e 5, ou mais de 5 salários mínimos também foram contempladas.

Sabemos que o custo de moradia é algo que pesa no orçamento das famílias, por isso considero interessante ver a situação domiciliar daqueles que estão recebendo o auxilio.

``` r
################ Auxilio - Por tipo do domicilio ###################

auxilio_domicilio <- pnad_com_pesos %>%
  group_by(domicilio_situacao) %>%
  summarise(
    auxilio = survey_total(D0051 == 1, na.rm = TRUE),
    total = survey_total(one, na.rm = TRUE)) %>%
  mutate(pessoas_auxilio  = (auxilio/total)*100) %>%
  drop_na()

# ordenando eixo X
legenda_domicilio <- c(
	"Próprio (já pago)",
    "Próprio (ainda pagando)",
    "Alugado", 
    "Cedido (Por empregador,\n Familiar ou outro)")

# gráfico
auxilio_domicilio_ssa <- ggplot(auxilio_domicilio,
                                aes(fill = domicilio_situacao,
                                    y = pessoas_auxilio,
                                    x = domicilio_situacao)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label=sprintf("%1.2f%%",pessoas_auxilio)),
            size = 3, position =position_dodge(width=0.9),
            vjust=-0.5, color = 'black',fontface='bold') +
  theme_classic() +
  theme(axis.title.x = element_text(colour = "black"),
        axis.title.y = element_text(colour = "black"),
        axis.text.y = element_text(face="bold",
                                   color="#000000", 
                                   size=10),
        axis.line = element_line(colour = "black", 
                                 size = 1,
                                 linetype = "solid"),
        axis.text=element_text(size=6, face="bold"),
        axis.text.x = element_text(face="bold",
                                   color="#000000",
                                   size=10),
        plot.title = element_text(colour = "black",
                                  size = 17,
                                  hjust=0.5),
        legend.position = "none") +
  labs(x = "Tipo de domicílio",
       y = "Percentual (%)",
caption =
"Fonte: Microdados da Pnad Covid19 - IBGE. Junho 2020.",
title =
 "Situação do domicílio daqueles que receberam o auxílio emergencial -\n Salvador/BA") +
  scale_fill_manual(values = c("#fad390","#e55039",
  	                           "#4a69bd","#60a3bc",
  	                           "#78e08f","#079992")) +
  scale_x_discrete(labels = legenda_domicilio) +
  scale_y_discrete(limits=factor(0:100),
                   breaks = c(0,10,20,30,40,50,
                   	          60,70,80,90,100),
                   name = "Percentual (%)")

# Salvando em PNG
ggsave(plot = auxilio_domicilio_ssa,
       "auxilio_domicilio_ssa.png",
       width = 10, height = 5, dpi = 120,
       units = "in",type = "cairo")
```

{{< figure src="/images/grafico_pnad_covid_8.png" caption="Situação do domicilio daqueles que receberam o auxílio emergencial" >}}


É interessante ver que 48,56% das pessoas com imóvel próprio receberam o auxilio. 65,65% daqueles que de alguma forma vivem em um imóvel cedido também foram beneficiados. 40,75% dos que pagam aluguel foram contemplados, isso significa que dentre os que pagam aluguel, 59,25% não receberam o pagamento do governo.


### Considerações Finais

Todo o script você poderá encontrar no meu [GitHub](https://github.com/CleitonOERocha/Scripts/blob/master/PNAD_COVID19/PNADCOVID_SSA_v2.R). Ademais, muito do que foi apresentado aqui teve como inspiração o trabalho do professor Regis A. Ely, da Universidade Federal de Pelotas, o trabalho dele poderá ser encontrado [aqui](http://regisely.com/blog/mercado-de-trabalho-covid/). Agradeço a todos que, porventura, caíram nesse site e acharam o conteúdo interessante.