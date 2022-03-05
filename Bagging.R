####################################################
# Aumentando a precisão do modelo usando BAGGING

# Quando usamos uma árvore de decisão para um determinado conjunto de dados, usamos
# apenas UM conjunto de dados de treinamento.

# Sendo assim temos a desvantagem de usar uma única árvore de decisão o problema disso 
# é que essa ÚNICA árvore tende a sofre de ALTA VARIÂNCIA, ou seja, se dividirmos o 
# conjunto de dados em duas metades e aplicarmos a árvore de decisão nas DUAS METADES, 
# resultados podem ser bem diferentes.

# O método BAGGING "agregação de bootstrap" é usado para reduzir a variação de 
# uma ÚNICA árvore de decisão.

# ** Lembrando que BOOTSTRAP é um jeito simple e eficiente para estimar a distribuição 
# amostral de uma estatística ou de parâmetros de modelo e extrair amostras adicionais, 
# com reposição, da própria amostra original, e recalcular a estatística ou modelo 
# para cada reamostra.


# O BAGGING funciona da seguinte forma:

# 1- Pegue "b" amostras de bootstrap do conjunto de dados original;
# 2- Construa uma árvore de decisão para cada amostra bootstrap;
# 3- Calcule as médias das previsões de cada árvore para chegar a um modelo final;

# Nesse método são contruídos centenas ou milhares de árvores de decisão individuais 
# e assim obtemos as previsões médias de todas as árvores, muitas vezes acabamos com um 
# modelo BAGGING ajustado e produz uma taxa de erro de teste muito menor em comparação 
# com uma única árvore.

###########################################################################
# Para a aplicação do método BAGGING usarei um dataset integrado ao R chamado arquality.

library(dplyr) # para de manipulação de dados.
library(e1071) # para realizar o cálculo de importância variável.
library(caret) # para montagem geral do modelo.
library(rpart) # para ajustar árvores de decisão. 
library(ipred) # para ajuste de árvores de decisão bagged.

# Exibe a estrutura do dataframe "airquality", que contém os dados das medições 
# da qualidade do ar em New York em 153 dias diferentes.
str(airquality)

# usei o set.seed para poder reproduzir os resultados dos geradores de números aleatórios.
# aplicado quando se quer ajustar um modelo de classificação e precisa de dois 
# subconjuntos dos dados, um para treinar o modelo e outro para o testar.
set.seed(1)

# gerar o modelo bagged

bagged <- ipred::bagging(formula = Ozone ~ ., # fomula: variável resposta ~ preditores
                         data = airquality,   # data: dataframe.
                         nbagg = 150,         # nbagg: um número inteiro que fornece o número de replicações 
                                              # de bootstrap.
                         coob = TRUE,         # coob: valor booleano que indica se uma estimatova fora taxa 
                                              # de erro deve ser computada, nesse caso será computada.
                         control = rpart.control(minsplit = 2, cp = 0))

# control: os hiperparametros:
# - minsplit: o número mínimo de observações que devem existir em um nó para dividir nesse caso duas observações.
# - cp: o cp é usado para limitar o crescimento da árvore anexando uma penalidade para complexidades 
#       (divisões) adicionais, nesse caso, como definimos como 0 não exigimos que o modelo seja capaz de 
#       melhorar o ajuste geral em qualquer valor para realizar uma divisão.

# Como resolvi definir os hiperparametros minsplit = 2 e cp = 0, permiti que as árvores individuais 
# cresçam extremamente profundas, isso resulta em árvores com ALTA VARIÂNCIA, mas baixo viés, sendo 
# assim ao aplicar o bagging reduzirei a variação do modelo final e mantendo o viés baixo.

# exibe o modelo ajustado.
bagged

# Análise do resultado:
# O erro quadrático médio estimado fora da bag é de: 17.4973, ou seja, esta é a diferença
# média entre o valor previsto e o valor observado para Ozone.


# Importância dos preditores.








