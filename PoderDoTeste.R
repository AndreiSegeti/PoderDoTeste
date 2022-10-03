#install.packages("distributions3")
library(distributions3)
library(ggplot2)
Z <- Normal(0, 1)

## Teste de Hipóteses : Poder do Teste

#### 1º Passo: gerar a região crítica 

# A região crítica é dada a partir da probabilidade do estimador
# ser maior, menor ou "distante" do parâmetro real. 
# No exemplo de estudo, temos uma máquina que enche pacotes de café com var. = (400g)^2,
# a hipótese nula é que a máquina está regulada (\mi = 500), e a alternativa é que não está,
# \mi != 500. 

# Geramos a RC considerando algum valor máximo para o erro de tipo I, rejeitar a hipótese dada 
# que ela é verdadeira:
#      P(rejeitar | H0 verdadeira) = P(X \in RC | \mi = 500) = P(X < 500 - d ou X > 500 + d | \mi = 500)

alpha_t = 0.01
n_t = 16
mi_t = 500
variance_t = 400
type_t = "double"

boundaries_normal <- function(alpha, mean, variance, type){
  if(type == "double"){
    alpha = alpha/2
    z = quantile(Z, alpha)
    d_negative = mean + z*sqrt(variance)
    d_positive  = mean - z*sqrt(variance)
    return(sort(c(d_negative, d_positive)))
  }
  if(type == "less"){
    z = quantile(Z, alpha)
    d = mean + z*sqrt(variance)
    return(d)
  }
  if(type == "greater"){
    z = quantile(Z, 1-alpha)
    d = mean + z*sqrt(variance)
    return(d)
  }
  
}

boundaries_t <- boundaries_normal(alpha_t, mi_t, variance_t/n_t, "double")
boundaries_t

#### 2º Passo: aplicar a região crítica para gerar o poder do teste

# O poder do teste é o complementar do erro de tipo II, ou seja,
# 1 - P(não rejeitar H0 | H1) = P(rejeitar H0 | H1) 

# Agora, define-se uma função considerando a probabilidade do estimador estar na região crítica,
# sendo que H1 é verdadeira

b <- function(mean, variance, type, boundaries){
  N <- Normal(mean, sqrt(variance))
  if(type == "double"){
    return(cdf(N, boundaries[1]) + (1 - cdf(N, boundaries[2]))) 
  }
  if(type == "less"){
    return(cdf(N, boundaries[1])) 
  }
  if(type == "greater"){
    return(1-cdf(N, boundaries[1])) 
  }
}

b(mi_t, variance_t/n_t, "double", boundaries_t)


rng_t <- 475:525
power_list_t <- c()


for(i in rng_t){
  c <- b(i, variance_t/n_t, "double", boundaries_t)
  power_list_t <- append(power_list_t, c)
}

power_list_2_t <- data.frame(power = power_list_t, mi = rng_t)
ggplot(data = power_list_2_t, aes(x=mi, y=power)) + geom_line() + geom_hline(yintercept=alpha_t)

### juntando as funções:

poder_do_teste <- function(alpha, mi, variance, type, n, rng){
  boundaries_f <- boundaries_normal(alpha, mi, variance/n, type)
  power_list <- c()
  for(i in rng){
    power_list <- append(power_list, b(i, variance/n, type, boundaries_f))
  }
  power_list_2 <- data.frame(power = power_list, mi = rng)
  p <- ggplot(data = power_list_2, aes(x=rng, y=power)) + geom_line() + geom_hline(yintercept=alpha, linetype="dotted")
  #return(p)
  #print(boundaries)
  #print(b(500, variance/n, type, boundaries))
  return(p)
}

poder_do_teste(0.005, 500, 400, "double", 100, 475:525)











