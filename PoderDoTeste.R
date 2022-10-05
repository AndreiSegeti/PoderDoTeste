#install.packages("distributions3")
#install.packages("gridExtra")
library(distributions3)
library(ggplot2)
library(gridExtra)

Z <- Normal(0, 1)

## Teste de Hipóteses : Poder do Teste

#### 1º Passo: gerar a região crítica 

# A região crítica é dada a partir da probabilidade do estimador
# ser maior, menor ou "distante" do parâmetro real. 
# No exemplo de estudo, temos uma máquina que enche pacotes de café com var. = (400g)^2,
# a hipótese nula é que a máquina está regulada (\mu = 500), e a alternativa é que não está,
# \mu != 500. 

# Geramos a RC considerando algum valor máximo para o erro de tipo I, rejeitar a hipótese dada 
# que ela é verdadeira:
#      P(rejeitar | H0 verdadeira) = P(X \in RC | \mu = 500) = P(X < 500 - d ou X > 500 + d | \mu = 500)

alpha_t = 0.01
n_t = 16
mu_t = 500
variance_t = 400
type_t = "double"

boundaries_normal <- function(alpha, mu, variance, type){
  if(type == "double"){
    alpha = alpha/2
    z = quantile(Z, alpha)
    d_negative = mu + z*sqrt(variance)
    d_positive  = mu - z*sqrt(variance)
    return(sort(c(d_negative, d_positive)))
  }
  if(type == "less"){
    z = quantile(Z, alpha)
    d = mu + z*sqrt(variance)
    return(d)
  }
  if(type == "greater"){
    z = quantile(Z, 1-alpha)
    d = mu + z*sqrt(variance)
    return(d)
  }
  
}

boundaries_t <- boundaries_normal(alpha_t, mu_t, variance_t/n_t, "double")
boundaries_t

#### 2º Passo: aplicar a região crítica para gerar o poder do teste

# O poder do teste é o complementar do erro de tipo II, ou seja,
# 1 - P(não rejeitar H0 | H1) = P(rejeitar H0 | H1) 

# Agora, define-se uma função considerando a probabilidade do estimador estar na região crítica,
# sendo que H1 é verdadeira

b <- function(mu, variance, type, boundaries){
  N <- Normal(mu, sqrt(variance))
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

b(mu_t, variance_t/n_t, "double", boundaries_t)


rng_t <- 475:525
power_list_t <- c()


for(i in rng_t){
  c <- b(i, variance_t/n_t, "double", boundaries_t)
  power_list_t <- append(power_list_t, c)
}

power_list_2_t <- data.frame(power = power_list_t, mu = rng_t)
ggplot(data = power_list_2_t, aes(x=mu, y=power)) + geom_line() + geom_hline(yintercept=alpha_t)

### juntando as funções:

poder_do_teste <- function(alpha, mu, variance, type, n, rng){
  boundaries_f <- boundaries_normal(alpha, mu, variance/n, type)
  power_list <- c()
  for(i in rng){
    power_list <- append(power_list, b(i, variance/n, type, boundaries_f))
  }
  power_list_2 <- data.frame(power = power_list, mu = rng)
  p <- ggplot(data = power_list_2, aes(x=rng, y=power)) + geom_line() + geom_hline(yintercept=alpha, linetype="dotted")
  return(p)
}

poder_do_teste(0.005, 500, 400, "double", 100, 475:525)

p1 <- poder_do_teste(0.01, 500, 400, "double", 20, 475:525)
p2 <- poder_do_teste(0.01, 500, 400, "double", 100, 475:525)
p3 <- poder_do_teste(0.01, 500, 400, "less", 20, 475:525)
p4 <- poder_do_teste(0.01, 500, 400, "greater", 100, 475:525)

grid.arrange(p1, p2, p3, p4, ncol=2)
