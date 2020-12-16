#³adowanie pakietów
library(dplyr)
library(lubridate)
library(ggplot2)
library(distr)
library(GAS)

#wczytanie danych
data <- read.csv("C:/Users/Pawe³/Desktop/kursy_walut.csv",
                 sep = ";", header = TRUE)
#sformatowanie daty
data$data <- ymd(data$data)
#pozostawienie wy³¹cznie 3 wybranych walut
data <- data[,c("data","X1GBP", "X1DKK", "X1THB")]
#rzut oka na dane
glimpse(data)



#wykres czas - kurs waluty
data %>% ggplot(aes(x = data, y = X1GBP)) + geom_point()
data %>% ggplot(aes(x = data, y = X1DKK)) + geom_point()
data %>% ggplot(aes(x = data, y = X1THB)) + geom_point()

#stopy zwrotu
stopy_zw <- function(data){
  res <- NA
  for (i in 1:length(data)-1){
  res[i] <- 100*log(data[i+1]/data[i])#procentowe stopy zwrotu !
  }
  return(res)
}

data$stopy_GBP <- c(1,-stopy_zw(data$X1GBP))
data$stopy_DKK <- c(1,-stopy_zw(data$X1DKK))
data$stopy_THB <- c(1,-stopy_zw(data$X1THB))

tail(data)
head(data)
#wykresy stóp zwrotu
data %>% ggplot(aes(x = data, y = stopy_GBP)) + geom_line()
data %>% ggplot(aes(x = data, y = stopy_DKK)) + geom_line()
data %>% ggplot(aes(x = data, y = stopy_THB)) + geom_line()

#funkcja 1
#q -> kwantyl, d -> szerokosc okna
symulacja_hist <- function(data, q = .99, d = 500){
  var <- NA
  ES <- NA
  for (i in 1:(length(data)-d)){
    okno <- data[i:(i+500-1)] 
    var[i] <- quantile(okno, q)
    ES[i] <- mean(okno[okno >= quantile(okno, q)])
  }
  
  return(list(var,ES))
}
#funkcja 2 
#g - wsp do wag
symulacja_hist_z_wagami <- function(data, q = .99, d = 500, g = .995){
  
  wagi <- sapply(c(1:d), function(x){(g^(d-x)*(1-g))/(1-g^d)})
  ES <- NA
  var <- NA
  for (i in (1:(length(data)-d))){
    okno <- data[i:(i+d-1)]
    rozklad <- DiscreteDistribution(supp = okno, prob = wagi)
    var[i] <- rozklad@q(q)
    index <- which(okno >= var[i])
    
    ES[i] <- sum(okno[index]*(wagi[index])/(1-q))# +
     # okno[which(okno == var[i])]*(1 - sum(wagi[index])/(1-q))
  }
  
  return(list(var,ES))
}

#funkcja bootstrap
#n - liczba symulacji, n - liczba obserwacji w losowanej probce
bootstrap_var <- function(data, q = .99, d = 500, x = 600, n = 10){
  
  #95% przedzial ufnosci
  przedzialy_ufnosci <- data.frame()
  var <- NULL
  ES <- NULL
  
  for (i in 1:(length(data)-d)){
    okno <- data[i:(i+500-1)] 
    
    var_b <- NA
    ES_b <- NA
    

    for (i in 1:n){
    #losowanie indeksów [ze zwracaniem]
    set.seed(i)
    index <- sample(x = c(1:d), size = x, replace = TRUE)
    
    boot_sample <- okno[index]
    var_b[i] <- quantile(boot_sample, q)
    ES_b[i] <- mean(boot_sample[boot_sample >= var_b[i]])
    
    }
    q1 <- quantile(var_b, .025)
    q2 <- quantile(var_b, .975)
    
    przedzialy_ufnosci <- bind_rows(przedzialy_ufnosci, c(q1,q2))
    
    var <- c(var, mean(var_b))
    ES <- c(ES, mean(ES_b))
    
  }
  return(list(VAR = var, ES = ES, przedzialy = przedzialy_ufnosci))
}

#test kupca zwrcajacy liczbe wyj¹tków 
test_kupca <- function(straty, var_vec){
  
  vec <- ifelse(straty > var_vec, 1 , 0)
  return(list(sum(vec), x = which(vec >0.1)))
}


#dla GBP
var_sh_GBP <- symulacja_hist(data$stopy_GBP)
var_h_GBP <- symulacja_hist_z_wagami(data$stopy_GBP)
var_b_GBP <- bootstrap_var(data = data$stopy_GBP)

#DKK
var_sh_DKK <- symulacja_hist(data$stopy_DKK)
var_h_DKK <- symulacja_hist_z_wagami(data$stopy_DKK)
var_b_DKK <- bootstrap_var(data = data$stopy_DKK)

#THB
var_sh_THB <- symulacja_hist(data$stopy_THB)
var_h_THB <- symulacja_hist_z_wagami(data$stopy_THB)
var_b_THB <- bootstrap_var(data = data$stopy_THB)


plot(var_sh_GBP[[1]], type = "l", ylim = (c(1,3)))
lines(var_sh_GBP[[2]], col = "red")
points(x = data$stopy_GBP)

plot(var_h_GBP[[1]], type = "l")
lines(var_h_GBP[[2]], col = "green")
points(x = data$stopy_GBP[-c(1:500)])

plot(var_b_GBP[[1]], type = "l")
lines(var_b_GBP[[2]], col = "red")

plot(var_sh_DKK[[1]], type = "l")
lines(var_sh_DKK[[2]], col = "red")
points(x = data$stopy_DKK[-c(1:500)])

plot(var_h_DKK[[1]], type = "l")
lines(var_h_DKK[[2]], col = "green")
points(x = data$stopy_DKK[-c(1:500)])

plot(var_b_DKK[[1]], type = "l")
lines(var_b_DKK[[2]], col = "red")


#przygotowanie danych do wykresu
data1 <- data %>% bind_cols(VAR_hist_GBP = c(rep(NA, 500), var_sh_GBP[[1]]), VAR_hist_DKK = c(rep(NA, 500), var_sh_DKK[[1]]),
                            VAR_hist_THB = c(rep(NA, 500), var_sh_GBP[[1]]), 
                            VAR_hull_GBP = c(rep(NA, 500), var_h_GBP[[1]]), VAR_hull_DKK = c(rep(NA, 500), var_h_DKK[[1]]),
                            VAR_hull_THB = c(rep(NA, 500), var_h_THB[[1]]),
                            VAR_boot_GBP = c(rep(NA, 500), var_b_GBP[[1]]), VAR_boot_DKK = c(rep(NA, 500), var_b_THB[[1]]),
                            VAR_boot_THB = c(rep(NA, 500), var_b_THB[[1]])
                            )
data1 <- data1[-c(1:500),]
glimpse(data1) 

#wykresy varów

#GBP
data1 %>%
  ggplot(aes(y = VAR_hist_GBP, x = data,colour = "metoda historyczna")) + geom_line(size = 1.1) +
  geom_point(data = data1, aes(y = stopy_GBP), size = 2.5, col = "black") +
  geom_line(data = data1, aes(y = VAR_hull_GBP, colour = "metoda wg hulla"), size = 1.0) +
  geom_line(data = data1, aes(y = VAR_boot_GBP, colour = "bootstrap"), size = 1.0) + ylim(0.5,2.75) +
  labs(title = "Wyniki dla funta brytyjskiego") +
  ylab("stopy zwrotu") + scale_colour_manual("", 
                                             breaks = c("metoda historyczna", "bootstrap", "metoda wg hulla"),
                                             values = c("blue", "red", "pink"))

#DKK
data1 %>%
  ggplot(aes(y = VAR_hist_DKK, x = data,colour = "metoda historyczna")) + geom_line(size = 1.1) +
  geom_point(data = data1, aes(y = stopy_DKK), size = 2.5, col = "black") +
  geom_line(data = data1, aes(y = VAR_hull_DKK, colour = "metoda wg hulla"), size = 1.0) +
  geom_line(data = data1, aes(y = VAR_boot_DKK, colour = "bootstrap"), size = 1.0) + ylim(0.24,1.8) +
  labs(title = "Wyniki dla korony duñskiej") +
  ylab("stopy zwrotu") + scale_colour_manual("", 
                                             breaks = c("metoda historyczna", "bootstrap", "metoda wg hulla"),
                                             values = c("blue", "red", "pink"))

#THB
data1 %>%
  ggplot(aes(y = VAR_hist_THB, x = data,colour = "metoda historyczna")) + geom_line(size = 1.1) +
  geom_point(data = data1, aes(y = stopy_THB), size = 2.5, col = "black") +
  geom_line(data = data1, aes(y = VAR_hull_THB, colour = "metoda wg hulla"), size = 1.0) +
  geom_line(data = data1, aes(y = VAR_boot_THB, colour = "bootstrap"), size = 1.0) + ylim(0.74,2.18) +
  labs(title = "Wyniki dla bahta tajskiego") +
  ylab("stopy zwrotu") + scale_colour_manual("", 
                                             breaks = c("metoda historyczna", "bootstrap", "metoda wg hulla"),
                                             values = c("blue", "red", "pink"))


data1 <- data[-c(1:500),] %>% bind_cols(VAR_hist_GBP = c(rep(NA, 500), var_sh_GBP[[1]]), VAR_hist_DKK = c(rep(NA, 500), var_sh_DKK[[1]]),
                            VAR_hist_THB = c(rep(NA, 500), var_sh_GBP[[1]]), 
                            VAR_hull_GBP = c(rep(NA, 500), var_h_GBP[[1]]), VAR_hull_DKK = c(rep(NA, 500), var_h_DKK[[1]]),
                            VAR_hull_THB = c(rep(NA, 500), var_h_THB[[1]]),
                            VAR_boot_GBP = c(rep(NA, 500), var_b_GBP[[1]]), VAR_boot_DKK = c(rep(NA, 500), var_b_THB[[1]]),
                            VAR_boot_THB = c(rep(NA, 500), var_b_THB[[1]]),
                            ES_hist_GBP = c(rep(NA, 500), var_sh_GBP[[2]]), ES_hist_DKK = c(rep(NA, 500), var_sh_DKK[[2]]),
                            ES_hist_THB = c(rep(NA, 500), var_sh_GBP[[2]]), 
                            ES_hull_GBP = c(rep(NA, 500), var_h_GBP[[2]]), ES_hull_DKK = c(rep(NA, 500), var_h_DKK[[2]]),
                            ES_hull_THB = c(rep(NA, 500), var_h_THB[[2]]),
                            ES_boot_GBP = c(rep(NA, 500), var_b_GBP[[2]]), ES_boot_DKK = c(rep(NA, 500), var_b_THB[[2]]),
                            ES_boot_THB = c(rep(NA, 500), var_b_THB[[2]]))


data1 %>% ggplot(aes(x = data, y = VAR_hist_GBP,colour = "VAR")) + geom_line(size = 1.1) +
  geom_line(aes(y = data1$ES_hist_GBP, colour = "ES"),size = 1.1) +
  geom_point(aes(y = data1$stopy_GBP), col = "black", size = 2.5) + 
  ylim(0.5,3) + scale_colour_manual("", 
                                    breaks = c("VAR", "ES"),
                                    values = c("blue", "orange")) +
  labs(title = "Wynik symulacji historycznej", subtitle = "dla funta brytyjskiego") +
  ylab("stopy zwrotu") 

data1 %>% ggplot(aes(x = data, y = VAR_hull_GBP,colour = "VAR")) + geom_line(size = 1.1) +
  geom_line(aes(y = data1$ES_hull_GBP, colour = "ES"),size = 1.1) +
  geom_point(aes(y = data1$stopy_GBP), col = "black", size = 2.5) + 
  ylim(0.75,3.25) + scale_colour_manual("", 
                                    breaks = c("VAR", "ES"),
                                    values = c("blue", "orange")) +
  labs(title = "Wynik symulacji historycznej z wagami", subtitle = "dla funta brytyjskiego") +
  ylab("stopy zwrotu") 

data1 %>% ggplot(aes(x = data, y = VAR_boot_GBP,colour = "VAR")) + geom_line(size = 1) +
  geom_line(aes(y = data1$ES_boot_GBP, colour = "ES"), size = 1) +
  geom_point(aes(y = data1$stopy_GBP), col = "black", size = 2.5) + 
  ylim(0.75,3.25) + scale_colour_manual("", 
                                        breaks = c("VAR", "ES"),
                                        values = c("blue", "orange")) +
  labs(title = "Wynik metody bootstrapowej", subtitle = "dla funta brytyjskiego") +
  ylab("stopy zwrotu") 




#DKK

data1 %>% ggplot(aes(x = data, y = VAR_hist_DKK,colour = "VAR")) + geom_line(size = 1.1) +
  geom_line(aes(y = data1$ES_hist_DKK, colour = "ES"),size = 1.1) +
  geom_point(aes(y = data1$stopy_DKK), col = "black", size = 2.5) + 
  ylim(0.24,1.8) + scale_colour_manual("", 
                                    breaks = c("VAR", "ES"),
                                    values = c("blue", "orange")) +
  labs(title = "Wynik symulacji historycznej", subtitle = "dla korony duñskiej") +
  ylab("stopy zwrotu") 

data1 %>% ggplot(aes(x = data, y = VAR_hull_DKK,colour = "VAR")) + geom_line(size = 1.1) +
  geom_line(aes(y = data1$ES_hull_DKK, colour = "ES"),size = 1.1) +
  geom_point(aes(y = data1$stopy_DKK), col = "black", size = 2.5) + 
  ylim(0.24,1.8) + scale_colour_manual("", 
                                        breaks = c("VAR", "ES"),
                                        values = c("blue", "orange")) +
  labs(title = "Wynik symulacji historycznej z wagami", subtitle = "dla korony duñskiej") +
  ylab("stopy zwrotu") 

data1 %>% ggplot(aes(x = data, y = VAR_boot_DKK,colour = "VAR")) + geom_line(size = 1) +
  geom_line(aes(y = data1$ES_boot_DKK, colour = "ES"), size = 1) +
  geom_point(aes(y = data1$stopy_DKK), col = "black", size = 2.5) + 
  ylim(0,2.8) + scale_colour_manual("", 
                                        breaks = c("VAR", "ES"),
                                        values = c("blue", "orange")) +
  labs(title = "Wynik metody bootstrapowej", subtitle = "dla korony duñskiej") +
  ylab("stopy zwrotu") 



#THB

data1 %>% ggplot(aes(x = data, y = VAR_hist_THB,colour = "VAR")) + geom_line(size = 1.1) +
  geom_line(aes(y = data1$ES_hist_THB, colour = "ES"),size = 1.1) +
  geom_point(aes(y = data1$stopy_THB), col = "black", size = 2.5) + 
  ylim(0.74,2.58) + scale_colour_manual("", 
                                        breaks = c("VAR", "ES"),
                                        values = c("blue", "orange")) +
  labs(title = "Wynik symulacji historycznej", subtitle = "dla bahta tajskiego") +
  ylab("stopy zwrotu") 

data1 %>% ggplot(aes(x = data, y = VAR_hull_THB,colour = "VAR")) + geom_line(size = 1.1) +
  geom_line(aes(y = data1$ES_hull_THB, colour = "ES"),size = 1.1) +
  geom_point(aes(y = data1$stopy_THB), col = "black", size = 2.5) + 
  ylim(0.74,2.18) + scale_colour_manual("", 
                                        breaks = c("VAR", "ES"),
                                        values = c("blue", "orange")) +
  labs(title = "Wynik symulacji historycznej z wagami", subtitle = "dla bahta tajskiego") +
  ylab("stopy zwrotu") 

data1 %>% ggplot(aes(x = data, y = VAR_boot_THB,colour = "VAR")) + geom_line(size = 1) +
  geom_line(aes(y = data1$ES_boot_THB, colour = "ES"), size = 1) +
  geom_point(aes(y = data1$stopy_THB), col = "black", size = 2.5) + 
  ylim(0.74,2.18) + scale_colour_manual("", 
                                        breaks = c("VAR", "ES"),
                                        values = c("blue", "orange")) +
  labs(title = "Wynik metody bootstrapowej", subtitle = "dla bahta tajskiego") +
  ylab("stopy zwrotu") 