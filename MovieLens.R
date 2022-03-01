library(tidyverse)
library(dslabs)
library(caret)
library(lubridate)
data("movielens")

#para mirar como tabla
movielens %>% as.tibble()

#usuarios unicos
movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

set.seed(755)
test_index <- createDataPartition(y = movielens$rating,
                                  times = 1, p = 0.2,
                                  list = FALSE)

train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#se deben eliminar en el set de evaluación las peliculas
#que no tienen evaluación en el set de entrenamiento

test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#primer modelo de recomendación
#promedio de todas las calificaciones de las peliculas
mu_hat <- mean(train_set$rating)
mu_hat

#se aplica a todas las calificaciones desconocidas el mu_hat
naive_rmse <- RMSE(test_set$rating,mu_hat)
naive_rmse

#ese resultado es muy inocente y se puede mejorar
#pero que se ponga el resultado en una tabla para comparar

rmse_results <- tibble(method = "Solo el promedio", RMSE = naive_rmse)

rmse_results
#PERO EL RESULTADO DEL PROMEDIO AÚN ES MEJOR QUE UN RESULTADO AL AZAR
#por ejemplo si se replica el 3 en cada una de las peliculas
#que tienen rating y luego se compara con lo efectivamente puesto

alazar <- rep(3, nrow(test_set))
RMSE(test_set$rating, alazar)

#?lm
#midiendo con modelo lineal para 
#ver por promedio simple las calificaciones
#de cada una de las películas
#pero no es lo mejor con lm porque tarda mucho
#fit <- lm(rating ~ as.factor(movieId), data = movielens)

#se podría hacer con un promedio simple de cada una de las películas
mu <- mean(train_set$rating)
mu
#si restamos el mu del ratig. querria
#decir que la calificación máxima sería 1.5

movie_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating  - mu))

qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings <- mu + test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  pull (b_i)

RMSE(predicted_ratings, test_set$rating)

#ya existe mejora a un promedio simple

#ver si hay algún efecto de usuario
#cuál es la calificación promedio de aquellos usuarios
#que han calificado más de 100 películas

train_set %>%
  group_by(userId) %>%
  filter(n()>=100) %>%
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black")

#entonces lo que se tendría que hacer es 
#es una modelación lineal lm 
#lm(rating ~ as.factor(movieId) + as.factor(userId))

#pero sería lento, entonces lo que se hará es una aproximación
#calculo de mu y rating de peli y estimando al usuario

user_avgs <- train_set %>%
  left_join(movie_avgs, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

#ahora construyendo los predictores y ver si mejora el RMSE
predicted_ratings <- test_set %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(user_avgs, by= "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

RMSE(predicted_ratings, test_set$rating)


#ME HE QUEDADO EN LA PAGINA 679
#EL APARTADO 33.8
#Q1. Compute the number of ratings for each movie and then plot it against the year the movie came out. Use the square root transformation on the counts.
#What year has the highest median number of ratings?

movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#2. Vemos que, en promedio, las películas que salieron después de 1993 obtienen más calificaciones. También vemos que con las películas más nuevas, a partir de 1993, el número de
#calificaciones disminuye con el año: entre más reciente sea una película, menos tiempo han
#tenido los usuarios para calificarla.
#Entre las películas que salieron en 1993 o más tarde, ¿cuáles son las 25 películas con más
#calificaciones por año? Además, indique la calificación promedio.

movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#3. De la tabla construida en el ejemplo anterior, vemos que las películas mejor calificadas
#tienden a tener calificaciones superiores al promedio. Esto no es sorprendente: más personas
#ven películas populares. Para confirmar esto, estratifique las películas posteriores a 1993 por
#calificaciones por año y calcule sus calificaciones promedio. Haga un gráfico de la calificación
#promedio versus calificaciones por año y muestre un estimador de la tendencia.

movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#Q5. The movielens dataset also includes a time stamp. This variable represents the time and data in which the rating was provided. The units are seconds since January 1, 1970. Create a new column date with the date.

movielens <- mutate(movielens, date = as_datetime(timestamp))

head(movielens)

#6. Calcule la calificación promedio de cada semana y calcule este promedio para cada día.
#Sugerencia: use la función round_date antes de group_by.

movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#7. El gráfico muestra alguna evidencia de un efecto temporero. Si definimos 𝑑𝑢,𝑖 como el día
#que el usuario 𝑢 hizo su calificación de la película 𝑖, ¿cuál de los siguientes modelos es el
#más apropiado?

#d. 𝑌𝑢,𝑖 = 𝜇 + 𝑏𝑖 + 𝑏𝑢 + 𝑓(𝑑𝑢,𝑖) + 𝜀𝑢,𝑖, con 𝑓 una función suave de 𝑑𝑢,𝑖.

#8 8. Los datos movielens también tienen un columna genres. Esta columna incluye todos
#los géneros que aplican a la película. Algunas películas pertenecen a varios géneros. Defina
#una categoría como cualquier combinación que aparezca en esta columna. Mantenga solo
#categorías con más de 1,000 calificaciones. Luego, calcule el promedio y error estándar para
#cada categoría. Grafique estos usando diagramas de barras de error.

movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>%
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) +
  geom_point() +
  geom_errorbar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5))

#33.9 Regularización
#probemos con sólo el b_i para seleccionar los errores de peliculas
test_set %>%
  left_join(movie_avgs, by= "movieId") %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%
  slice(1:10) %>%
  pull (title)

test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>%  
  slice(1:10) %>% 
  pull(title)

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()

#segun el primer estimador, las 10 mejores peliculas

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  slice(1:10)  %>% 
  pull(title)

#segun el primer estimador, las 10 peores peliculas

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  slice(1:10)  %>% 
  pull(title)

#todas desconocidas, ver la frecuencia de calificación

train_set %>% count(movieId) %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by= "movieId") %>%
  arrange(desc(b_i)) %>%
  slice(1:10) %>%
  pull(n)

train_set %>% count(movieId) %>%
  left_join(movie_avgs, by="movieId") %>%
  left_join(movie_titles, by= "movieId") %>%
  arrange(b_i) %>%
  slice(1:10) %>%
  pull(n)

#penalizando a las pelis que no tienen muchas valoraciones

lambda <- 3

mu <- mean(train_set$rating)

#regularizando los promedios de las peliculas
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

#comparación de los promedios simples y los regularizados

tibble(original = movie_avgs$b_i,
       regularizado = movie_reg_avgs$b_i,
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularizado, size=sqrt(n))) +
  geom_point(shape=1, alpha=0.5)

#ahora veamos las 10 mejores peliculas
train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>%
  slice(1:10) %>%
  pull(title)

#las peores peliculas
train_set %>%
  count(movieId) %>%
  left_join(movie_reg_avgs, by = "movieId") %>%
  left_join(movie_titles, by = "movieId") %>%
  arrange(b_i) %>%
  slice(1:10) %>%
  pull(title)

#los resultados mejoraron?
predicted_ratings_con_penalizacion <- test_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull (pred)
RMSE(predicted_ratings_con_penalizacion, test_set$rating)

#pero como se podría elegir la cantidad para penalizar
#como podríamos saber los valores de ajuste lambda
#validación cruzada

lambdas <- seq(0,10,0.25)
mu <- mean(train_set$rating)
solo_la_suma <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(solo_la_suma, by="movieId") %>%
    mutate(b_i = s/(n_i+1)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by='movieId') %>%
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)
lambdas[which.min(rmses)]

#el anterior ejemplo solo es para fines ilustrativos. en realdiad se tendría que hacer con solo el set de entrenamiento

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <-
    test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)

lambda <- lambdas[which.min(rmses)]
lambda

#ME QUEDE EN 33.11 PAGINA 688