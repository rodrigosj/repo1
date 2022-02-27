library(tidyverse)
library(dslabs)
library(caret)
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