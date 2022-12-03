#Damage Carrots data set

#Phelps, K. (1982). Use of the complementary log-log function to describe doseresponse relationships in insecticide evaluation field trials.
#In R. Gilchrist (Ed.), Lecture Notes in Statistics, No. 14. GLIM.82: Proceedings of the International Conference on Generalized Linear Models; Springer-Verlag.
#McCullagh P. and Nelder, J. A. (1989) Generalized Linear Models. London: Chapman and Hall.

## success - es cantidad de zanahorias con daño hecho por insectos.
## total - es el total de zanahorias por unidad experimental
## logdose - es un vector numérico que da el log() de la dosis (hay sólo 8 niveles)
## block - un factor (va de B1 a B3)

#####librerías, dataset y split
#install.packages("robustbase")
#install.packages("robust")

library(tidymodels)
library(robustbase)
library(robust)

data(carrots)
carrots$infected = as.numeric(carrots$success/carrots$total > .2) #creo una variable dicotómica "infectado" si o no
#str(carrots)
View(carrots)

#### sección de código de la cátedra, para split train test
# fijamos semilla
set.seed(5)
# Partición Train y Test, indicando proporción
train_test <- initial_split(carrots, prop = 0.75)
train_data <- training(train_test)
test_data <- testing(train_test)
####

#####visualización

## ploteo daño de insectos en función de la dosis de insecticida
plot(success ~ logdose, data = train_data, col = as.integer(block))

plot(infected ~ logdose, data = train_data, col = as.integer(block))

## ploteo lo mismo pero separado por block
coplot(success ~ logdose | block, data = train_data)


################modelos regresion logistica############

Stan <- glm(infected ~ logdose + block,
             data = train_data, family = binomial(link="logit"))
summary.glm(Stan)


Rob <- glmrob(infected ~ logdose + block,
                family = binomial(link="logit"), data = train_data, method= "Mqle",
                control= glmrobMqle.control(tcc=1.2))
summary.glmRob(Rob)


######################ploteos
plot(Stan)
plot.glmRob(Stan)
plot.glmRob(Rob)

#ploteos caseros (solo si se necesita)
#plot(Stan$fitted.values, Stan$residuals)
#plot(Rob$fitted.values, Rob$residuals) #falta completar, estandarizar residuos, etc


######## rendimiento en test
pred_Stan = augment(Stan, newdata = test_data) 
pred_Rob = augment(Rob, newdata = test_data) 

#pred_Stan <- predict(Stan,newdata = test_data, type="response")
#pred_Rob <- predict(Stan,newdata = test_data, type="response")

rmse(data = pred_Stan, truth = infected, estimate = .fitted)
rmse(data = pred_Rob, truth = infected, estimate = .fitted)

mae(data = pred_Stan, truth = infected, estimate = .fitted)
mae(data = pred_Rob, truth = infected, estimate = .fitted)



########################### family = gaussian (predigo success en vez de infected) ###
Stan <- glm(success ~ logdose + block,
            data = train_data, family = gaussian)
summary.glm(Stan)


Rob <- glmrob(success ~ logdose + block,
              family = gaussian, data = train_data, method= "Mqle",
              control= glmrobMqle.control(tcc=1.2))
summary.glmRob(Rob)

#######################


######################ploteos
plot(Stan)
plot.glmRob(Stan)
plot.glmRob(Rob)


# testeo
pred_Stan = augment(Stan, newdata = test_data) 
pred_Rob = augment(Rob, newdata = test_data) 

#pred_Stan <- predict(Stan,newdata = test_data, type="response")
#pred_Rob <- predict(Stan,newdata = test_data, type="response")

rmse(data = pred_Stan, truth = success, estimate = .fitted)
rmse(data = pred_Rob, truth = success, estimate = .fitted)

mae(data = pred_Stan, truth = success, estimate = .fitted)
mae(data = pred_Rob, truth = success, estimate = .fitted)

plot(success ~ logdose, data = test_data, col = as.integer(block))
plot(.fitted ~ logdose, data = pred_Stan, col = "red")
plot(.fitted ~ logdose, data = pred_Rob, col = "blue")

glance(Rob)

