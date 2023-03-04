library(tidymodels)
library(rsample)
#install.packages("ggcorrplot")
library("ggcorrplot")
#install.packages("ggmosaic")
library("ggmosaic")
library(gridExtra) 
library(ggfortify)
library("GGally")
library(MASS)
library(tidyverse)
#install.packages("gridExtra")

# dataset <- read.csv("./Datasets/encuesta_salud_train.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 
# 
# # fijamos semilla
# set.seed(311)
# # Partición Train y Test, indicando proporción
# train_test <- initial_split(dataset, prop = 0.75)
# dtrain <- training(train_test)
# dtest <- testing(train_test)

dtrain <- read.csv("./Datasets/encuesta_salud_train.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 
dtest  <- read.csv("./Datasets/encuesta_salud_test.csv" , encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 

summary(dtrain)
names(which(colSums(is.na(dtrain))>0))

dtrain %>% ggplot(aes(edad)) +
            geom_histogram()

dtrain %>% ggplot(aes(genero)) +
            geom_bar()

dtrain %>% ggplot(aes(y = nivel_educativo)) +
  geom_bar() 

dtrain %>% ggplot(aes(altura)) +
  geom_histogram()

dtrain %>% ggplot(aes(peso)) +
  geom_histogram()

dtrain %>% ggplot(aes(y = frecuencia_hambre_mensual)) +
  geom_bar()

dtrain %>% ggplot(aes(dias_consumo_comida_rapida)) +
  geom_histogram()

dtrain %>% ggplot(aes(y = edad_consumo_alcohol)) +
  geom_bar()

dtrain %>% ggplot(aes(consumo_diario_alcohol)) +
  geom_histogram()

dtrain %>% ggplot(aes(dias_actividad_fisica_semanal)) +
  geom_histogram()

dtrain %>% ggplot(aes(y = consumo_semanal_frutas)) +
  geom_bar()

dtrain %>% ggplot(aes(y = consumo_semanal_verdura)) +
  geom_bar()

dtrain %>% ggplot(aes(y = consumo_semanal_gaseosas)) +
  geom_bar()

dtrain %>% ggplot(aes(y = consumo_semanal_snacks)) +
  geom_bar()

dtrain %>% ggplot(aes(y = consumo_semanal_comida_grasa)) +
  geom_bar()

dtrain %>% select(where(is.numeric)) %>%
            cor()

dtrain %>% filter(genero == "Femenino") %>%
  select(where(is.numeric)) %>%
  cor() %>%
  ggcorrplot()

dtrain %>% filter(genero == "Masculino") %>%
  select(where(is.numeric)) %>%
  cor() %>%
  ggcorrplot()


dtrain %>% select(edad,altura,peso,dias_consumo_comida_rapida, consumo_diario_alcohol,dias_actividad_fisica_semanal,genero) %>%
  ggpairs(aes(color = genero, alpha = 0.5))

# dtrain %>% ggplot(aes(x = consumo_semanal_verdura))  +
#   facet_wrap(~frecuencia_hambre_mensual) +
#   geom_bar(aes(y = after_stat(count / sum(count))))
# 
# dtrain %>% ggplot()  +
#   geom_bar(aes(x = consumo_semanal_verdura, y = after_stat(count / sum(count)), fill = frecuencia_hambre_mensual)) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# 
# dtrain %>% ggplot()  +
#   geom_bar(aes(x = frecuencia_hambre_mensual, y = after_stat(count / sum(count)))) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   facet_wrap(~consumo_semanal_verdura)
# 
# dtrain %>% ggplot()  +
#   geom_bar(aes(x = frecuencia_hambre_mensual, y = density)) + 
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   facet_wrap(~consumo_semanal_verdura)

dtrain %>% ggplot() +
   geom_mosaic(aes(x=product(frecuencia_hambre_mensual,consumo_semanal_verdura), fill = frecuencia_hambre_mensual),show.legend = FALSE) + 
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 

dtrain %>% ggplot() +
  geom_mosaic(aes(x = product(consumo_semanal_verdura), fill=consumo_semanal_verdura), divider = "vspine") + 
  facet_grid(~frecuencia_hambre_mensual) +
  theme(aspect.ratio = 3,
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

#install.packages("tidymodels")
first_model <- lm(peso ~ altura + edad + genero + dias_actividad_fisica_semanal + consumo_diario_alcohol, data = dtrain)
tidy_first_model <- tidy(first_model, conf.int = TRUE)
tidy_first_model

glance(first_model)
summary(first_model)

#sugerencia: añadir gráfico de coeficientes como se vio en la clase 7

dtrain <- dtrain %>%
  mutate(consumo_semanal_snacks = relevel(consumo_semanal_snacks,ref = "No comí comida salada o snacks en los últimos 7 días"))
dtest <- dtest %>%
  mutate(consumo_semanal_snacks = relevel(consumo_semanal_snacks,ref = "No comí comida salada o snacks en los últimos 7 días"))

cat_model <- lm(peso ~ altura + edad + genero + consumo_semanal_snacks + genero*edad, data = dtrain)
tidy_cat_model <- tidy(cat_model, conf.int = TRUE)
tidy_cat_model

glance(cat_model)

tidy(anova(cat_model))

dtrain <- dtrain %>%
  mutate(consumo_semanal_snacks2 = case_when(
    consumo_semanal_snacks == "No comí comida salada o snacks en los últimos 7 días" ~ "Infrecuente",
    consumo_semanal_snacks == "1 a 3 veces durante los últimos 7 días" ~ "Moderado",
    !(consumo_semanal_snacks %in% c("No comí comida salada o snacks en los últimos 7 días","1 a 3 veces durante los últimos 7 días")) ~ "Frecuente"
  )) %>%
  mutate(consumo_semanal_snacks2 = relevel(as.factor(consumo_semanal_snacks2),ref="Infrecuente"))
dtest <- dtest %>%
  mutate(consumo_semanal_snacks2 = case_when(
    consumo_semanal_snacks == "No comí comida salada o snacks en los últimos 7 días" ~ "Infrecuente",
    consumo_semanal_snacks == "1 a 3 veces durante los últimos 7 días" ~ "Moderado",
    !(consumo_semanal_snacks %in% c("No comí comida salada o snacks en los últimos 7 días","1 a 3 veces durante los últimos 7 días")) ~ "Frecuente"
  )) %>%
  mutate(consumo_semanal_snacks2 = relevel(as.factor(consumo_semanal_snacks2),ref="Infrecuente"))

cat_model2 <- lm(peso ~ altura + edad + genero + consumo_semanal_snacks2 + genero*edad, data = dtrain)
tidy_cat_model2 <- tidy(cat_model2, conf.int = TRUE)
tidy_cat_model2

glance(cat_model2)

tidy(anova(cat_model2))

dtrain <- dtrain %>%
  mutate(inverso_cuadrado_altura = 1/altura**2)
dtest <- dtest %>%
  mutate(inverso_cuadrado_altura = 1/altura**2)


extra_model <- lm(peso ~ inverso_cuadrado_altura + edad + genero, data = dtrain)
tidy_extra_model <- tidy(extra_model, conf.int = TRUE)
tidy_extra_model

glance(extra_model)

extra_model2 <- lm(peso ~ altura + edad + genero + dias_actividad_fisica_semanal + dias_consumo_comida_rapida + consumo_semanal_comida_grasa, data = dtrain)
tidy_extra_model2 <- tidy(extra_model2, conf.int = TRUE)
tidy_extra_model2

glance(extra_model2)

models <- list(first_model = first_model,cat_model = cat_model,cat_model2 = cat_model2, extra_model = extra_model, extra_model2 = extra_model2)

df_eval = map_df(models, glance, .id = "model") %>%
  # ordenamos por R2 ajustado
  arrange(desc(adj.r.squared)) %>%
  select(model,r.squared,adj.r.squared)

df_eval

pred_list_train = map(.x = models, .f = augment)
rmse_train <- map_dfr(.x = pred_list_train, .f = rmse, truth = peso, estimate = .fitted, .id="model") %>% arrange(.estimate)
rmse_train

mae_train <- map_dfr(.x = pred_list_train, .f = mae, truth = peso, estimate = .fitted, .id="model") %>% arrange(.estimate)
mae_train

pred_list_test = map(.x = models, .f = augment, new_data = dtest)
rmse_test <- map_dfr(.x = pred_list_test, .f = rmse, truth = peso, estimate = .fitted, .id="model") %>% arrange(.estimate)
rmse_test

mae_test <- map_dfr(.x = pred_list_test, .f = mae, truth = peso, estimate = .fitted, .id="model") %>% arrange(.estimate)
mae_test

aug_first_model <- augment(first_model)
g1 = ggplot(aug_first_model, 
            aes(.fitted, .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  geom_smooth(se = FALSE) +
  labs(title = "Residuos vs valores predichos") + 
  theme_bw()
g2 = ggplot(aug_first_model, 
            aes(sample = .std.resid)) +
  stat_qq() +
  geom_abline() +
  labs(title = "Normal QQ plot") + 
  theme_bw()
g3 = ggplot(aug_first_model, 
            aes(.fitted, sqrt(abs(.std.resid)))) +
  geom_point() +
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Scale-location plot")
g4 = ggplot(aug_first_model, 
            aes(.hat, .std.resid)) +
  geom_vline(size = 2, colour = "white", xintercept = 0) +
  geom_hline(size = 2, colour = "white", yintercept = 0) +
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_bw() +
  labs(title = "Residual vs leverage")
# grafico todos juntos
grid.arrange(g1, g2, g3, g4, nrow = 2)

aug_first_model %>%  filter(.hat>0.4) #verificar a partir de qué valor se considera que hay leverage

autoplot(first_model, nrow = 2) #alternativa


dataset2 <- read.csv("./Datasets/encuesta_salud_modelo6.csv", encoding="UTF-8",stringsAsFactors = TRUE, row.names ="record") 

dataset2 %>% select(edad,altura,peso,dias_consumo_comida_rapida, consumo_diario_alcohol,dias_actividad_fisica_semanal,genero) %>%
  ggpairs(aes(color = genero, alpha = 0.5))

first_model2 <- lm(peso ~ altura + edad + genero + dias_actividad_fisica_semanal + consumo_diario_alcohol, data = dataset2)
tidy_first_model2 <- tidy(first_model2, conf.int = TRUE)
tidy_first_model2

glance(first_model2)
summary(first_model2)

first_model2 %>% augment() %>% metrics(truth = peso, estimate = .fitted)

robust_model <- rlm(peso ~ altura + edad + genero + dias_actividad_fisica_semanal + consumo_diario_alcohol, data = dataset2)
tidy_robust_model <- tidy(robust_model, conf.int = TRUE)
tidy_robust_model

glance(robust_model)
summary(robust_model)

robust_model %>% augment() %>% metrics(truth = peso, estimate = .fitted)

robust_model2 <- rlm(peso ~ altura + edad + genero + dias_actividad_fisica_semanal + consumo_diario_alcohol, data = dataset2, psi = psi.bisquare)
tidy_robust_model2 <- tidy(robust_model2, conf.int = TRUE)
tidy_robust_model2

glance(robust_model2)
summary(robust_model2)

robust_model2 %>% augment(newdata = dataset2) %>% metrics(truth = peso, estimate = .fitted)
