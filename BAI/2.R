setwd("C:/Users/Kristina/Documents/BAI")

library(graphics)
library(stats)
library(utils)
library(dplyr)   # Для манипуляции данными
library(ggplot2) # Для визуализации данных

# Загрузка данных (замените "data.csv" на имя вашего файла данных)
data <- read.csv("data.csv")

# Предполагаемые столбцы в данных:
# - tree_id: уникальный идентификатор дерева
# - year: год наблюдения
# - basal_area: базальная площадь в данном году

# Функция для вычисления прироста базальной площади
calculate_BAI <- function(data) {
  data <- data %>% 
    arrange(tree_id, year) %>%  # Сортировка данных по идентификатору дерева и году
    group_by(tree_id) %>%        # Группировка данных по идентификатору дерева
    mutate(BAI = c(NA, diff(basal_area))) %>% # Вычисление прироста базальной площади
    ungroup()                     # Снятие группировки
  return(data)
}

# Вызов функции для вычисления BAI
data_with_BAI <- calculate_BAI(data)

# Визуализация BAI
ggplot(data_with_BAI, aes(x = year, y = BAI)) +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE) + # Добавление сглаженной кривой
  labs(title = "Basal Area Increment Over Time", x = "Year", y = "BAI") +
  theme_minimal()


## Use gp data
data("C:/Users/Kristina/Documents/BAI/APA.rvl")
data(gp.d2pith)
foo <- bai.in(rwl = gp.rwl, d2pith = gp.d2pith)
foo.crn <- chron(foo)
yrs <- time(foo.crn)
plot(yrs, foo.crn[, 1], type = "n",
     xlab = "Year", ylab = expression(mm^2))
lines(yrs, foo.crn[, 1], col = "grey", lty = "dashed")
lines(yrs, caps(foo.crn[, 1], nyrs = 32), col = "red", lwd = 2)