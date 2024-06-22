#########################################
# SCM

# useful link: https://github.com/edunford/tidysynth

# Clearing the environment
rm(list = ls())
options(scipen=999)


library(Synth)
library(tidysynth)
library(tidyverse)
library(dplyr)
library(tidyr)
library(haven)
library(knitr)
library(plm)
library(psych)

###### 神盾 #####
data <- read_dta('processed_data.dta')
data <- as.data.frame(data)

data <- data %>% 
  select(代號:週轉率, 天數) %>% 
  filter(天數 >= -100)
  

synth_6462 <- data %>%
  synthetic_control(outcome = 收盤價元, 
                    unit = 名稱, 
                    time = 天數, 
                    i_unit = '神盾', 
                    i_time = -30, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = -100:-30, 
                     開盤價元 = mean(開盤價元, na.rm = TRUE),
                     收盤價元 = mean(收盤價元, na.rm = TRUE),
                     最高價元 = mean(最高價元, na.rm = TRUE),
                     最低價元 = mean(最低價元, na.rm = TRUE)
                     ) %>%
  generate_weights(optimization_window = -100:-30) %>%
  generate_control()

synth_6462 %>%
  plot_trends() +
  labs(title = "神盾(6462)",
       y = "Stock Price",
       x = "Days relative to announced date") +
  geom_vline(xintercept = -30, lty = 2, color = 'red') +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  theme_classic()



synth_6462 %>% plot_differences() +
  labs(title = "Trends before and after M&A announced",
       y = "Stock Price",
       x = "Days relative to announced date")


synth_6462 %>% plot_weights()

synth_6462 %>% grab_balance_table()

synth_6462 %>% plot_placebos()

synth_6462 %>% plot_placebos(prune = FALSE)

synth_6462 %>% plot_mspe_ratio()

synth_6462 %>% grab_significance()

###### 台哥大 #####
data <- read.csv('台哥大.csv')
data <- as.data.frame(data)

data <- data %>% 
  select(代號:週轉率, 天數) %>% 
  mutate(年月日 = as.Date(年月日)) %>% 
  filter(天數 >= -100 & 天數 <= 30)

synth_3045 <- data %>%
  synthetic_control(outcome = 收盤價元, 
                    unit = 名稱, 
                    time = 天數, 
                    i_unit = '台灣大', 
                    i_time = -30, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = -100:-30, 
                     開盤價元 = mean(開盤價元, na.rm = TRUE),
                     收盤價元 = mean(收盤價元, na.rm = TRUE),
                     最高價元 = mean(最高價元, na.rm = TRUE),
                     最低價元 = mean(最低價元, na.rm = TRUE)
  ) %>%
  generate_weights(optimization_window = -100:-30) %>%
  generate_control()

synth_3045 %>%
  plot_trends() +
  labs(title = "台灣大(3045)",
       y = "Stock Price",
       x = "Days relative to announced date") +
  geom_vline(xintercept = -30, lty = 2, color = 'red') +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  theme_classic()



synth_3045 %>% plot_differences() +
  labs(title = "Trends before and after M&A announced",
       y = "Stock Price",
       x = "Days relative to announced date")


synth_3045 %>% plot_weights()

synth_3045 %>% grab_balance_table()

synth_3045 %>% plot_placebos()

synth_3045 %>% plot_placebos(prune = FALSE)

synth_3045 %>% plot_mspe_ratio()

synth_3045 %>% grab_significance()

###### 遠傳 #####
data <- read.csv('遠傳.csv')
data <- as.data.frame(data)

data <- data %>% 
  select(代號:週轉率, 天數) %>% 
  mutate(年月日 = as.Date(年月日)) %>% 
  filter(天數 >= -100 & 天數 <= 30)

synth_4904 <- data %>%
  synthetic_control(outcome = 收盤價元, 
                    unit = 名稱, 
                    time = 天數, 
                    i_unit = '遠傳', 
                    i_time = -30, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = -100:-30, 
                     開盤價元 = mean(開盤價元, na.rm = TRUE),
                     收盤價元 = mean(收盤價元, na.rm = TRUE),
                     最高價元 = mean(最高價元, na.rm = TRUE),
                     最低價元 = mean(最低價元, na.rm = TRUE)
  ) %>%
  generate_weights(optimization_window = -100:-30) %>%
  generate_control()

synth_4904 %>%
  plot_trends() +
  labs(title = "遠傳(4904)",
       y = "Stock Price",
       x = "Days relative to announced date") +
  geom_vline(xintercept = -30, lty = 2, color = 'red') +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  theme_classic()



synth_4904 %>% plot_differences() +
  labs(title = "Trends before and after M&A announced",
       y = "Stock Price",
       x = "Days relative to announced date")


synth_4904 %>% plot_weights()

synth_4904 %>% grab_balance_table()

synth_4904 %>% plot_placebos()

synth_4904 %>% plot_placebos(prune = FALSE)

synth_4904 %>% plot_mspe_ratio()

synth_4904 %>% grab_significance()


###### 富邦 #####
data <- read.csv('富邦.csv')
data <- as.data.frame(data)

data <- data %>% 
  select(代號:週轉率, 天數) %>% 
  mutate(年月日 = as.Date(年月日)) %>% 
  filter(天數 >= -100 & 天數 <= 30)

synth_2881 <- data %>%
  synthetic_control(outcome = 收盤價元, 
                    unit = 名稱, 
                    time = 天數, 
                    i_unit = '富邦金', 
                    i_time = -30, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = -100:-30, 
                     開盤價元 = mean(開盤價元, na.rm = TRUE),
                     收盤價元 = mean(收盤價元, na.rm = TRUE),
                     最高價元 = mean(最高價元, na.rm = TRUE),
                     最低價元 = mean(最低價元, na.rm = TRUE)
  ) %>%
  generate_weights(optimization_window = -100:-30) %>%
  generate_control()

synth_2881 %>%
  plot_trends() +
  labs(title = "富邦(2881)",
       y = "Stock Price",
       x = "Days relative to announced date") +
  geom_vline(xintercept = -30, lty = 2, color = 'red') +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  theme_classic()



synth_2881 %>% plot_differences() +
  labs(title = "Trends before and after M&A announced",
       y = "Stock Price",
       x = "Days relative to announced date")


synth_2881 %>% plot_weights()

synth_2881 %>% grab_balance_table()

synth_2881 %>% plot_placebos()

synth_2881 %>% plot_placebos(prune = FALSE)

synth_2881 %>% plot_mspe_ratio()

synth_2881 %>% grab_significance()


###### 開發金 #####
data <- read.csv('開發金.csv')
data <- as.data.frame(data)

data <- data %>% 
  select(代號:週轉率, 天數) %>% 
  mutate(年月日 = as.Date(年月日)) %>% 
  filter(天數 >= -100)

data$成交量千股 <- gsub(",", "", data$成交量千股) # 去掉逗號
data$成交量千股 <- as.numeric(data$成交量千股) # 轉換為數值型態
data$成交值千元 <- gsub(",", "", data$成交值千元) # 去掉逗號
data$成交值千元 <- as.numeric(data$成交值千元) # 轉換為數值型態

synth_2883 <- data %>%
  synthetic_control(outcome = 收盤價元, 
                    unit = 名稱, 
                    time = 天數, 
                    i_unit = '開發金', 
                    i_time = -30, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = -100:-30, 
                     開盤價元 = mean(開盤價元, na.rm = TRUE),
                     收盤價元 = mean(收盤價元, na.rm = TRUE),
                     最高價元 = mean(最高價元, na.rm = TRUE),
                     最低價元 = mean(最低價元, na.rm = TRUE)
                     
  ) %>%
  generate_weights(optimization_window = -100:-30) %>%
  generate_control()

synth_2883 %>%
  plot_trends() +
  labs(title = "開發金(2883)",
       y = "Stock Price",
       x = "Days relative to announced date") +
  geom_vline(xintercept = -30, lty = 2, color = 'red') +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  theme_classic()



synth_2883 %>% plot_differences() +
  labs(title = "",
       y = "Stock Price",
       x = "Days relative to announced date")


synth_2883 %>% plot_weights()

synth_2883 %>% grab_balance_table()

synth_2883 %>% plot_placebos()

synth_2883 %>% plot_placebos(prune = FALSE)

synth_2883 %>% plot_mspe_ratio()

synth_2883 %>% grab_significance()

synth_2883 %>% grab_outcome()

deal_2883 <- data %>%
  synthetic_control(outcome = 成交量千股, 
                    unit = 名稱, 
                    time = 天數, 
                    i_unit = '開發金', 
                    i_time = -30, 
                    generate_placebos = TRUE) %>%
  generate_predictor(time_window = -100:-30, 
                     開盤價元 = mean(開盤價元, na.rm = TRUE),
                     收盤價元 = mean(收盤價元, na.rm = TRUE),
                     最高價元 = mean(最高價元, na.rm = TRUE),
                     最低價元 = mean(最低價元, na.rm = TRUE),
                     成交值千元 = mean(成交值千元, na.rm = TRUE)
  ) %>%
  generate_weights(optimization_window = -100:-30) %>%
  generate_control()


deal_2883 %>%
  plot_trends() +
  labs(title = "開發金(2883)",
       y = "成交量(千股)",
       x = "相對日期") +
  geom_vline(xintercept = -30, lty = 2, color = 'red') +
  geom_vline(xintercept = 0, lty = 2, color = 'red') +
  theme_classic()




pdf <- pdata.frame(data, c("代號", "年月日"))
summary(pdf)
