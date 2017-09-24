econ_data <- read.csv('index2017.csv')
summary(econ_data)
colnames(econ_data)
library(ggplot2) 
#Judical Effectiveness
ggplot(data = econ_data, aes(x = econ_data$Judical.Effectiveness, y = econ_data$Government.Integrity)) +
  geom_smooth(method = "lm", color="red") +
  geom_point()+labs(title = "Judical Effectiveness and Government Integrity", x="Judical Effectiveness",
                    y='Government Integrity')

#Government Integrity and Spending
ggplot(data = econ_data, aes(x = econ_data$Government.Integrity, y = econ_data$Gov.t.Spending)) +
  geom_smooth(method = "lm", color="red") +
  geom_point()+labs(title = "Judical Effectiveness and Government Integrity", x="Judical Effectiveness",
                    y='Government Integrity')

#Government Integrity and Spending
ggplot(data = econ_data, aes(x = econ_data$Unemployment, y = econ_data$Labor.Freedom)) +
  geom_smooth(method = "lm", color="red") +
  geom_point()+labs(title = "Unemployment and Labor Freedom", x="Labor Freedom",
                    y='Unemployment')
