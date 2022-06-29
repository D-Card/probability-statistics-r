library(ggplot2)
library("openxlsx")
library("tidyr")
library("dplyr")
df1 <- read.xlsx(xlsxFile = 'QualidadeARO3.xlsx', sheet = 1, rows = 1:8785, cols = 1:10)
df1 <- df1 %>%
    mutate(across(everything(), as.double))
df1_tidy <- df1 %>%
  pivot_longer(names(.), names_to = "Estação", values_to = "O3")
df1_final <- df1_tidy %>%  
  filter(grepl('Paio-Pires|Restelo', Estação))
df1_plot <- ggplot(df1_final, aes(x = O3, fill = Estação)) +
    geom_histogram(binwidth = 3, alpha = 1, position = "dodge") +
    labs(y = "Número", x = "Nível de ozono (μg/m³)", title = "Histograma dos níveis de ozono em 2020", caption = "Baseado em QualidadeARO3.xlsx") +
    theme(plot.title = element_text(size = 18, face ="bold"))
ggsave("QualidadeARO3.png", df1_plot)