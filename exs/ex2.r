library(ggplot2)
library("openxlsx")
library("tidyr")
library("dplyr")
library("stringr")
df1 <- read.xlsx(xlsxFile = 'EsperancaVida.xlsx', sheet = 1, rows = 9:69, cols = 1:103)
names(df1)[1] <- c("Ano")
names(df1)[2:35] <- paste("Total", names(df1)[2:35], sep = " | ")
names(df1)[36:69] <- paste("Homens", names(df1)[36:69], sep = " | ")
names(df1)[70:103] <- paste("Mulheres", names(df1)[70:103], sep = " | ")
df1_tidy <- df1 %>%
  pivot_longer(names(.)[2:103], names_to = "País", values_to = "Esperança de vida")
df1_final <- df1_tidy %>%  
filter(grepl('NO|AT|CH', País)) %>%  
filter(!grepl('Total', País))
df1_final <- df1_final %>%
filter(Ano %in% c(2002:2019))
df1_final$País <- str_replace_all(df1_final$País, "\\.", " ")
df1_plot <- ggplot(df1_final, aes(x = Ano, y = `Esperança de vida`, colour = País)) +
    labs(y = "Esperança de vida (anos)", title = "Evolução da esperança média de vida", caption = "Baseado em EsperancaVida.xlsx") +
    theme(plot.title = element_text(size = 18, face ="bold")) +
    geom_line() +
    geom_point(size = 1)
ggsave("EsperancaVida.png", df1_plot)
