library(ggplot2)
library("openxlsx")
library("tidyr")
library("dplyr")
df1 <- read.xlsx(xlsxFile = 'ResiduosPerCapita.xlsx', sheet = 1, rows = 12:43, cols = 1:3)
names(df1) <- c("País", "2004", "2018")
df1_tidy <- df1 %>%
  pivot_longer(c("2004", "2018"), names_to = "Ano", values_to = "Resíduos")
df1_final <- df1_tidy %>%
filter(País %in% c("CY - Chipre", "IE - Irlanda", "IS - Islândia"))
df1_plot <- ggplot(df1_final, aes(x = País, y = Resíduos, fill = Ano)) + geom_col(position = "dodge") +
geom_text(aes(label = Resíduos), size = 6, vjust = 1.5, colour = "white", position = position_dodge(0.9))
df1_plotfinal <- df1_plot +labs(x = "País", y = "Resíduos (ton)", title = "Produção de resíduos per capita", caption = "Baseado em ResiduosPerCapita.xlsx") + 
 theme(axis.text.x = element_text(angle = 20, size = 12), axis.text.y = element_text(size = 12), plot.title = element_text(size = 18, face ="bold"))
ggsave("ResiduosPerCapita.png", df1_plotfinal)