library(ggplot2)
library("openxlsx")
df1 <- read.xlsx(xlsxFile = "Utentes.xlsx", sheet = 1, rows = 1:77, cols = 1:4)
df1_plot <- ggplot(df1, aes(x = Colesterol, y = TAD)) +
    geom_point(shape = 8, size = 1.5, color = "blue") +
    stat_smooth(method = loess) +
    labs(y = "Colesterol", x = "TAD", title = "Gráfico de disperção - Colesterol vs TAD", caption = "Baseado em Utentes.xlsx") +
    theme(plot.title = element_text(size = 18, face ="bold"))
ggsave("Utentes.png", df1_plot)
