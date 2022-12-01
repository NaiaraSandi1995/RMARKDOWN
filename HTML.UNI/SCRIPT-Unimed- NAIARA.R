# BIBLIOTECAS
library(tidyverse)
library(tidyr)
library(dplyr)
library(expss)
library(openxlsx)
library(rio)
library(tseries)

#  DÍGITO
options(OutDec = ",", digit = 0)


################################################################################

# IMPORTANDO 


BENE = rio::import("DADOS.xlsx", which = "BASEBEN")
FIN = rio::import("DADOS.xlsx", which = "BASEFIN")
REC = rio::import("DADOS.xlsx", which = "BASEREC")

View(base)

BeneFin <- merge(BENE, FIN, by="ID")
BaseTotal <- merge(BeneFin, REC, by="ID")



#criamos uma pasta de trabalho
wb = createWorkbook() 

#Adicionamos no documento criado 

sh3 = addWorksheet(wb, "BaseTotal")

#Gravamos 

xl_write(BaseTotal, wb, sh3) # MÉDIA

#criando o arquivo xlsx

saveWorkbook(wb, "DADOSTOTAIS.xlsx", overwrite = TRUE)

#

BaseTotal$`01.06.2022`
BaseTotal$TOTAL

options(scipen = 999)

cor.test(~TOTAL + `01.06.2022`, data = BaseTotal,
         method = "spearman", continuity = F, 
         conf.level = 0.95)
# 
# Spearman's rank correlation rho
# 
# data:  TOTAL and 01.06.2022
# S = 9455485, p-value = 0,000000000003202
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#       rho 
# 0,3248271 

plot(BaseTotal$`01.06.2022`, BaseTotal$TOTAL,
     xlab = "Sexo", ylab = "Interesse porpolítica", log = "xy")


BaseTotal$`01.06.2022` <- log(BaseTotal$`01.06.2022`)
BaseTotal$TOTAL <- log(BaseTotal$TOTAL)

#DISPERSÃO2####
install.packages(ggplot2)
library(ggplot2)


# basic scatterplot
GRA1 <- ggplot(BaseTotal, aes(x=`01.06.2022`, y=TOTAL, 
                          color =`01.06.2022` , 
                          size = TOTAL)) + 
  geom_point(shape = 21, size = 3,
             fill = "black", stroke = 2, alpha = 0.2)+
  geom_count(alpha=0.3)  + theme_classic()

Gra2 <- GRA1 + theme(text = element_text(family = "serif", size = 14),
                     rect = element_blank(),
                     panel.grid = element_blank(),
                     title = element_text(color = "black"),
                     axis.line = element_line(color = "black")) +
  theme(legend.position="none")


Gra2 +  
  geom_smooth(method =  "lm", se = F, col = "red")+
  theme(text = element_text(family = "serif", size = 14),
        rect = element_blank(),
        panel.grid = element_blank(),
        title = element_text(color = "black"),
        axis.line = element_line(color = "black")) +
  theme(legend.position="none")+
  labs(y= "Total de reclamações",
       x= "Total de beneficiários")



