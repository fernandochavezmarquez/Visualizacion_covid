#@author Fernando Chávez
#Proyecto R covid19

#Instalaremos las paqueterias necesarias para empezar
install.packages("tidyverse")
library(tidyverse)
install.packages("COVID19")
library(COVID19)
glimpse(df)
library(forecast)

#Definiremos nuestro data frame el cual sera la tabla covid19
df <- covid19()
df

#Seleccionaremos en un vector a los paises que queremos
paises <- c("MEX","ARG","BRA","COL","CAN")
paises

colores <- c("#2973b4", "darkblue", "#0363c3", "#e64385", "#46732eff")
colores
#Grafica 1 

df %>%
  filter(id %in% paises)%>%
  select(date,confirmed, population)%>%
  arrange(date)%>%
  group_by(id)%>%
  mutate(new_confirmed = c(0,diff(confirmed)))%>%
  mutate(new_confirmed = (1e6)*new_confirmed/population)%>%
  mutate(new_confirmed_smooth = ma(new_confirmed,order = 7))%>%
  ggplot(aes(x=date,y=new_confirmed_smooth, col=id))+
  ylim(0,NA)+
  geom_line()+
  labs(x="Fecha",y="Nuevos casos confirmados",
  title="Evolución de nuevos casos",
  subtitle = "Casos por millon de habitantes")+
  scale_color_manual(values=colores)+
  theme_elegante()

#Grafica 2

df %>%
  filter(id %in% paises)%>%
  select(date,deaths, population)%>%
  arrange(date)%>%
  group_by(id)%>%
  mutate(new_deaths = c(0,diff(deaths)))%>%
  mutate(new_deaths = (1e6)*new_deaths/population)%>%
  mutate(new_deaths_smooth = ma(new_deaths,order = 7))%>%
  ggplot(aes(x=date,y=new_deaths_smooth, col=id))+
  ylim(0,NA)+
  geom_line()+
  labs(x="Fecha",y="Fallecimiento",
       title="Evolucion de nuevos fallecimientos",
       subtitle = "fallecimientos por millon de habitantes")+
  scale_color_manual(values=colores)+
  theme_elegante()
  


  ggplot(aes# CREATING PLOT THEMES
#######################
(x=detahs,y=new_confirmed_12, col=id))+
  ylim(0,NA)+
  geom_line()+
  labs(x="Fecha",y="Nuevos casos confirmados",
       title="Evolucion de nuevos casos",
       subtitle = "Casos por millon de habitantes")+
  scale_color_manual(values=c("#85660D","#782AB6","#565656","#1C8356","#C4451C"))+
  theme_elegante()


install.packages("fpp2")
library(fpp2)

#######################
library(extrafont)

# Create Base Theme
#------------------
theme_elegante <- function(base_size = 10,
                           base_family = "Raleway"
)
{
  color.background = "#FFFFFF" # Chart Background
  color.grid.major = "#D9D9D9" # Chart Gridlines
  color.axis.text = "#666666" # 
  color.axis.title = "#666666" # 
  color.title = "#666666"
  color.subtitle = "#666666"
  strip.background.color = '#9999CC'
  
  ret <-
    theme_bw(base_size=base_size) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.55, linetype="dotted")) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=base_size-3,color=color.axis.title, family = base_family)) +
    
    theme(strip.text.x = element_text(size=base_size,color=color.background, family = base_family)) +
    theme(strip.text.y = element_text(size=base_size,color=color.background, family = base_family)) +
    #theme(strip.background = element_rect(fill=strip.background.color, linetype="blank")) +
    theme(strip.background = element_rect(fill = "grey70", colour = NA)) +
    # theme(panel.border= element_rect(fill = NA, colour = "grey70", size = rel(1)))+
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, 
                                  size=20, 
                                  vjust=1.25, 
                                  family=base_family, 
                                  hjust = 0.5
    )) +
    
    theme(plot.subtitle=element_text(color=color.subtitle, size=base_size+2, family = base_family,  hjust = 0.5))  +
    
    theme(axis.text.x=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(axis.text.y=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(text=element_text(size=base_size, color=color.axis.text, family = base_family)) +
    
    theme(axis.title.x=element_text(size=base_size+2,color=color.axis.title, vjust=0, family = base_family)) +
    theme(axis.title.y=element_text(size=base_size+2,color=color.axis.title, vjust=1.25, family = base_family)) +
    theme(plot.caption=element_text(size=base_size-2,color=color.axis.title, vjust=1.25, family = base_family)) +
    
    # Legend  
    theme(legend.text=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.title=element_text(size=base_size,color=color.axis.text, family = base_family)) +
    theme(legend.key=element_rect(colour = color.background, fill = color.background)) +
    theme(legend.position="bottom", 
          legend.box = "horizontal", 
          legend.title = element_blank(),
          legend.key.width = unit(.75, "cm"),
          legend.key.height = unit(.75, "cm"),
          legend.spacing.x = unit(.25, 'cm'),
          legend.spacing.y = unit(.25, 'cm'),
          legend.margin = margin(t=0, r=0, b=0, l=0, unit="cm")) +
    
    # Plot margins
    theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
  
  ret
}
