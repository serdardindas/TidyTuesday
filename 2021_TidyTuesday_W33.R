## TidyTuesday 2021 Week-33 /  BEA Infrastructure Investment

#Get Data
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')

library(dplyr)
library(ggplot2)
library(ggtext)
library(forcats)
library(patchwork)
library(showtext)
library(pals)

font_add_google("IBM Plex Serif", "IBMserif")

# Create doughnut chart ready dataframe for top 10 categories with; total investment, percentage and color(#Hex)
graph_data <- chain_investment %>% 
  group_by(meta_cat) %>% 
  summarise(total_investment=sum(gross_inv_chain)) %>% 
  mutate(inv_pct=round(total_investment/sum(total_investment)*100)) %>% 
  arrange(desc(inv_pct)) %>% 
  head(10) %>% 
  mutate(rect_x_max = cumsum(inv_pct),
         rect_x_min = rect_x_max - inv_pct)%>%
  mutate(meta_cat=fct_reorder(meta_cat,desc(total_investment))) %>% 
  mutate(group_color=cubicyf(n = 10))


# I define a function that creates special doughnut charts by categories
# Function arguments; main_group= use categories defined meta-cat column,
#                     pct_place= defines how far % value far from chart

##Function##
group_graph <- function(main_group, pct_place=12) {
  
  
single_part <- graph_data %>% filter(meta_cat==main_group) 
other_parts <- data.frame(other=c("first", "second"),
                          rect_x_max=c(single_part$rect_x_min, 96),
                          rect_x_min=c(0,single_part$rect_x_max))
  

single_part$meta_cat <- stringr::str_wrap(single_part$meta_cat, 13)
  
single_part%>% 
    ggplot() +
    
    geom_rect(
      aes(
        ymin = 1,
        ymax = 10,
        xmin = rect_x_min,
        xmax = rect_x_max,
          ),
      colour="black",
      fill=single_part$group_color ,
      size=2,) +
    
    coord_polar(theta = "x") +
    ylim(-40, 40) +
    xlim(0,96) +
    
    geom_rect(data=other_parts,
      aes(
        ymin = 1,
        ymax = 10,
        xmin = rect_x_min,
        xmax = rect_x_max
          ), 
      colour="black",
      fill = 'white') +
    
    geom_text(
      aes(
        x = 0, 
        y = 35,
        label = stringr::str_to_title(meta_cat)
          ),
      color = single_part$group_color,
      family="IBMserif",
      fontface = "bold",
      size=5) +
    
    geom_text(data=single_part,  
      aes(
        x=(rect_x_max+rect_x_min)/2, 
        y=3, 
        label=paste0(inv_pct, "%")
          ),
      family="IBMserif",
      fontface="bold",
      position = position_stack(vjust=pct_place),
      size=3.5)+
    
    geom_text(
      aes(
        x = 0, 
        y = -40,
        label = "(x million USD)"
          ),
      color = single_part$group_color,
      family="IBMserif",
      fontface = "bold",
      size=2,) +
  
    geom_text(
      aes(
        x = 0, 
        y = -30,
        label = paste0(round(total_investment/1000000,0), " Million $")
          ),
      color = single_part$group_color,
      family="IBMserif",
      fontface = "bold",
      size=4) +
  
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      plot.background = element_rect(fill = "#F5F4EF", colour = "#F5F4EF"),
      panel.background = element_rect(fill = "#F5F4EF", colour = "#F5F4EF"),
      panel.grid = element_blank(),
      legend.position = "none"
         )
}
##Function[END]##
  
# Create a doughnut chart for each categories with "group_graph" function  
p1 <- group_graph("Total infrastructure",9)
p2 <- group_graph("Total basic infrastructure",7)
p3 <- group_graph("Social",9)
p4 <- group_graph("Transportation",9)
p5 <- group_graph("Highways and streets",7)
p6 <- group_graph("Power",7)
p7 <- group_graph("Education", -3)
p8 <- group_graph("Electric power",-3)
p9 <- group_graph("Air /water /other transportation")
p10 <- group_graph("Digital",-3)
  
# Define patchwork layout 3x3
# Exclude  (p9) "Air /water /other transportation"
patch <- (p1+p2+p3) / (p4+p5+p6) / (p7+p8+p10) #no "p9"

showtext_auto()

# Add title, subtitle and caption and define positions for each 
patch +
  plot_annotation(
    title = "Investment In US <span style='color:#953011;'>Top 9 Categories</span>",
    subtitle = "Between 1947-2017 (2021 - Inflation Adjusted)",
    caption =  "TidyTuesday 2021 W:33 | Source:Bureau of Economic Analysis",
    theme = theme(plot.title = element_markdown(hjust = 0.5, family = "IBMserif", size=30),
                  plot.subtitle = element_markdown(hjust = 0.5, family ="IBMserif"),
                  plot.caption = element_text(hjust = 0.5, vjust = 15, face="bold", family ="IBMserif")))

# Save output as PDF format 
ggsave("2021_TidyTuesday_W33.pdf",
       width = 8.27, height = 8.69, units="in", device = "pdf") 