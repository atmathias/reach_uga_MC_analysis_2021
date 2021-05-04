
library(tidyverse)

# farmers utilizing tillage services by phases of intervention
# creating a data frame
df_farmers <- tibble(response= c("Yes", "No", "Yes", "No"), 
                     number_farmers  = c(27, 293, 461, 56), 
                     phases = c("Baseline", "Baseline","Endline", "Endline")
                     
)
# creating a graph
df_farmers %>% 
  ggplot(aes(fill=response, x = phases, y = number_farmers,
  )) 
# creating bars
geom_bar(position = "dodge", stat = "identity") 

# adding title
labs(title = paste("Main implementation phase"))

# adding title and subtitle
labs(title = paste("Main implementation phase", 
                   subtitle = "n baseline: 654, n endline:517", x= element_blank(), y= element_blank()))

# adding colors from palette
scale_fill_manual(values = c("#303434", "#00aeef"
))  

# adjusting the height position  the labels
geom_text(aes(label = number_farmers, vjust = 3.4))

# how to edit legend
theme(legend.title = element_blank(),
      number_farmers=position_stack(vjust=0.5))

# how to adjust variable option in x axis
theme(legend.title = element_blank(),
      number_farmers=position_stack(vjust=0.5), hjust=1)

# how to adjust variable options at an angle
axis.text.x = element_text(angle = 90)

# Limiting the height of y axis
ylim(0, 40)

# adding % sign of y axis values 
scale_y_continuous(labels = function(x) paste0(x, "%"))

# removing background texture
theme_bw() +
  
  # removing the graph from the square box
  theme_minimal() 

# adjusting data label with on graph bars
position = position_dodge2(width = .9)

# removing x axis title
axis.title.x = element_blank()

# removing axis y values
axis.text.y = element_blank()

# removing ticks from y axis
axis.ticks.y = element_blank()

# allows to set y limits even though have already applied a scale e.g 
# scale_y_continuous(labels = function(x) paste0(x, "%"))
 
coord_cartesian(ylim = c(0,25))





