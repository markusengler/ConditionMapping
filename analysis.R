library(tidyverse)
library(broom)
library(multcomp)
import::from(multcomp, glht, mcp, contrMat)
import::from(broom, tidy)
source("util.R") 


data <- read_csv("exampledata.csv")
map_condition <- read_csv("mapcondition.csv")

# add IV columns
data[, "EO"] <- NA
data[, "TL"] <- NA
data[, "ES"] <- NA
data[, "SS"] <- NA

# map endIndex to condition
for(i in 1:nrow(data)){
  # starting index of table is one
  index_that_maps <- data$endIndex[i] + 1
  row_to_map <- map_condition[index_that_maps,]
  data[i,]$EO <- row_to_map$`Edge orientation`
  data[i,]$TL <- row_to_map$`Target location`
  data[i,]$ES <- row_to_map$`Edge section`
  data[i,]$SS <- row_to_map$`Side section`
}

data$ES <- as.factor(data$ES)
data$SS <- as.factor(data$SS)

#Graph Mean time vs IDE with grouping by Edge Orientation
data_Edge <- data %>% group_by(EO) %>% summarise(
  Mean_Time = mean(MT),
  ID = mean(ID) ,
  median = median(MT),
  sd = sd(MT),
  min = min(MT),
  max = max(MT)
)
ggplot(data_Edge, aes(x=ID, y=Mean_Time, group=EO)) +
  geom_point(aes(color=EO))

#Graph Mean time vs IDE with grouping by Target Location
data_TL <- data %>% group_by(TL) %>% summarise(
  Mean_Time = mean(MT),
  ID = mean(ID) ,
  median = median(MT),
  sd = sd(MT),
  min = min(MT),
  max = max(MT)
) 
ggplot(data_TL, aes(x=ID, y = Mean_Time, group = TL)) + 
  geom_point(aes(color =TL))

#Graph Mean time vs IDE with grouping by Edge Section
data_ES <- data %>% group_by(ES) %>% summarise(
  Mean_Time = mean(MT),
  ID = mean(ID) ,
  median = median(MT),
  sd = sd(MT),
  min = min(MT),
  max = max(MT)
) 
ggplot(data_ES, aes(x=ID, y = Mean_Time, group = ES)) + 
  geom_point(aes(color = ES))

#Graph Mean time vs IDE with grouping by Side Section
data_SS <- data %>% group_by(SS) %>% summarise(
  Mean_Time = mean(MT),
  ID = mean(ID) ,
  median = median(MT),
  sd = sd(MT),
  min = min(MT),
  max = max(MT)
) 
ggplot(data_SS, aes(x=ID, y = Mean_Time, group = SS)) + 
  geom_point(aes(color =SS))

#Graph Mean time vs IDE with grouping by ES and SS
data_ES_SS <- data %>% group_by(ES,SS) %>% summarise(
  Mean_Time = mean(MT),
  ID = mean(ID) ,
  median = median(MT),
  sd = sd(MT),
  min = min(MT),
  max = max(MT)
)

ggplot(data_ES_SS, aes(x=ID, y=Mean_Time)) +
  geom_line(aes(linetype=ES))+
  geom_point(aes(color=SS))

##### Code for Interaction effect between Edge section and Side Section
#Visualize the data for interaction between Edge section and side section
pd <- position_dodge(0.3) # ensure no overlaps
p_data <- 
  data %>% 
  ggplot(aes(x = ES, y = MT, color = SS, group = SS)) +
  stat_summary(fun.y = mean, geom = "point", shape = 18, size = 3, position = pd) + 
  stat_summary(fun.y = mean, geom = "line", position = pd) + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0, position = pd) + 
  ylab("Mean(Time) and 95% CI (from individual group)") +
  expand_limits(y = 0)
p_data

#Asses the above linear model with goodness of fit ANOVA
m_full <- lm(MT ~ SS * ES, data = data)
anova(m_full)


#If the interaction effect is not significant
m_full_main <-  update(m_full, .~. - SS:ES)  
pairwise_main <- 
  glht(m_full_main,
       linfct = mcp(
         ES = "Tukey",
         SS = "Tukey"))
p_pairwise_main <- 
  tidy(confint(pairwise_main)) %>% 
  plot_glht()
p_pairwise_main


#If the interaction effect is significant
# 1. construct the matrix for each variable
ss_mat <- contrMat(table(data$SS), "Tukey")

# 2. pad other columns with zero
touch_rows <- cbind(ss_mat,       (ss_mat * 0))
knob_rows <- cbind((ss_mat * 0),   ss_mat)

# 3. add hypothesis labels as row names
rownames(touch_rows) <- paste(levels(data$ES)[1],  rownames(touch_rows), sep =": ")
rownames(knob_rows)  <- paste(levels(data$ES)[2],  rownames(knob_rows),  sep =": ")

# 4. concatenate them together
contrast_matrix <- rbind(
  touch_rows,
  knob_rows
)
contrast_matrix

# We will also need a model *only* with the interaction term
m_interaction <- update(m_full, .~. - SS - ES -1)

# use the contrast matrix in glht() in place of "mcp()"
pairwise_interaction <- glht(m_interaction, linfct = contrast_matrix)  
p_pairwise_interaction <- 
  tidy(confint(pairwise_interaction)) %>% 
  plot_glht()
p_pairwise_interaction

################Code ends for interaction between ES and SS

