library(tidyverse)
library(ggalt)
library(showtext)
library(ggridges)
library(paletteer) 
library(PNWColors)
library(ggtext)

showtext_auto()

font_add_google('PT Sans', 'ptsans')



#Scottish Household Survey - sport quintile breakdown
T8Q18 <- read_csv("SHS data/data/topic8question18.csv")
#changing * and - to na
T8Q18 <- na_if(T8Q18, '*')
T8Q18 <- na_if(T8Q18, "-")
#renaming unnamed columns
T8Q18 <- T8Q18 %>% select(-X1) %>% rename("Activity" = X4)


#Scottish Household Survey - neighbourhood rating
T4Q3 <- read_csv("SHS data/data/topic4question3.csv")
#changing * and - to na
T4Q3 <- na_if(T4Q3, '*')
T4Q3 <- na_if(T4Q3, "-")
#removing excess columns
T4Q3 <- T4Q3 %>% select(-X1)


#Scottish Household Survey - neighbourhood problems
T4Q20 <- read_csv("SHS data/data/topic4question20.csv")
#changing * and - to na
T4Q20 <- na_if(T4Q20, '*')
T4Q20 <- na_if(T4Q20, '-')
#renaming unnamed columns
T4Q20 <- T4Q20 %>% select(-X1) %>% rename("Problem" = X4)

#Scottish Household Survey =- access to green space
T10Q22 <- read_csv("SHS data/data/topic10question22.csv")
#changing * and - to na
T10Q22 <- na_if(T10Q22, '*')
T10Q22 <- na_if(T10Q22, '-')
#renaming unnamed columns
T10Q22 <- T10Q22 %>% select(-X1) 




#wrangling data 
physical_activity_df <- T8Q18 %>%
  filter(Year == '2019',
         Activity == 'Any sport incl. walking',
         Council != 'Scotland') %>%
  mutate(
    `SIMD quintile 1` = as.numeric(`1 - 20% most deprived`),
    `SIMD quintile 2` = as.numeric(`2`),
    `SIMD quintile 3` = as.numeric(`3`),
    `SIMD quintile 4` = as.numeric(`4`),
    `SIMD quintile 5` = as.numeric(`5 - 20% least deprived`)
  ) %>%
  gather(
    `SIMD quintile 1`,
    `SIMD quintile 2`,
    `SIMD quintile 3`,
    `SIMD quintile 4`,
    `SIMD quintile 5`,
    key = 'quintile',
    value = 'percent'
  )


#palette for plots
pal <- pnw_palette(name="Bay",n=5,type="discrete")

#plot theme
plot_theme <- function() {
  theme_classic() +
    
    theme(
      text = element_text(color = 'black', family = 'ptsans'),
      plot.title = element_text(hjust = 1, size = 16),
      plot.caption = element_text(size = 10),
      axis.title = element_text(size = 14, hjust = 1),
      axis.line = element_line(color = 'black', size = 0.8),
      axis.text = element_text(
        size = 12,
        color = 'black',
        family = 'ptsans'
      ),
      axis.ticks = element_line(color = 'black'),
      legend.position = 'none'
    )
}



#boxplot for quintile breakdown for activity including walk
ggplot(physical_activity_df,
       aes(x = quintile, y = percent, fill = quintile)) +
  geom_boxplot(lwd = 0.8) +
  scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "participation",
    color = NULL,
    title = "Scottish local authority participation in sport \n(any activity including walking -2019)",
    caption = "Source: Scottish Household Survey | @nicci_potts"
  ) +
  scale_y_continuous(
    limits = c(50, 100),
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_x_discrete(
    labels = function(quintile)
      str_wrap(quintile, width = 10)
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 50,
    label = "most deprived",
    color = "#838383",
    family = 'ptsans'
  ) +
  annotate(
    geom = "text",
    x = 5,
    y = 50,
    label = "least deprived",
    color = "#838383",
    family = 'ptsans'
  ) +
  plot_theme()



#Actiivity breakdown by quintile
DF2b <- T8Q18 %>%
  group_by(Council) %>%
  filter(
    !Activity %in% c(
      'Base',
      'Any sport incl. walking',
      'Any sport excl. walking',
      'None',
      'Other'
    ),
    Council != 'Scotland',
    Year == '2019'
  ) %>%
  mutate(Q1 = as.numeric(`1 - 20% most deprived`),
         Q5 = as.numeric(`5 - 20% least deprived`))



#Quintile activity plot
ggplot(DF2b, aes(y = Activity)) +
  geom_boxplot(aes(x = Q1), fill = '#00496f') +
  geom_boxplot(aes(x = Q5), fill = '#dd4124') +
  labs(
    x = 'participation',
    y = NULL,
    title = "Scottish local authorities 2019 sport participation <br>  for 
    <span style='color:#00496f;'><b>SIMD quintile 1<b></span>
         and <span style='color:#dd4124;'><b>SIMD quintile 5 <b></span>.
         </span>",
    caption = "Source: Scottish Household Survey | @nicci_potts"
  ) +
  scale_x_continuous(
    expand = c(0, 0),
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_y_discrete(
    labels = function(Activity)
      str_wrap(Activity, width = 10)
  ) +
  plot_theme() +
  theme(plot.title = element_markdown(hjust = 1, size = 16))



#High neighbourhood rating
T4_LA <- T4Q3 %>% group_by(Council) %>%
  filter(Council != 'Scotland',
         Rating == 'Very/fairly good',
         Year == c('2017', '2018', '2019')) %>%
  mutate(`20% most deprived` = as.numeric(`20% most deprived`),
         `2` = as.numeric(`2`),
         `3` = as.numeric(`3`),
         `4` = as.numeric(`4`),
         `20% least deprived` = as.numeric(`20% least deprived`)) %>%
  summarise('SIMD quintile 1' = mean(`20% most deprived`, na.rm = TRUE),
            'SIMD quintile 2' = mean(`2`, na.rm = TRUE),
            'SIMD quintile 3' = mean(`3`, na.rm = TRUE),
            'SIMD quintile 4' = mean(`4`, na.rm = TRUE),
            'SIMD quintile 5' = mean(`20% least deprived`, na.rm = TRUE)) %>%
  gather('SIMD quintile 1', 'SIMD quintile 2', 'SIMD quintile 3', 'SIMD quintile 4', 'SIMD quintile 5', 
         key= 'quintile', value= 'neighbour')

#walking participation
LA_QSIMD_Walking <- 
  df_T8Q18 %>%
  group_by(Council) %>%
  filter(Council != 'Scotland',
         Activity == 'Walking (at least 30 min)',
         Year == c('2017', '2018', '2019')) %>%
  mutate(`20% most deprived` = as.numeric(`1 - 20% most deprived`),
         `2` = as.numeric(`2`),
         `3` = as.numeric(`3`),
         `4` = as.numeric(`4`),
         `20% least deprived` = as.numeric(`5 - 20% least deprived`)) %>%
  summarise('SIMD quintile 1' = mean(`20% most deprived`, na.rm = TRUE),
            'SIMD quintile 2' = mean(`2`, na.rm = TRUE),
            'SIMD quintile 3' = mean(`3`, na.rm = TRUE),
            'SIMD quintile 4' = mean(`4`, na.rm = TRUE),
            'SIMD quintile 5' = mean(`20% least deprived`, na.rm = TRUE)) %>%
  gather('SIMD quintile 1', 'SIMD quintile 2', 'SIMD quintile 3', 'SIMD quintile 4', 'SIMD quintile 5', 
         key= 'quintile', value= 'walk')



#Joining two data sets
Neighbour_walk <- T4_LA %>%
  left_join(LA_QSIMD_Walking, by= c('Council', 'quintile'))


#neighbour rating and walking plot
ggplot(Neighbour_walk, aes(y = neighbour, x = walk, color = quintile)) +
  geom_smooth(method = 'lm', aes(fill = quintile), alpha = 0.9) +
  scale_fill_manual(values = pal) +
  
  scale_x_continuous(
    labels = function(x) paste0(x, "%")) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_manual(values = pal, guide = FALSE) +
labs(
  y = "High rating of neighborhood",
  x = 'Walking participation',
  fill = NULL,
  title = "Do people who really like their \n neighborhoods take more walks?",
  caption = "Source: Scottish Household Survey | @nicci_potts"
) +
  plot_theme() +
  theme(legend.position = 'bottom')




#Neighbourhood problems
Problem_DF2 <- T4Q20 %>%
  group_by(Problem) %>%
  filter(Year == '2019',
         Council == 'Scotland',
         Problem != 'Base') %>%
  mutate(Q1 = as.numeric(`20% most deprived`),
         Q2 = as.numeric(`2`),
         Q3 = as.numeric(`3`),
         Q4 = as.numeric(`4`),
         Q5 = as.numeric(`20% least deprived`))



#neighborhood problems by quintile
ggplot(Problem_DF2, aes(y= Problem)) +
  geom_bar(aes(x= Q1), stat= 'identity', width = 0.55, fill= '#00496f') +
  geom_bar(aes(x= Q5), stat= 'identity', fill= '#dd4124', width=0.35) +
  labs(
    y = NULL,
    x = "population",
    fill = NULL,
    title = "Comparison of neighborhood problems 
    <br> between <span style='color:#dd4124;'><b> SIMD quintile 5<b></span> 
    and <span style='color:#00496f;'><b>SIMD quintile 1<b></span>.
    </span>",
    # title = "Are there fewer problems in quintile 1 compared to quintile 5?",
    # subtitle = "<span style = ';color:#dd4124;'><quintile1></span> 
    # and <span style = ';color#00496f;'>b<quintile 5<b></span>",
    caption = "Source: Scottish Household Survey | @nicci_potts"
  ) +
  scale_y_discrete(labels = function(Problem) str_wrap(Problem, width = 10)) +
  scale_x_continuous(expand= c(0,0),
                     labels = function(x) paste0(x, "%")) +
  plot_theme() +
  theme(plot.title = element_markdown(hjust = 1, size = 16),
        axis.text = element_text(size =10))



Green_DF1 <- T10Q22 %>%
  group_by(Council) %>%
 filter(Council != 'Scotland',
   `Walking distance` == 
          c('In 5 minutes', '6-10 minutes', 
            '11 minute walk or more')) %>%
  mutate('SIMD quintile 1' = as.numeric(`1 - 20% most deprived`),
         'SIMD quintile 2' = as.numeric(`2`),
         'SIMD quintile 3' = as.numeric(`3`),
         'SIMD quintile 4' = as.numeric(`4`),
         'SIMD quintile 5' = as.numeric(`5 - 20% least deprived`)) %>%
  gather('SIMD quintile 1', 'SIMD quintile 2', 'SIMD quintile 3', 'SIMD quintile 4', 'SIMD quintile 5', 
         key= 'quintile', value= 'green') %>%
  select(`Walking distance`, quintile, green) %>%
  spread(`Walking distance`, green) %>%
  mutate(less10 = (`In 5 minutes` + `6-10 minutes`),
         more10 = `11 minute walk or more`)



p <- ggplot(Green_DF1, aes(x= quintile)) +
  geom_boxplot(aes(y= less10, fill = quintile)) +
scale_fill_manual(values = pal) +
  labs(
    x = NULL,
    y = "population",
    color = NULL,
    title = "Scottish local authority access to green space
    (less than 10 minute walk)",
    caption = "Source: Scottish Household Survey | @nicci_potts"
  ) +
  scale_y_continuous(
    
    labels = function(x)
      paste0(x, "%")
  ) +
  scale_x_discrete(
    labels = function(quintile)
      str_wrap(quintile, width = 10)
  ) 


p + annotate(
  geom = "text",
  x = 1,
  y = 60,
  label = "most deprived",
  color = "#838383",
  family = 'ptsans'
) +
  annotate(
    geom = "text",
    x = 5,
    y = 60,
    label = "least deprived",
    color = "#838383",
    family = 'ptsans'
  ) +

plot_theme()
