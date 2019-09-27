### Identifying weak and strong Islamophobia
  # 5, Visualize results

library(ggplot2)
library(cowplot) # not essential - but it leads to nicer plotting
library(dplyr)
library(gridExtra)
options(scipen = 999)
gc()

author.theme = theme(text = element_text(size = 12, family = "Arial"),
                     title = element_text(size = 12, family = "Arial"),
                     axis.text.x = element_text(size = 10, family = "Arial"),
                     axis.text.y = element_text(size = 10, family = "Arial"))

# Load data
load("tweets.RData") # our starting dataset of tweets
load('annotation.output.RData') # the 'model.in' output from your the previous script, with the annotations from the classifier
tweets$strength = model.in$strength
tweets$created_atDate = NULL



## Calculate prevalence of each type of hate speech
df.strength = data.frame(table(tweets$strength))
df.strength$Var1 = c('None', 'Weak', 'Strong')
df.strength$Var1 = factor(df.strength$Var1,
                         levels = c('None', 'Weak', 'Strong'))
df.strength$Freq = df.strength$Freq / sum(df.strength$Freq)
plot.strength = ggplot2::ggplot(df.strength,
                aes(Var1, Freq)) +
  geom_bar(stat = 'identity', fill = c('light grey', 'dark grey', 'black'),
           color = 'black') +
  author.theme +
  scale_y_continuous(labels = scales::percent) +
  xlab('Islamophobia') +
  ylab('Percentage of tweets')
plot.strength



## Prevalence over time
tweets$strength = factor(tweets$strength)

strength.time = tweets %>%
  group_by(date) %>%
  count(strength)
strength.time$strength = gsub(x = strength.time$strength,
     pattern = '1',
     replacement = 'None')
strength.time$strength = gsub(x = strength.time$strength,
                              pattern = '2',
                              replacement = 'Weak')
strength.time$strength = gsub(x = strength.time$strength,
                              pattern = '3',
                              replacement = 'Strong')
strength.time = strength.time[1:520,] # better plotting
colnames(strength.time)[2] = 'Strength'
strength.time$Strength = factor(strength.time$Strength,
                                levels = c('None', 'Weak', 'Strong'))

plot.islamo.time = ggplot2::ggplot(strength.time,
                aes(date, n,
                    group = Strength,
                    color = Strength)) +
  geom_line() +
  scale_color_manual(values = c('light grey', 'dark grey', 'black')) +
  xlab('2017') +
  ylab('Number of tweets') +
  author.theme
plot.islamo.time                
  # Due to the low number of tweets in the sample, there are likely to be many NAs - for every day/strength missing you get an NA.
  # You won't have this problem if you apply the classifier to a larger dataset



# Save plots together
plot.islamo = gridExtra::arrangeGrob(
    plot.strength,
    plot.islamo.time,
    ncol = 2)

ggplot2::ggsave(plot.islamo,
                file = "fig1.png",
                units = 'cm',
                width = 20, height = 10,
                dpi = 450)          
                
                
                
                