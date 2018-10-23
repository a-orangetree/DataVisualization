library(tidyverse)
library(ggthemes)
library(gridExtra)

theme_set(theme_minimal())

# https://www.kaggle.com/abcsds/pokemon/home

#```
# `#`: ID for each pokemon
# `Name`: Name of each pokemon
# `Type 1`: Each pokemon has a type, this determines weakness/resistance to attacks
# `Type 2`: Some pokemon are dual type and have 2
# `Total`: sum of all stats that come after this, a general guide to how strong a pokemon is
# `HP`: hit points, or health, defines how much damage a pokemon can withstand before fainting
# `Attack`: the base modifier for normal attacks (eg. Scratch, Punch)
# `Defense`: the base damage resistance against normal attacks
# `SP Atk`: special attack, the base modifier for special attacks (e.g. fire blast, bubble beam)
# `SP Def`: the base damage resistance against special attacks
# `Speed`: determines which pokemon attacks first each round
#```

pokemon_data <- read_csv('Pokemon.csv') %>% 
  mutate(Legendary = factor(Legendary),
         Generation = factor(Generation),
         `Type 1` = factor(`Type 1`),
         `Type 2` = factor(`Type 2`),
         Total_Bin = factor(case_when(Total < 400 ~ '< 400',
                           Total >= 400 & Total < 600 ~ '400 - 600',
                           Total >= 600 & Total < 800 ~ '600 - 800')))



dim(pokemon_data)

head(pokemon_data)

############### Before using case_when in my mutate function, I used the 
############### range() function. Specifically, I used range(pokemon_data$Total).
############### What did that do? Why was that useful before my case_when statement?


######### Recreate this plot. Compare the pros and cons of plot 1, 2, 3.
######### Hue doesn't matter.

ggplot(pokemon_data, aes(Generation, fill = Generation)) +
  geom_bar() +
  facet_wrap(~ Legendary) +
  scale_fill_hue(l=40)

ggplot(pokemon_data, aes(Generation, fill = Generation)) +
  geom_bar() +
  facet_wrap(~ Legendary, scales = 'free_y') +
  scale_fill_hue(l=40)

ggplot(pokemon_data, aes(Generation, fill = Legendary)) +
  geom_bar() +
  scale_fill_hue(l=40)

########## Test to replace the above ########

ggplot(pokemon_data, aes(Generation, Speed)) +
  geom_boxplot()

ggplot(pokemon_data, aes(Generation, Speed)) +
  geom_boxplot(aes(fill = Legendary)) +
  facet_wrap(~ Legendary) + 
  scale_fill_hue(l=40)

ggplot(pokemon_data, aes(Generation, Speed)) +
  geom_boxplot(aes(fill = Legendary)) +
  scale_fill_hue(l=40)


################ What's the benefit of the second plot over the first
################ not much in this case, as most Type's follow this same pattern
ggplot(pokemon_data, aes(Attack, Defense)) +
  geom_point() +
  geom_smooth()

ggplot(pokemon_data, aes(Attack, Defense)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ `Type 1`)


################ Recreate the below visualization.
################ What's the problem with the below visualization?

ggplot(pokemon_data, aes(Attack, Defense)) +
  geom_point() +
  facet_grid(`Type 1` ~ `Type 2`)


####################################

ggplot(pokemon_data, aes(`Type 1`)) +
  geom_bar(aes(fill = Generation)) +
  facet_grid(Generation ~ Legendary, scales = 'free', space = 'free') +
  scale_fill_hue(l=40)


############ Challenge: similar but related concept: create 2 visualizations
############ (could be very simple) and use grid.arrange(), from the gridExtra packages,
############ to plot them

glimpse(pokemon_data)

# plot1 <- ggplot(pokemon_data, aes(`Type 1`, fill = Generation)) +
#   geom_bar() +
#   scale_fill_hue(l=40) +
#   theme(legend.position='bottom') +
#   guides(fill = guide_legend(nrow = 1)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))
# 
# plot2 <- ggplot(pokemon_data, aes(Speed, HP, color = Legendary)) + 
#   geom_point() +
#   geom_smooth(se = FALSE, method = 'lm', show.legend = FALSE)
#   scale_fill_hue(l=40)

(plot1 <- ggplot(pokemon_data, aes(`Type 1`)) +
  geom_bar(fill = 'black') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)))

(plot2 <- ggplot(pokemon_data, aes(Speed, HP, color = Legendary)) + 
  geom_point())

grid.arrange(plot1, plot2)
