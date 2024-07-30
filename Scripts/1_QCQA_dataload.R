# Herbivore plots QCQA and data load:

# Load in libraries:
library(googledrive)
library(readxl)
library(tidyverse)
library(nlme)

# Download the data:

googledrive::drive_download("https://docs.google.com/spreadsheets/d/1X4YxNTbQNiVg1-kzoqazSMM6JYgQ-q0ag682sZgh1zE/edit?gid=0#gid=0",
                            path = "Data/herbivory.xlsx", overwrite = T)

googledrive::drive_download("https://docs.google.com/spreadsheets/d/1OXkXKB0SRqanmb3wOPqOgzsjBMVvnpYFa9i1DmdYW-M/edit?gid=1795008621#gid=1795008621",
                            path = "Data/plantchar.xlsx", overwrite = T)

googledrive::drive_download("https://docs.google.com/spreadsheets/d/13BHYJIl5wmqMopoErdp4PeqoYxTmA5DE8jaPCGS5d0s/edit?gid=0#gid=0",
                            path = "Data/vegsurvey.xlsx", overwrite = T)


# Read in plantchat data:
temp1 = vector(mode = "list", length = 8)

for(i in 1:length(temp1)){
  temp1[[i]] = read_xlsx("Data/plantchar.xlsx", sheet = paste0("Block ", i), skip = 2)
  
  colnames(temp1[[i]]) = c("Date",paste0(rep(c("Exclusion", "Grasshopper", "Shade", "Control"), each = 4),"_",
                                         rep(c("H1", "H2", "H3", "Av"), times = 4)), "BlockAvg")
  
  temp1[[i]] = temp1[[i]] %>% mutate(Block = i)
  
}

temp1 = do.call("rbind", temp1)

temp1 = temp1 %>%
  select(Date, Block, contains("_H")) %>%
  pivot_longer(!Date & !Block) %>%
  separate(name, into = c("Treatment", "Replicate"), sep = "_") %>%
  rename(Height_cm = value)

temp1 %>% mutate(Block = as.factor(Block)) %>% ggplot(aes(x = Block, y = Height_cm, color = Treatment)) + geom_boxplot()

# Run a basic ANOVA for the baseline conditions:
baselineheightmodel = lm(Height_cm ~ Treatment + Block, data = temp1)
summary(baselineheightmodel)
plot(baselineheightmodel)

# Read in the herbivory data:

herb = vector(mode = "list", length = 8)

for(i in 1: length(herb)){
  herb[[i]] = read_xlsx("Data/herbivory.xlsx", sheet = paste0("Block ", i), skip = 1) %>%
    mutate(Block = i)
}

herb = do.call("rbind", herb)

herb %>%
  select(Date, `Location (T,M,B)`, `Type (E,G,S,C)`, Block, `Above ziptie (%)`, `Below ziptie (%)`) %>%
  pivot_longer(`Above ziptie (%)` | `Below ziptie (%)`) %>%
  rename(Damage = value) %>%
  mutate(Block = as.factor(Block)) %>%
  ggplot(aes(x = `Type (E,G,S,C)`,y = Damage, color = Block)) + geom_boxplot() + facet_grid(name~`Location (T,M,B)`)



herb = herb %>%
  mutate(UPlant = paste0(`Plant ID (R,Y,G)`, `Type (E,G,S,C)`, Block),
         UCage = paste0(`Type (E,G,S,C)`, Block))  %>%
  pivot_longer(`Above ziptie (%)` | `Below ziptie (%)`) %>%
  rename(Damage = value) %>%
  rename(Treatment =`Type (E,G,S,C)`) %>%
  mutate(Block = as.factor(Block))


m1 <- lme(Damage ~ Treatment, random = ~1|Block/UCage/UPlant, data = herb)
summary(m1)
plot(m1)
