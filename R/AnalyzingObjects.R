load(file = 'data/myobjects.RDat')
meanAge <- mean(a$age)



data <- caffeine %>%
  group_by(drink) %>%
  summarise(
    mean = mean(score))

data <- caffeine %>%
  filter(gender == "female" & age > 20) %>%
  group_by(drink, cups) %>%
  summarise(
    mean = mean(score),
    med = median(score),
    max = max(score),
    sd = sd(score),
    n = n()
  )

  