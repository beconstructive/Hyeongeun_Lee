# load ggplot package
library(ggplot2)

# load our group dataset
ourdata <- read.csv(file.choose())

# change response's value to character
ourdata$EMPLOYMENT = ifelse(ourdata$EMPLOYED==1, 'EMPLOYED', 'UNEMPLOYED')

# draw a plot
qplot(GPA, AGE, color=EMPLOYMENT, data=ourdata)

