library(ggpubr)
library(ggplot2)
library(gplots)

########## Exploratory Data Analysis ########## 

summary(covid_trim)

##histogram for each numeric variables
par(mfrow = c(2,3))
p1 <- ggplot(covid_trim, aes(x=total_new_cases_per_million)) + geom_histogram(color="black", fill="lightblue") + theme(axis.title.x = element_text(size =8)) + theme(axis.text.x = element_text(angle = 45)) + xlab("Total new cases (/M)") + labs(tag = "A")

p2 <- ggplot(covid_trim, aes(x=PERSONS_VACCINATED_1PLUS_DOSE_PER100)) + geom_histogram(color="black", fill="orchid4") + xlab('#People vaccinated 1+ dose (/100)') + theme(axis.title.x = element_text(size = 8))+ labs(tag = "B")
p3 <- ggplot(covid_trim, aes(x=PERSONS_FULLY_VACCINATED_PER100)) + geom_histogram(color="black", fill="orchid4") + theme(axis.title.x = element_text(size = 8)) + xlab('#People fully vaccinated (/100)')+ labs(tag = "C")
p4 <- ggplot(covid_trim, aes(x=FIRST_VACCINE_DATE)) + geom_histogram(color="black", fill="white") + theme(axis.title.x = element_text(size = 8)) + xlab('#Days first vaccination date from now')+ labs(tag = "D")
p5 <- ggplot(covid_trim, aes(x=stringency_index)) + geom_histogram(color="black", fill="white") + theme(axis.title.x = element_text(size = 8)) + xlab('Stringency index')+ labs(tag = "E")
library(ggpubr)
p <- ggarrange(p1,p2,p3,p4,p5,
               ncol = 3, nrow = 2)
annotate_figure(p, top = text_grob("Histogram for Each Quantitative Variables", 
                                   color = "Black", size = 14))

##histogram and bar plot for only dependant variable
plt1 <- ggplot(covid_trim, aes(x=total_new_cases_per_million)) + geom_histogram(color="black", fill="lightblue") + theme(axis.title.x = element_text(size =8)) + xlab('') + labs(title = "A", subtitle = "Total new cases (/M)") 

plt2 <- covid_trim %>% select(total_new_cases_per_million) %>%
  ggplot(aes(x="", y = total_new_cases_per_million)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  coord_flip() +
  theme_classic() +
  ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
egg::ggarrange(plt1, plt2, heights = 2:1)

##histogram for `PERSONS_VACCINATED_1PLUS_DOSE_PER100` &`PERSONS_VACCINATED_1PLUS_DOSE_PER100`
ggplot(covid_trim) +
  geom_histogram(aes(x=PERSONS_FULLY_VACCINATED_PER100), 
                 color="black", fill="grey") + 
  geom_histogram(aes(x=PERSONS_VACCINATED_1PLUS_DOSE_PER100), 
                 color="black", fill="orchid4", alpha = 0.5) + 
  theme(axis.title.x = element_text(size = 14), 
        axis.title.y = element_text(size = 14)) + 
  xlab('#People vaccinated (/100)') +
  labs(title = "B & C", subtitle = "Histogram for #people vaccinated 1+ dose (purple) & 2 doses (grey)") 

##boxplot for each numeric variables
par(mfrow = c(2,3))
boxplot(covid_trim$total_new_cases_per_million, main="total_new_cases_per_million", type="l")
boxplot(covid_trim$PERSONS_VACCINATED_1PLUS_DOSE_PER100, main="PERSONS_VACCINATED_1PLUS_DOSE_PER100", type="l")
boxplot(covid_trim$PERSONS_FULLY_VACCINATED_PER100, main="PERSONS_FULLY_VACCINATED_PER100", type="l")
boxplot(covid_trim$FIRST_VACCINE_DATE, main="FIRST_VACCINE_DATE", type="l")
boxplot(covid_trim$stringency_index, main="stringency_index", type="l")

##correlation matrix plot
covid_trim_numeric <- covid_trim %>% 
  select("PERSONS_VACCINATED_1PLUS_DOSE_PER100",
         "PERSONS_FULLY_VACCINATED_PER100",
         "FIRST_VACCINE_DATE",
         "total_new_cases_per_million",
         "total_new_deaths_per_million",
         "stringency_index") %>%
  rename('stringency' = stringency_index, 
         '1+ dose' = PERSONS_VACCINATED_1PLUS_DOSE_PER100, 
         '2 doses' = PERSONS_FULLY_VACCINATED_PER100,
         'date difference' = FIRST_VACCINE_DATE,
         'new cases' = total_new_cases_per_million,
         'new deaths' = total_new_deaths_per_million)

corrplot.mixed(cor(covid_trim_numeric), 
               order = 'AOE',
               tl.cex = 0.65,
               addCoef.col = 'black',
               title = 'Correlation Matrix Plot',
               mar=c(0,0,1,0) #http://stackoverflow.com/a/14754408/54964
)

##display the summary statistics by vaccine_type_used
with(covid_trim, tapply(total_new_cases_per_million, vaccine_type_used, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

##pairwise bivariate displays for dependent variables against each of the regressor
formula <- total_new_cases_per_million ~ FIRST_VACCINE_DATE + vaccine_type_used + PERSONS_FULLY_VACCINATED_PER100 + stringency_index
par(mfrow = c(2,2))
plot(formula, data = covid_trim)

##mean plot: show comparison with average wage by occupations
plotmeans(total_new_cases_per_million~vaccine_type_used,data=covid_trim, ylab = 'average total new cases', main = 'Main Effect Plot')

##histogram for vaccine_type_used
ggplot(covid_trim, aes(total_new_cases_per_million, fill = vaccine_type_used)) + 
  geom_histogram() + 
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442","#CC79A7")) +
  facet_grid(vaccine_type_used ~ ., margins = TRUE, scales = "free") +
  ggtitle ("Histogram of Vaccine Types Used")


