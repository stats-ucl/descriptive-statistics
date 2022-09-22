# Foundation Fortnight

# Session:  Descriptive Statistics 
 
# Date:     23/09/2022

#####################################


# Required packages

#install.packages(c("tidyverse", "dslabs", "tikzDevice","cowplot","ggridges"))


library(tikzDevice) # Loads an environement with useful packages
library(tidyverse)
library(dslabs) # loads the R package containing the data
library(kableExtra)

####################### Part 1  ############################################### 


######################################

#Load the data
data(gapminder) 

#The View commmand can be used to see the row datadata
View(gapminder)

gapminder %>% as_tibble()
# as_tibble converts matrices and lists to Data Frames

# The following commands subsets the dataframe gapminder, i.e. selects
# rows using 'filter' and columns using 'select' that satisfied a set of conditions: 
# Select  columns 'country' and 'infant_mortality', and rows for which the 
# variable year takes the value 2015 and the variable
# country takes e.g. the value Sri Lanka or Turkey

gapminder %>% filter(year==2015 & country %in% c("Sri Lanka","Turkey")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Poland","South Korea")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Malaysia","Russia")) %>% select(country,infant_mortality)
gapminder %>% filter(year==2015 & country %in% c("Thailand","South Africa")) %>% select(country,infant_mortality)

# Q: Which are the countries with the lowest and highest infant mortality 
#  for 1960
# One way to do that is to use the function arrange

gapminder %>% filter(year==1960 & !is.na(infant_mortality)) %>% select(country,infant_mortality) %>% arrange(infant_mortality)

# Or we can get this directly using 

gapminder %>% 
  group_by(year) %>%
  filter(year==1960 & !is.na(infant_mortality)) %>%
  summarize(min = min(infant_mortality),
            rank=which.min(infant_mortality),
            country_min = country[rank]) %>%
  select(year, country_min, min)


# To plot Life expectancy against Fertility rate we use the graphical device
# geom_point suggests that it is a scatterplot that is to be plotted 
# the aprt in the front subsets conditions of rows of gapminder
data(gapminder)
pl=gapminder %>% filter(year==1962) %>% ggplot(aes(fertility,life_expectancy)) + geom_point() + theme_bw() +
  labs(x="Fertility rate  (average number of children per woman)",
       y="Life expectancy (in years)",
       title="Life expectancy and fertility, 1962")
pl

# NOTE: In ggplot
# The first part, gapminder %>%, denotes the data to be used
# the ggplot calls an image object
# aes (aesthetics referring to variables) are the variables we wish to visualise
# Then we add 'layers', that is we state how the variables are to be visualised
# E.g. geom_point, geom_line etc


# Then highlight the mean expectancy for:
# a) fertility rate <4.5
# b) fertility rate >4.5
# Then annotate them in the graph
# In the function 'annotate', x and y are coordinates in the graph we wish to 
# add the text

pl+geom_point(
  aes(
    x=3,
    y=gapminder %>% filter(year==1962 & fertility<=4.5) %>% summarise(mean(life_expectancy)) %>% as.numeric()
  ),col="red",size=4
) + geom_point(
  aes(
    x=7,
    y=gapminder %>% filter(year==1962 & fertility>4.5) %>% summarise(mean(life_expectancy)) %>% as.numeric()
  ),col="blue",size=4
) + 
  annotate("text",x=3,y=55,label="Mean life expectancy with < 5 children") +
  annotate("text",x=7,y=70,label="Mean life expectancy with > 4 children") + 
  annotate("segment", 
           x=3,
           xend=3, 
           y=gapminder %>% filter(year==1962 & fertility<=4.5) %>% summarise(mean(life_expectancy)) %>% as.numeric(),
           yend=56, 
           arrow=arrow(length=unit(.25,"cm"),type="closed"),color="red") +
  annotate("segment", 
           x=7,
           xend=7, 
           y=gapminder %>% filter(year==1962 & fertility>4.5) %>% summarise(mean(life_expectancy)) %>% as.numeric(),
           yend=69, 
           arrow=arrow(length=unit(.25,"cm"),type="closed"),color="blue")

# Add linear regression line through the points
pl+geom_smooth(aes(fertility,life_expectancy),alpha=.2,method="lm",formula="y~x") +
  labs(title="Life expectancy and fertility, 1962. Linear fit (95% interval) superimposed")

# Or consider a non-linear relationship...
pl+geom_smooth(aes(fertility,life_expectancy),alpha=.2,method="gam") +
  labs(title="Life expectancy and fertility, 1962. Cubic spline fit (95% interval) superimposed")


# We can colour differently certain points in the graphs
# using the color function within the ggplot function
# Here we use different colour for continent (which is a variable in our data)
gapminder %>% filter(year==1962) %>% ggplot(aes(fertility,life_expectancy,color=continent)) + geom_point() + theme_bw() +
  labs(x="Fertility rate  (average number of children per woman)",y="Life expectancy (in years)",title="Life expectancy and fertility, 1962",
       color="Continent") 

# Consider a longer time-period and present the information by year and continent in 'panels'  

gapminder %>% filter(year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_grid(continent~year) +
  labs(x="Fertility rate  (average number of children per woman)",y="Life expectancy (in years)",color="Continent")


# We now only wish to consider Asia and Europe comparisons over time
# As before we use different colour
# The command ''facet_wrap' creates different panels for each year (we have already filtered the years)
gapminder %>% filter(year %in% c(1962,1972,1982,1992,2002,2012),continent %in% c("Asia","Europe")) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x="Fertility rate  (average number of children per woman)",y="Life expectancy (in years)",color="Continent")




# We can use the same scale on the y axis for the two graphs
p1=gapminder %>% filter(year %in% c(1962,2012),continent %in% c("Asia","Europe")) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_wrap(~year) +
  labs(x="Fertility rate  (average number of children per woman)",y="Life expectancy (in years)",color="Continent",
       title="Both years on the same scale (life expectancy \nbetween 35 and 85 years)")+
  theme(legend.position = "bottom")
p1

# Or a different scale (with the option below 'scales="free" ')

p2=gapminder %>% filter(year %in% c(1962,2012),continent %in% c("Asia","Europe")) %>% 
  ggplot(aes(fertility,life_expectancy,col=continent)) + 
  geom_point() + 
  facet_wrap(~year,scales="free") +
  labs(x="Fertility rate  (average number of children per woman)",y="Life expectancy (in years)",color="Continent",
       title="Year-specific scaling (life expectancy between \n35 and 75 years in 1962 and between 65 and 85 in 2012)")+
  theme(legend.position = "bottom")
p2

# The result can be deceiving if we don't consider the same scale

library(cowplot)
# The function plot_grid  can be used to combine two graphs obtained from ggplot
# here setting the, side by side, nrow=2
plot_grid(p1,NULL,p2,nrow=1,rel_widths=c(4,.05,4))




# Draw time series, i.e consider points for life expectancy/fertility rate against time and 
# join them by a line using geom_line
#

# We can add labels to the graph and add them at the desired (x,y) coordinates
# Here we wish to add the names 'South Korea' and 'Germany' 
labels=data.frame(country=c("South Korea","Germany"),x=c(1975,1965),y=c(60,72))


p1=gapminder %>% filter(country %in% c("Germany","South Korea")) %>% 
  ggplot(aes(year,life_expectancy,col=country))+
  geom_line(size=1.1)+
  geom_text(data=labels,aes(x,y,label=country),size=5)+
  labs(x="Year",y="Life expectancy (in years)",title="Life expectancy (in year) over time for Germany and South Korea") + 
  theme_bw() +
  theme(legend.position = "none") 


p2=gapminder %>% filter(country %in% c("Germany","South Korea") & !is.na(fertility)) %>% 
  ggplot(aes(year,fertility,col=country)) + 
  geom_line(size=1.1) + 
  labs(x="Year",y="Fertility rate",title="Fertility rate over time for Germany and South Korea",color="Country") +
  theme_bw()


# Note that for the first graph we don't use auto-generated labels
# theme(legend.position = "none") 
# but instead we define them and add them manually on the graph
# using geom_text(data=labels,aes(x,y,label=country),size=5)


plot_grid(p1,NULL,p2,nrow=1,rel_widths=c(4,.05,4))

####################### Part 2  ############################################### 


# Histogram using the function 'geom_histogram'

gapminder=gapminder %>% mutate(dollars_per_day=gdp/population/365) 
# function 'mutate' adds new variables tot he data.frame


gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  theme_bw() +
  labs(x="Dollars per day",y="Absolute frequency",title="Histogram of daily income, 1970 (bins of $1)") +
  annotate("text",x=15,y=28,label="Most countries have daily average income < $10") + 
  annotate("segment", 
           x=15,
           xend=5, 
           y=27,
           yend=18, 
           arrow=arrow(length=unit(.25,"cm"),type="closed"),color="red") + 
  annotate("text",x=35,y=10,label="There is a long tail of countries with larger incomes") +
  annotate("segment", 
           x=40,
           xend=35, 
           y=9,
           yend=3, 
           arrow=arrow(length=unit(.25,"cm"),type="closed"),color="blue")


# Use wider bin width
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=5,color="black")+
  theme_bw() +
  labs(x="Dollars per day",y="Absolute frequency",title="Histogram of daily income, 1970 (bins of $5)") 

# Or transform the variable to be displayed, her using the log with base 2
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day)))+
  geom_histogram(binwidth=1,color="black")+
  theme_bw() +
  labs(x=expression(paste("Dollars per day (",log[2]," scale)")),y="Absolute frequency",title="Histogram of daily income, 1970") 


# Presenting descriptive statistics

#Here we present means a median for a subset of data

gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  summarise(mean=mean(dollars_per_day),median=median(dollars_per_day))


# Or we can produce a plot as before and display descriptives on the plot by 
# adding additional layers (geom_vline)
gapminder %>% filter(year==1970 & !is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  theme_bw() +
  geom_vline(aes(xintercept=mean(dollars_per_day),color="Mean"),linetype=2,size=1.5) + 
  geom_vline(aes(xintercept=median(dollars_per_day),color="Median"),linetype=2,size=1.5) + 
  scale_color_manual(name = "",values=c(Median="blue",Mean="red")) +
  labs(x="Dollars per day",y="Absolute frequency",title="Histogram of daily income, 1970 (bins of $1)") + 
  theme(legend.position=c(.75,.9),legend.text=element_text(size=10))


############################ Part 3 #####################################################

# Utility function to create the explanation of the boxplot
ggplot_box_legend <- function(family = "serif"){
  # Create data to use in the boxplot legend:
  set.seed(100)
  
  sample_df <- data.frame(parameter = "test",
                          values = sample(500))
  
  # Extend the top whisker a bit:
  sample_df$values[1:100] <- 701:800
  # Make sure there's only 1 lower outlier:
  sample_df$values[1] <- -350
  
  # Function to calculate important values:
  ggplot2_boxplot <- function(x){
    quartiles <- as.numeric(quantile(x,
                                     probs = c(0.25, 0.5, 0.75)))
    names(quartiles) <- c("25th percentile",
                          "50th percentile (median)",
                          "75th percentile")
    IQR <- diff(quartiles[c(1,3)])
    upper_whisker <- max(x[x < (quartiles[3] + 1.5 * IQR)])
    lower_whisker <- min(x[x > (quartiles[1] - 1.5 * IQR)])
    
    upper_dots <- x[x > (quartiles[3] + 1.5*IQR)]
    lower_dots <- x[x < (quartiles[1] - 1.5*IQR)]
    
    return(list("quartiles" = quartiles,
                "25th percentile" = as.numeric(quartiles[1]),
                "50th percentile (median)" = as.numeric(quartiles[2]),
                "75th percentile" = as.numeric(quartiles[3]),
                "IQR" = IQR,
                "upper_whisker" = upper_whisker,
                "lower_whisker" = lower_whisker,
                "upper_dots" = upper_dots,
                "lower_dots" = lower_dots))
  }
  
  # Get those values:
  ggplot_output <- ggplot2_boxplot(sample_df$values)
  
  # Lots of text in the legend, make it smaller and consistent font:
  update_geom_defaults("text",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  # Labels don't inherit text:
  update_geom_defaults("label",
                       list(size = 3,
                            hjust = 0,
                            family = family))
  
  # Create the legend:
  # The main elements of the plot (the boxplot, error bars, and count)
  # are the easy part.
  # The text describing each of those takes a lot of fiddling to
  # get the location and style just right:
  explain_plot <- ggplot() +
    stat_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 geom ='errorbar', width = 0.3) +
    geom_boxplot(data = sample_df,
                 aes(x = parameter, y=values),
                 width = 0.3, fill = "lightgrey") +
    #geom_text(aes(x = 1, y = 950, label = "500"), hjust = 0.5) +
    #geom_text(aes(x = 1.17, y = 950,
    #              label = "Number of values"),
    #          fontface = "bold", vjust = 0.4) +
    theme_minimal(base_size = 5, base_family = family) +
    geom_segment(aes(x = 2.5, xend = 2.5,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.5,
                     y = ggplot_output[["25th percentile"]],
                     yend = ggplot_output[["25th percentile"]])) +
    geom_segment(aes(x = 1.2, xend = 2.5,
                     y = ggplot_output[["75th percentile"]],
                     yend = ggplot_output[["75th percentile"]])) +
    geom_text(aes(x = 2.6, y = ggplot_output[["50th percentile (median)"]]),
              label = "Interquartile\nrange", fontface = "bold",
              vjust = 0.4) +
    geom_text(aes(x = c(1.17,1.17),
                  y = c(ggplot_output[["upper_whisker"]],
                        ggplot_output[["lower_whisker"]]),
                  label = c("Largest value within 1.5 times\ninterquartile range above\n75th percentile",
                            "Smallest value within 1.5 times\ninterquartile range below\n25th percentile")),
              fontface = "bold", vjust = 0.9) +
    geom_text(aes(x = c(1.17),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Outside value"),
              vjust = 0.5, fontface = "bold") +
    geom_text(aes(x = c(2),
                  y =  ggplot_output[["lower_dots"]],
                  label = "Value is > 1.5 times and"),
              vjust = 0.5) +
    geom_text(aes(x = 1.17,
                  y = ggplot_output[["lower_dots"]],
                  label = "< 3 times the interquartile range\nbeyond either end of the box"),
              vjust = 1.5) +
    geom_label(aes(x = 1.17, y = ggplot_output[["quartiles"]],
                   label = names(ggplot_output[["quartiles"]])),
               vjust = c(0.4,0.4,0.4),
               fill = "white", label.size = 0) +
    ylab("") + xlab("") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          aspect.ratio = 4/3,
          plot.title = element_text(hjust = 0.5, size = 13,face="bold")) +
    coord_cartesian(xlim = c(1.4,3.1), ylim = c(-600, 900)) +
    labs(title = "How does a boxplot work?") 
  
  return(explain_plot)
  
}

# Create the explainer
explainer=ggplot_box_legend("sans")
explainer

# Makes the data & creates the Boxplot
gapminder=gapminder %>% 
  mutate(group=case_when(
    region %in% c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")~"West",
    region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
    region %in% c("Caribbean","Central America","South America")~"Latin America",
    continent=="Africa" & region != "Northern Africa"~"Sub-Saharan",
    TRUE~"Others"
  ))
gapminder=gapminder %>% mutate(group=factor(group,levels=c("Others","Latin America","East Asia","Sub-Saharan","West")))
p=gapminder %>% filter(year==1970, !is.na(gdp)) %>% 
  ggplot(aes(group,dollars_per_day))+
  geom_boxplot()+
  scale_y_continuous(trans="log2") +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x="",y=expression(paste("Dollars per day (",log[2]," scale)")),title=expression(paste("Boxplot of the distribution of the median daily income(",log[2]," scale, in $) by aggregated region, 1970"))) + 
  theme_bw()

# Arranges the two plots
library(cowplot)
plot_grid(p,NULL,explainer,nrow=1,rel_widths=c(3,.1,2.2))

# We coud also show the points in the boxplots by adding a layer
p+geom_point(alpha=.5)


gapminder %>% filter(year==1970,!is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day,group))+
  scale_x_continuous(trans="log2") +
  labs(y="Aggregated region",x=expression(paste("Dollars per day (",log[2]," scale)")),title=expression(paste("Ridgeplot of the distribution of the median daily income (",log[2]," scale)"," \n by aggregated region, 1970")) ) + 
  theme_bw() +
  ggridges::geom_density_ridges(jittered_points=TRUE)


# Without using the log(2) scale and with additional annotation

gapminder %>% filter(year==1970,!is.na(dollars_per_day)) %>% 
  ggplot(aes(dollars_per_day,group))+
  scale_x_continuous() +
  labs(y="Aggregated region",x="Dollars per day",title="Ridgeplot of the distribution of the median daily income (natural scale, in $) \nby aggregated region, 1970") + 
  theme_bw() +
  ggridges::geom_density_ridges(jittered_points=TRUE) + 
  annotate("text",
           x=gapminder %>% filter(year==1970,!is.na(dollars_per_day),group=="East Asia") %>% arrange(desc(dollars_per_day)) %>% slice(1) %>% 
             pull(dollars_per_day),
           y=3,
           label=gapminder %>% filter(year==1970,!is.na(dollars_per_day),group=="East Asia") %>% arrange(desc(dollars_per_day)) %>% slice(1) %>% 
             pull(country),
           size=4,vjust=-2
  ) + 
  annotate("text",
           x=gapminder %>% filter(year==1970,!is.na(dollars_per_day),group=="West") %>% arrange((dollars_per_day)) %>% slice(1) %>% 
             pull(dollars_per_day),
           y=5,
           label=gapminder %>% filter(year==1970,!is.na(dollars_per_day),group=="West") %>% arrange((dollars_per_day)) %>% slice(1) %>% 
             pull(country),
           size=4,vjust=-2
  ) + 
  annotate("text",
           x=gapminder %>% filter(year==1970,!is.na(dollars_per_day),group=="Latin America") %>% arrange(desc(dollars_per_day)) %>% slice(1) %>% 
             pull(dollars_per_day),
           y=2,
           label=gapminder %>% filter(year==1970,!is.na(dollars_per_day),group=="Latin America") %>% arrange(desc(dollars_per_day)) %>% slice(1) %>% 
             pull(country),
           size=4,vjust=-2
  ) 



country1=gapminder %>% filter(year==1970,!is.na(dollars_per_day)) %>% pull(country)
country2=gapminder %>% filter(year==1985,!is.na(dollars_per_day)) %>% pull(country)
country3=gapminder %>% filter(year==2000,!is.na(dollars_per_day)) %>% pull(country)
country4=gapminder %>% filter(year==2010,!is.na(dollars_per_day)) %>% pull(country)
list_country=intersect(intersect(intersect(country1,country2),country3),country4)
gapminder %>% filter(year %in% c(1970,1985,2000,2010),country %in% list_country) %>% mutate(year=factor(year)) %>% 
  ggplot(aes(group,dollars_per_day,fill=year))+
  geom_boxplot()+
  scale_y_continuous(trans="log2")+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  labs(x="",y=expression(paste("Dollars per day (",log[2]," scale)")),title=expression(paste("Boxplot of the distribution of the daily income (",log[2]," (scale, in $)", "\n by aggregated region, selected years"))) + 
  theme_bw() +
  scale_fill_discrete(name = "Year")


# Make a similar plot like the one we saw in the beginning with added features!
gapminder %>% filter(year==2010) %>%  ggplot()+geom_point(aes(fertility,life_expectancy,size=dollars_per_day,color=group))+
  scale_size_continuous(breaks=c(0,2,4,8,16,32)) +
  theme_bw() +
  labs(x="Fertility rate (average number of children per woman",y="Life expectancy (in years)",title="Life expectancy and fertility by daily income (in \\$) and geographical area, 2010",color="Geographical group",size="Daily income (\\$)")




# Confounding

defaultW <- getOption("warn") 
options(warn = -1) 

options(warn = defaultW)

library(kableExtra,quietly = TRUE)


tab=tibble(Exposure=c("Exposed","Not exposed"),
           Disease=c(81,28),
           `No disease`=c(29,182),
           Risk=c("=81/(81+29)=0.7364","=28/(28+182)=0.1333"))

tab %>% kable(align=c("l","r","r","c")) %>% kable_classic() %>% kable_styling(full_width = F)



options(knitr.kable.NA = '')
tab=tibble(
  Disease=c(1,20,80,8),
  `No disease`=c(9,180,20,2),
  Sex=c(rep("M",2),rep("F",2)),
  Exposure=rep(c("Exposed","Non exposed"),2)
)

tab %>% filter(Sex=="M") %>% select(ExposureM=Exposure,DiseaseM=Disease,NodiseaseM=`No disease`) %>% 
  mutate(RiskM=DiseaseM/(DiseaseM+NodiseaseM)) %>% 
  bind_cols(
    tab %>% filter(Sex=="F") %>% select(Exposure,Disease,`No disease`) %>% mutate(RiskF=Disease/(Disease+`No disease`))
  ) %>% select(c(1,2,3,4,6,7,8)) %>% add_row(ExposureM="Relative risk",RiskM=1,RiskF=1) %>% 
  kable(col.names=c("Exposure","Disease","No disease","Risk","Disease","No disease","Risk")) %>% 
  kable_classic() %>% kable_styling(full_width = F) %>% 
  add_header_above(c("","Sex=Male"=3,"Sex=Female"=3)) %>% row_spec(2, extra_css = "border-bottom: 2px solid")

           
tab %>% filter(Sex=="M") %>% select(ExposureM=Exposure,DiseaseM=Disease,NodiseaseM=`No disease`) %>% 
  mutate(RiskM=DiseaseM/(DiseaseM+NodiseaseM)) %>% 
  bind_cols(
    tab %>% filter(Sex=="F") %>% select(Exposure,Disease,`No disease`) %>% mutate(RiskF=Disease/(Disease+`No disease`))
  ) %>% select(c(1,2,3,4,6,7,8)) %>% add_row(ExposureM="Relative risk",RiskM=1,RiskF=1) %>% 
  kable(col.names=c("Exposure","Disease","No disease","Risk","Disease","No disease","Risk")) %>% 
  kable_classic() %>% kable_styling(full_width = F) %>% 
  add_header_above(c("","Sex=Male"=3,"Sex=Female"=3)) %>% row_spec(2, extra_css = "border-bottom: 2px solid")

###############################

#Another example on Confounding


# We need to be careful to take summary/descriptive statistics or visualisation at face value!    

#There are many classical examples of "confounding" in many applications    

# "Effects" can be masked/modified by variables that we do not observe    

#  Phenomena such as **"Simpson's Paradox"**: exposure to a health condition is not 
#  bad for men and is not bed for women. But it's bad when we observe the whole population


# Consider the Berkeley admission data example

# 1973 Applications and admissions to 6 different programmes   

## Berkeley example


library(kableExtra,quietly = TRUE)

data(admissions)


# You can se the data using View function
# For example there were 	825 male applicants for program A	and 62% were admited
View(admissions)

# The aim of this analysis is to explore whether admission 
# is associated to gender and whether program of admission is a confounder in 
# the of the relationship between gender and  admission outcome


#Some data-manipulations and summaries

tab= admissions %>% filter(gender=="men") %>% 
  select(major=major,admittedM=admitted,applicantM=applicants) %>% 
  bind_cols(admissions %>% filter(gender=="women") %>% select(-c(major,gender))) 

tab=tab %>% add_row(major="Total",
                    admittedM=admissions %>% group_by(gender) %>% summarise(sum(admitted*(applicants)/sum(applicants))) %>% 
                      filter(gender=="men") %>% pull(2) %>% round(0),
                    applicantM=admissions %>% group_by(gender) %>% summarise(sum(applicants)) %>% filter(gender=="men") %>% pull(2),
                    admitted=admissions %>% group_by(gender) %>% summarise(sum(admitted*(applicants)/sum(applicants))) %>% 
                      filter(gender=="women") %>% pull(2) %>% round(0),
                    applicants=admissions %>% group_by(gender) %>% summarise(sum(applicants)) %>% filter(gender=="women") %>% pull(2)
)


tab %>% 
  kable(col.names=c("Programme","% Admitted","Applicants","% Admitted","Applicants")) %>% 
  kable_classic() %>% kable_styling(full_width = F) %>% 
  add_header_above(c("","Sex=Male"=2,"Sex=Female"=2)) %>% row_spec(6, extra_css = "border-bottom: 2px solid")


# At a glance the table above shows that the overall admission rate is much higher
# for males than females (45% vs 30%)

# Is there evidence for gender discrimination here?

# Having a closer look at the table above we can see that:

#  the percentage of admitted applicants is higher for females than males
#  in 4 out of 6 programs (A, B, D and F)!!! 


admissions=admissions %>% as_tibble() %>% mutate(sex=case_when(gender=="men"~"M",TRUE~"F")) 

admissions %>% ggplot(aes(major,admitted,col=sex,size=applicants))+geom_point() + 
  scale_size_continuous(name="Applicants") +
  scale_color_discrete(name = "Gender", labels = c("Women","Men")) +
  theme_bw() + labs(x="Programme",y="Percentage admission")


#######

# We then check the proportions of applicants that apply to each program separately  
# for men and women 

tab=admissions %>% filter(gender=="men") %>%  mutate(percent=round(applicants/sum(applicants),2)*100)  %>%  
  select(major=major,applicantM=applicants,percentM=percent) %>% 
  bind_cols(admissions %>% filter(gender=="women")  %>% mutate(percent=round(applicants/sum(applicants),2)*100)  %>%  
              select(applicantF=applicants,percentF=percent))  

tab %>% 
  kable(col.names=c("Programme","Applicants", "%","Applicants", "%")) %>% 
  kable_classic() %>% kable_styling(full_width = F) %>% 
  add_header_above(c("","Sex=Male"=2,"Sex=Female"=2)) %>% row_spec(6, extra_css = "border-bottom: 2px solid")


# The proportions differ substantially. For example in program A 31% of the male 
# applicants applied, but only 6% of the female applicants. 
# Conversely, in program E 7% of the male applicants applied but 21% of the females


# Consequently gender seems to be associated to the program of admission.  

###########

# We then check the percentage of admitted applicants regardless of gender  

tab=admissions %>% filter(gender=="men") %>%  mutate(percent=applicants/sum(applicants))  %>%  
  select(major=major,admittedM=admitted, applicantM=applicants,percentM=percent) %>% 
  bind_cols(admissions %>% filter(gender=="women")  %>% mutate(percent=applicants/sum(applicants))  %>%  
              select(admittedF=admitted,applicantF=applicants,percentF=percent)) %>%
  mutate(total_admission=applicantM+applicantF, total_percent=round((admittedM*applicantM+admittedF*applicantF)/total_admission),2)%>% 
  select(major, applicants=total_admission, admitted=total_percent)


tab

tab %>% 
  kable(col.names=c("Programme","Applicants", "Admitted %")) %>% 
  kable_classic() %>% kable_styling(full_width = F) %>%
  row_spec(6, extra_css = "border-bottom: 2px solid")

# The  admission rate seem to be associated is associated to 'Programme'


# In epidemiological terms, 'Program of Study' satisfies two key conditions to 
# be a confounder in the relationship between gender (exposure) and admission (outcome)
# because:

# 1) It associates to outcome (probability of admission)
# 2) Associates to the exposure (gender)


# In summary,   

# For most programmes, the admission rates are very similar for men and women    
# In the two least selective programmes (A and B), women tend to get higher admission rates    
# **BUT**: they also tend to apply much less!
# This drives the overall averages up for men 
  

###########
