# Repeated Measures ANOVA
R Helpdesk (Denise J. Roth) @FSW VU Amsterdam

- [Introduction](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#introduction)
- [Set up the R Session](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#set-up-the-r-session)
- [Import the Dataset](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#import-the-dataset)
    - [Prepare the Dataset](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#prepare-the-dataset)
    - [Convert the Dataframe from Wide to Long Format](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#convert-the-dataframe-from-wide-to-long-format)
- [Perform Repeated Measure Analysis](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#perform-repeated-measure-analysis)
- [The Multi Level Approach](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/tutorials/repeated_measures_ANOVA.md#the-multi-level-approach)




# Introduction

As social scientists delving into the world of statistics, it's essential to grasp various statistical techniques commonly used in research. One such technique is the Repeated Measures Analysis of Variance (ANOVA). In this tutorial, we'll explore what a repeated measures ANOVA is, when to use it, and how it can be applied to analyze data in research studies. So let's dive in!

Repeated Measures ANOVA is a statistical method used to analyze data when the same subjects or participants are measured on multiple occasions or under different conditions. It allows researchers to examine whether there are significant differences across these repeated measurements or conditions while accounting for the interdependence of the data. It is particularly useful when you have data that involve multiple measurements taken from the same subjects or participants. It is commonly used in various fields such as psychology, biology, medicine, education, and social sciences. You would typically choose a repeated measures ANOVA over other analysis methods when you either have a: 

1. within-subject design: This means that each participant in your study is measured or tested under different conditions or at multiple time points. For example, measuring the performance of students before and after an intervention or assessing the effects of different treatment conditions on patients' blood pressure. Or:

2. You want to compare the means of three or more conditions: Repeated Measures ANOVA is specifically designed to analyze data with three or more levels or conditions. If you have only two conditions, a paired t-test might be more appropriate.


# Set up the R Session

When we start working in R, we always need to setup our session. For this we need to set our working directory. In this case I am doing that for the folder that holds the downloaded dataset from a pre-test which was setup to determine which of five different brands  (Adidas, Puma, Nike, H&M, Tommy Hilfiger) had the best fit with two celebrity influencer couples (Beyoncé and Jay Z and Shakira and Piqué) for the promotion of a brand on Instagram. Participants rated the congruence of five sports brands with one of the celebrity couples (randomized), measured using a 7-Point Likert scale ranging from one (strongly disagree) to seven (strongly agree). Participants were also asked their agreement level to statements such as “Adidas is a good match with the celebrities” or “Puma is a good match with the celebrities” etc.. The congruence was the within factor with five levels (Adidas, Puma, Nike, H&M, Tommy Hilfiger) and the between factor was the type of celebrity couple (Beyonce & Jay-Z versus Shakira & Piqué). In total, 32 participants filled in the survey.


```{r, eval=F, error=F}
setwd("YOURWORKINGDIRECTORY")
```


The next step for setting up our session will be to load the packages that we will be using. We will use a package that is included in the ```tidyverse``` for data wrangling (```dplyr```) as well as the ```rio``` package for importing our data. Additionally, we need the ```reshape2``` to reshape our data. Finally, the packages ```stats```, ```ez```, ```nlme``` and ```emmeans``` are used for conducting our analyses. Note that you potentially need to install some of these packages, however.


```{r, eval=T, message=F}
library(rio)
library(dplyr)
library (reshape2)
library(stats)
library(ez)
library(nlme)
library(emmeans)
```


# Import the Dataset

We are importing a ```.sav``` file from Qualtrics.

```{r, eval=T}
d <- import("/Users/deniseroth/Downloads/Native advertisement - pretest wide.sav")
```

## Prepare the Dataset

In these next steps, we first create a variable that contains a participant ID and we also change the classes for some of the variables included, as our model will ultimately need to know how to treat which variables in its calculations.


```{r, eval=T}
# Add a participant id to the dataframe
d_id <- d %>%
  mutate(id = row_number())


#Convert colums Cleb_Congruence_1 to Cleb_Congruence_5 as numeric, and Condition_Couple as #factor 
d_id <- d_id %>%
  mutate(across(c(Cleb_Congruence_1, Cleb_Congruence_2, 
                 Cleb_Congruence_3, Cleb_Congruence_4, 
                 Cleb_Congruence_5, Condition_Couple), as.numeric)) %>%
  mutate(across(c(Condition_Couple,id), as.factor))

```

## Convert the Dataframe from Wide to Long Format

In R, data frames can be organized in two different formats: wide and long. Each format has its advantages and is suitable for different types of analyses. Let's explore the differences between these formats.
In the wide format, each subject's measurements or observations are represented in a single row, and each variable or condition has its own column. This format is often used when data is initially collected or entered. Advantages of the wide format include simplicity and ease of data entry. It is also suitable for certain analyses like independent samples t-tests or ANOVA. However, for repeated measures analyses, the long format is preferred and necessary.

In the long format, each observation is represented in a separate row, and a single column is used to indicate the condition or variable. This format is particularly useful when dealing with repeated measures or within-subjects designs. Each subject's measurements are stacked on top of each other, allowing for easy identification of repeated measures and within-subjects factors. This format facilitates the analysis of dependencies and enables appropriate statistical techniques such as Repeated Measures ANOVA. Repeated Measures ANOVA requires a long format because it relies on the interdependence of measurements taken from the same subjects. By organizing the data in a long format, we explicitly represent the repeated measurements and their associated conditions. This allows the statistical analysis to properly account for the within-subjects variability.

In a long format, each row represents a unique measurement, and the subject identifier ensures that the measurements are linked to the corresponding individuals. This format provides the necessary structure for conducting analyses that involve repeated measures, as it allows for the identification of within-subjects factors, the calculation of subject-specific means, and the assessment of the differences between conditions.

By using the long format in Repeated Measures ANOVA, we can effectively examine the effects of the within-subjects factors while accounting for the dependency between measurements. The analysis can then yield valuable insights into the significance of the conditions and their impact on the measured outcome.

Let us first view what the our initially "wide" dataframe looks like, reshape it to the long format and then take a look again!


```{r, eval=T}
View(d)

d_long <- melt(d_id,id.vars=c("id", "Condition_Couple"),
measure.vars=c("Cleb_Congruence_1", "Cleb_Congruence_2","Cleb_Congruence_3", "Cleb_Congruence_4","Cleb_Congruence_5"),
          variable.name="Cleb_Congruence", 
          value.name="brand")


View(d_long)
```

# Perform Repeated Measure Analysis 

Now, it is time to actually perform a Repeated Measure ANOVA using functions from the ```ez``` package that we loaded earlier. In addition to that, we can also perform a Bonferroni post-hoc test. 


```{r}
model <-ezANOVA(data=d_long, dv=brand, wid=.(id), within=.(Cleb_Congruence),between=.(Condition_Couple),
            detailed=TRUE,type=3, return_aov = TRUE)
print(model)



pairwise.t.test (d_long$brand, d_long$Cleb_Congruence,paired=TRUE, p.adjust.method="bonferroni")
```

# The Multi Level Approach 

In a multi-level approach, it is assumed that scores come from the same entities (in this case the variable id that we created earlier), and the scores are therefore correlated.  So, one can use the the the multi-level approach using the function```lme``` for  Linear Mixed Effects Models from the ```nlme``` package.
It is assumed that there is variability in participants in brand scores for Celebrity Congruence and the model takes into account that the predictor Celebrity Congruence is made up of data from the same participants. 
For more information on this see: [Field, A., Miles, J., & Field, Z. (2012). Discovering Statistics Using R. SAGE.](https://www.discoveringstatistics.com/books/discovering-statistics-using-r/).
Finally, we can use the ```emmeans``` function for a contrast analysis.

```{r}
lme <- lme(brand ~ Cleb_Congruence + Cleb_Congruence*Condition_Couple, random = ~1|id/Cleb_Congruence, data = d_long, method = "ML")
summary(lme)

#Perform a contrast analysis
library(emmeans)
emmeans(lme, pairwise ~ Cleb_Congruence)
emmeans(lme, pairwise ~ Cleb_Congruence | Condition_Couple)
```


