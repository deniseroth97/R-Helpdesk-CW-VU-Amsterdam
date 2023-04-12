# Creating and Exporting Tables in APA Format (7th Edition)
R Helpdesk (Denise J. Roth) @FSW VU Amsterdam


- [Introduction](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#introduction/)
- [Set Up the R Session](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#set-up-the-r-session/)
- [Read in the dataset](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#read-in-the-dataset/)
    - [Prepare the dataset](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#prepare-the-dataset/)
- [Example: Simple Linear Regression](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#example-simple-linear-regression/)
    - [Creating a ```nice_table```](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#creating-a-nice_table/)
    - [Exporting the Table to Word](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#exporting-the-table-to-word/)
- [Integration with ```rempsyc``` functions](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#integration-with-rempsyc-functions/)
    - [Linear Regression with ```nice_lm```](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#linear-regression-with-nice_lm/)
    - [T-test with ```nice_t_test```](https://github.com/deniseroth97/R-Helpdesk-CW-VU-Amsterdam/blob/main/creating_and_exporting_tables.md#t-test-with-nice_t_test/)


# Introduction

As soon as we have cleaned our data and conducted our analyses to test our hypotheses and investigate our research questions, we want to be able to present our findings in an appealing and readable way. This can be done in various ways, but one important means of visualizing data is the use of tables.
In the social sciences, many journals make use of the formatting and citation guidelines provided by the *American Psychological Association (APA)*, whose current edition is its 7th revision. 
While there are many existing options for creating tables in APA format, several packages often only allow you to format tables using a specific type of analysis or lack the ability of easily converting them to Word. 
This is why in this tutorial, functions of the packages ```rempsyc``` are introduced as they allow for a great degree of flexibility.


# Set Up the R session

When we start working in R, we always need to setup our session. For this we need to set our working directory, in this case I am doing that for the folder that holds the downloaded dataset from [Kaggle](https://www.kaggle.com/datasets/gianinamariapetrascu/survey-on-students-perceptions-of-ai-in-education/). Kaggle is an online data scientist community that provides a wide range of different datasets that are freely available to users to practice their computational skills. In this specific example we will be using a dataset which contains the results of a survey conducted on undergraduate students enrolled in the 2nd and 3rd year of study at the Faculty of Cybernetics, Statistics and Economic Informatics at the Bucharest University of Economic Studies. 

```{r, eval=F, error=F}
setwd("YOURWORKINGDIRECTORY")
```



The next step for setting up our session will be to load the packages that we will be using. We will use ```tidyverse``` for data wrangling and then only need to load ```rempsyc``` in addition to that. As ```rempsyc``` functions as a wrapper around ```flextable```, we will not need to explicitly the latter package.  Note that you potentially need to install some of these packages, however.



```{r, eval=T, message=F}
library(tidyverse)
library(rempsyc)
```


# Read in the dataset 

Furthermore, we are reading in our data in ```.csv``` format.


```{r, eval=T}
mydata <-read_csv("/Users/deniseroth/Downloads/Survey_AI.csv")
```


## Prepare the dataset

Let us first select only the variables that we will be using for our analysis and give them a more intuitive variable name. We want to see whether there is a relationship between students’ knowledge AI and their perceptions of its usefulness in education. 


```{r, eval=T}
mydata <- mydata %>%  select(Knowledge = Q1.AI_knowledge, `AI Utility` = Q7.Utility_grade, Sex = Q12.Gender)
```


In a next step, we should create a factor variable that contains information of the respondents’ sex as a control variable and give it a meaningful scale. 


```{r, eval=T}
mydata <- mydata %>% 
  mutate(Female = as.factor(ifelse(Sex == "1", 1, 0)))
```


# Example: Simple Linear Regression

Let us run a simple linear regression model where we use *AI Knowledge* as our main independent variable and *AI Utility* as our dependent variable while controlling for sex. 

```{r, eval=T}
model <- lm(`AI Utility` ~ Knowledge + Female, mydata)
```


Now we gather the summary statistics that we would like to include in our table. 

```{r, eval=T}
# Gather summary statistics
statistics.table <- as.data.frame(summary(model)$coefficients)

# Get the confidence interval (CI) of the regression coefficient
CI <- confint(model)

# Add a row to join the variables names and CI to the stats
statistics.table <- cbind(row.names(statistics.table), statistics.table, CI)
# Rename the columns appropriately
names(statistics.table) <- c("Term", "B", "SE", "t", "p", "CI_lower", "CI_upper")
```


## Creating a ```nice_table```

With the summary statistics gathered, we can now easily create an attractive table using ```nice_table```.

```{r, eval=T}
nice_table(statistics.table)
```


## Exporting the Table to Word

In this next step, we can easily export the table we just created to ```.docx```. This is especially handy considering that formats such as ```.jpeg``` or ```.png``` would not appear in the often strict word counts that we work with. 


```{r, eval=T}
my_table <- nice_table(statistics.table)
flextable::save_as_docx(my_table, path = "nice_table_example.docx")
```


# Integration with ```rempsyc``` functions
The ```nice_table``` functions also integrates smoothly with other statistical functions from the ```rempsyc``` package such as ```nice_lm``` or ```nice_t_test```, among others. These functions already provide useful default effect sizes and thus make for an attractive summary table. 

## Linear Regression with ```nice_lm```

```{r, eval=T}
nice_lm(model) %>% nice_table()
```


## T-test with ```nice_t_test```

```{r, eval=T}
nice_t_test(
  data = mydata,
  response = "`AI Utility`",
  group = "Sex",
  warning = FALSE) %>% nice_table()
```


The ```nice_t_test``` functions additionally offers making several t-tests at once by specifying the desired dependent variables.


```{r, eval=F, message=F, error=F}
nice_t_test(
  data = yourdata,
  response = c("variable1", "variable2", "variable3"),
  group = "groupvariable",
  warning = FALSE) %>% nice_table()
```


These are only some of the options offered with ```nice_table``` and ```rempsyc```. For further information, check out [Rémi Thériault's vignette](https://rempsyc.remi-theriault.com/articles/table/).