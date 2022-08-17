---
title: "Voting in Canada: 2014-2019"
author: Dave Armstrong and Laura Stephenson
---
<style> 
#refs {
  border-collapse: collapse;
  display: inline-block;
}
#refs td, #refs th {
  border: 1px solid #ddd;
  padding: 8px;
}
#refs tr:nth-child(even){background-color: #f2f2f2;}
#refs th {
  padding-top: 12px;
  padding-bottom: 12px;
  text-align: left;
  background-color: #4B51B680;
  color: white;
}
</style>
This app is designed as a tool for instructors to use in classes focusing on canadian politics and political behaviour.  We designed it in such a way that it implements the current best-practices in statistical modeling for these types of data.  Before describing the methodology, though, we discuss the variables and sources of the data.  The data come from six different, independent studies - the Canadian Election Studies from 2004, 2006, 2008, 2011, 2015 and 2019.  We chose a set of variables that are common to all of the studies.  The variables in the dataset are: 

### Variables
  - ***Vote***: This variable is used to make all of the "Vote for ..." variables.  These are actual self-reported votes from the post-election study, not campaign-period vote intention.  We coded those who indicated did not vote, none, don't know and refused as missing. 
  - ***Turnout***: The vote turnout question asks whether or not people turned out.  Here, we coded those who indicated they did not vote and those who indicated that they did not know as not having voted.  We drop from our sample those who are coded 0 on the turnout question. 
  - ***Gender***: in the CES is a binary construct 
  - ***Age Group***: Age is calculated by subtracting the year of birth from the survey year.  Then observations are put into age-groups (18-34, 35-54, 55+)
  - ***Religion***: Respondents are coded into four groups - no religious affiliation/Agnostic, Catholic, Non-Catholic Christians (incl. Anglican, Baptist, Eastern Orthodox, Johova's Witness, Lutheran, Pentecostal, Presbyterian, Protestant, United Church of Canada, Christian, Salvatian Army, Mennonite) and Other (incl. Buddhist, Hindu, Hewish, Muslim, Sikh).  We also include an indicator variable for Catholic vs non-Catholic. 
  - ***Schooling***: Education was coded into three categories HS or Less (incl. No schooling, some elementary, completed elementary, some secondary, completed secondary), Some Post-secondary (incl. some techical/community college, completed technical/community college, some university) and Univ Grad (incl. bachelor's degree, master's degree, professional degree)
  - ***Union Household***: Does the respondent or a family member belong to a union.  In 2019, this question only codes whether the respondent belongs to a union.  There is no question in 2019 about other family members. 
  - ***Year***: Year of the survey 
  - ***Region***: Provinces are coded into four regions: Atlantic (Newfoundland and Labrador, PEI, Nova Scotia, New Brunswick), Quebec, Ontario and the West (Manitoba, Saskatchewan, Alberta and British Columbia).  
  - ***Continentalism***: 
  - ***Market Liberalism***: 
  - ***Moral Tradition***: 
  - ***Political Cynicism***: 
  - ***Party ID***: Party with which respondent identifies.  These are coded into Liberal, Conservative, NDP, Green, Bloc Quebecois and Other.  We also include indicators where the Green party is move to the other category, the BQ is moved to the other category and both BQ and Green are moved to the other category. 
  - ***Personal Retrospective Economy***: Whether respondent thinks his or her personal economic situation has gotten better, stayed the same or gotten worse in the past year. 
  - ***National Retrospective Economy***: Whether respondent thinks Canada's economic situation has gotten better, stayed the same or gotten worse in the past year. 
  - ***Defense Spending***: Respondent's opinion of how much defence spending should change in three categories - Less (much less, less), Stay the same, More (more or much more). 
  - ***Environmental Spending***: Respondent's opinion of how much spending on the environment should change in three categories - Less (much less, less), Stay the same, More (more or much more). 
  - ***More Immigration***: Respondent's opinion about how immigration levels should change - Increase, Stay the same/Don't Know, Decrease
  - ***Ties to USA***: Respondent's opinion about how ties between Canada and the US should change - Much more distant, Somewhat more distant, Stay the Same/Don't Know, Somewhat closer, Much closer. 
  - ***Private Sector Create Jobs***: Level of agreement with the following statement - The government should leave it ENTIRELY to the private sector to create jobs: Strongly disagree, Disagree, Don't know, Agree, Strongly agree. 
  - ***Blame Self Failure***: Level of agreement with the following statement - People who don't get ahead should blame themselves, not the system: Strongly disagree, Disagree, Don't know, Agree, Strongly agree. 
  - ***Reduce Income Inequality***: How much should be done to reduce the gap between rich and poor in Canada - Much less, Somewhat less, About the same/Don't know, Somewhat more, Much more. 
  - ***Women Stay Home***: Level of agreement with the following statement - Society would be better off if fewer women worked outside the home: Strongly disagree, Disagree, Don't know, Agree, Strongly agree. 
  - ***Do Women***: How much do you think should be done for women: Much less, Somewhat less, About the same/Don't know, Somewhat more, Much more. 
  - ***Feelings Liberal***: Feeling thermometer for leader of the Liberal party (Martin in 04 and 06, Dion in 08, Ignatieff in 11 and Trudeau in 15 and 19). 
  - ***Feelings Conservative***: Feeling thermometer for the leader of the Conservative party (Harper 04-15, Scheer 19)
  - ***Feelings NDP***: Feeling thermometer for the leader of the NDP (Layton 94-11, Mulcair 15, Singh 19)
  - ***Feelings BQ***: Feeling thermometer for the Bloc Quebecois leader (Duceppe 04-15, Blanchet 19)
  - ***Feelings Incumbent***: Feeling thermometer for the Incumbent party (Liberal in 2004, 2006 and 2019, Conservative in 2008, 2011 and 2015). 

### Methods

We used [mice](https://www.jstatsoft.org/article/view/v045i03), a package in R that imputes missing data with the "Multiple Imputation by Chained Equations" algorithm.  This is widely regarded as the standard method for imputing data.  We use the default imputation methods for all the variables and create five imputed datasets that we use in all of the calculations presented in the app.  

Since each of the dependent variables (vote for party, vote for incumbent and vote turnout) is binary (i.e, it only has two values 0 (No) and 1 (Yes)), we use a binomial generalized linear model (GLM) to estimate the relationships between the dependent variable and the independent variables.  For variables that have category labels above, we treat them as categorical by creating dummy variables for all but the first level of the variable and include the set of dummy variables for each variable.  The refernece categories for the variables in the dataset are as follows: 

<div style="display: flex; justify-content: center; align-items: center">
<table id="refs">
<thead>
<tr>
<th class="block" style="text-align: center;">Variable</th>
<th class="block" style="text-align: center;">Reference Group</th>
</tr>
</thead>
<tbody>
<tr>
<td class="block" style="text-align: center;">Year</td>
<td class="block" style="text-align: center;">2004</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Age Group</td>
<td class="block" style="text-align: center;">18-34</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Region</td>
<td class="block" style="text-align: center;">Ontario</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Schooling</td>
<td class="block" style="text-align: center;">HS or less</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Religion</td>
<td class="block" style="text-align: center;">No religion</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Catholic</td>
<td class="block" style="text-align: center;">Not Catholic</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Union</td>
<td class="block" style="text-align: center;">No</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Personal Retrospective Economy</td>
<td class="block" style="text-align: center;">Got Worse</td>
</tr>
<tr>
<td class="block" style="text-align: center;">National Retrospective Economy</td>
<td class="block" style="text-align: center;">Got Worse</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Party ID</td>
<td class="block" style="text-align: center;">Liberal</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Party ID (Green = Other)</td>
<td class="block" style="text-align: center;">Liberal</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Party ID (BQ = Other)</td>
<td class="block" style="text-align: center;">Liberal</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Party ID (Green + BQ = Other)</td>
<td class="block" style="text-align: center;">Liberal</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Defence Spending</td>
<td class="block" style="text-align: center;">Less</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Environmental Spending</td>
<td class="block" style="text-align: center;">Less</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Immigration</td>
<td class="block" style="text-align: center;">Fewer</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Ties with US</td>
<td class="block" style="text-align: center;">Much More Distant</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Private Sector Create Jobs</td>
<td class="block" style="text-align: center;">Strongly Disagree</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Blame Self for Failure</td>
<td class="block" style="text-align: center;">Strongly Disagree</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Do More to Reduce Income Inequality</td>
<td class="block" style="text-align: center;">Strongly Disagree</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Women Stay at Home</td>
<td class="block" style="text-align: center;">Strongly Disagree</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Do for Women</td>
<td class="block" style="text-align: center;">Much Less</td>
</tr>
<tr>
<td class="block" style="text-align: center;">Vote</td>
<td class="block" style="text-align: center;">Liberal</td>
</tr>
</tbody>
</table>
</div>

We used the [survey](https://www.jstatsoft.org/article/view/v009i08) package in R to generate nationally representative estimates based on the sample weights from each survey.  In 2004-2008, these are described in the data as "National Weights", in 2011 they are weights to sample for the RDD respondents and corrected for unequal probabilities, in 2015 these are Weights to the sample based on a post-stratification of age and gender to true population counts and in 2019 these are aggregate weights for CES respondents based on province and ...  The descriptive statistics and cross-tabulations are based on the weighted, non-imputed data.  The models are based on the weighted and imputed data.  

### Inputs and Outputs

The inputs and outputs to the various panels are described in the virtual walk-throughs that you can access by pressing the "Walk me Through" button and then working through the steps.  However, we recapitulate those descriptions here. 

#### Descriptives Input: 
  - ***Summarise Variable***: Numeric summaries will be produced for this varaible.   
  - ***Group By***: This is a grouping variable.  The summaries that are produced for the summarised variable will be produced independently for each different level of the "Group By" variable. 
  - ***Number of Bins***: When a histogram is produced, this numeric input box accepts an integer value (i.e., no decimal places) that tells R how many different bars the histogram should have.  This will be "approximately" respected.  Based on the distribution of the variable, it may not give you exactly the number you ask for. 
  
#### Descriptives Output: 
  - ***Summary***: The summary statistics, which include the mean, standard deviation, minimum, maximum, quartiles, and the number of valid and missing observations.  By way of interpretation, the mean and median (50%) values are good measures of centre.  The mean and median will be very similar when the variable is symmetrically distributed, but the median is somewhat more useful if the distribution is skewed.  It will be closer to the mode of the distribution.  The summaries will be done independently for each value of the grouping variable.  
  - ***Histogram***: When no grouping variable is selected a histogram giving the distribution of the summarised variable is produced.  This plot is rendered with [plotly](https://plotly.com/r/), so hoverinf over the top of the plot will activate some itneractive functions.  The camera icon allows you to save a picture of of the graph to your computer.  Hovering your cursor over the bars will produce a tooltip that has the data values used to make the chart in them.  The `mids` value is the middle of the bar and the `height` value is the proportion of observations in that group.  
  - ***Error Bar Chart***: When a grouping variable is selected, you get a plot with points and lines.  The points represent the mean of the summarised variable for each value of the grouping variable.  The values of the grouping variable are given on the x-axis (the horizontal one).  The bars are the 95% confidence intervals.  These give the set of non-rejectable null hypothesis values for the mean.  
  - ***Factorplot***: When a grouping variable is present, you also get what is called a [factorplot](https://journal.r-project.org/archive/2013/RJ-2013-021/index.html).  This shows you whether or not the means for each value of the grouping variable are statistically different from each other.  The values of the grouping variable are arrayed on the left and top axes, such that the intersection of the row and column of the chart identify all possible pairs of values.  The box contains two numbers - the top is the difference in the two means and the bottom is the standard error of the difference.  The box is colored dark gray if the group in the row has a significantly higher mean than the group in the column.  It's colored light gray if the group in the row has a significantly smaller mean than the group in the column.  The box is white if the two groups are not statistically distinguishable from each other.  
  
  
#### Cross-Tabs Inputs
The cross-tabs interface allows you to make a contingency table. 

  - ***Row Variable***: identifies the variable that goes in the row of the contingency table.  If you have one, this should be the dependent variable (i.e., the thing you're trying to explain).  
  - ***Column Variable***: identifies the variable that goes in the columns of the contingency table.  If you have one, this should be the independent variable (i.e., the thing you're using to explain the dependent variable). 
  
#### Cross-Tabs Outputs
  - ***Contingency Table***: When only a row-variable is selected, you get a frequency and relative frequency distribution of the variable.  The frequency (count) is in parentheses and the relative frequency (column percentage) is the main entry.  If a column variable is selcted, the format of the table is the same, counts are in parentheses and column percentages are the main entries.  The general idea is to look across the rows of the table to see if there are any systematic patterns in the column percentages.  
  - ***Chi-squared***: When a column variable is selected, a $\chi^2$ (pronounced $k\overline{i}$, like the first syllable of Kylo Ren and not chai like the tea or "chee" like the first syllable of chia).  This tests whether the row and column variables are independent, the null hypothesis being that they are. 
  - ***Mosaic Plot***: When a column variable is selected, a mosaic plot is produced.  This is a visual representation of the contingency table. The columns of the figure represent the columns of the table.  The width of each is proportional to the number of observations in each group in the column variable.  Within each column, the height of each bar is a representation of the column proportion. This display was also rendered with [plotly](https://plotly.com/r/), so hovering over the top of the plot activates some interactive controls, including a button to save the plot to your computer (the button is shaped like a camera).  Also, hovering over any of the boxes in the plot will give you the data for that box. 
  
#### Models Inputs
  - ***Dependent Variable***: This is what you're trying to explain.  This could be either vote for a party, vote for the incumbent or vote turnout.  Note that choosing vote for the Bloc Quebecois will necesarily restrict your sample to only those observations in Quebec.  
  - ***Model 1***: The app allows you to comapre two models if you want, but if you only want to estimate a single model, you should add variables to model 1.  By clicking in the box, a list will appear of the variables that you can use.  You can either scroll through the list, or start typing and it will sequentially filter down the list to those that start with the typed characters.  
  - ***Model 2***: You can specify model 2 to compare to model 1.  Generally, this would be a model with more controls or it would be the model that includes the important variables in addition to the controls.  It could also be a completely different specification.  There are no constraints on the variables that can go into both models, though it does have implications for other model outputs (discussed below).  
  - ***Independent Variable of Interest***: This is a variable for which you want to have a better understanding of its effect.  When you select a variable here, an effect plot is made for the variable in the "Plot" tab and predicted probability tables are produced in the "Predicted Probabilities" tab.  When only one model is estimated, this field is automatically populated with the variables that are in that model.  When two models are estimated, this field is automatically populated with the variables that are common to both models.  
  - ***Vary Effect by...***: This allows you to vary the effect of the variable by year or region.  What happens is that an interaction is included between the variable of interest and either year or region.  
  - ***Regions***: Allows you to exclude regions from the analysis by unchecking the box by the region name.  The models, plots and tables will be re-rendered without the unselected regions.  
  - ***Years***: Allows you to exclude years from the analysis by unchecking the box by the year.  This works the same way as the region check boxes.  
  
#### Model Outputs
  - ***Model Output***
    - ***Model Coefficients***: The coefficient table gives the logistic regression coefficients that describe the relationship between the dependent and independent variables.  Note that these are not directly interpretable as changes in probability.  The units of the dependent variable are the log of the odds of engaging in the dependent variable activity (e.g., voting for the Liberal party).  The odds are just the probability of voting for the party divided by the probability of not voting for the party.  This is not an intuitive metric for lots of people, so the effects plots and predicted probabilities are intended to shed some light on the nature of the relationships.  What the coefficients can do is tell us whether there is a statistically significant relationship between the variables.  
    - ***Model Fit Statistics***: These gives us a sense of how the model fits.  The PRE (proportional reduction in error) tells us by what factor we can reduce our errors in predicting votes by using the model that we estimated.  In this case, observations are classified as voting for the party if their predicted probability of bigger than 0.5 and are predicted to not have voted for the party otherwise.  The expected PRE takes into account not the classification, but the predicted probability itself following Michael Herron's [article](https://doi.org/10.1093/oxfordjournals.pan.a029806). These numbers can't be higher than 1, which means we have an error-free classification from our model.  However, they can be less than zero, which means that we are actively misclassifying observations with the estimated model. 
  - ***Plot***: The plots that get produced are effects plots similar to those produced by [Fox](https://www.jstatsoft.org/article/view/v032i01).  The points in the plot are the predicted probabilities of voting for the selected party for an average observation.  The average observation is one that takes on median values of continuous variables and modal values of categorical variables for all but the variable of interest.  The variable of interest is then moved over its range to produce the plot.  By clicking the "Show Plot Controls" button, you will have access to colour pickers for for model 1 and model 2 points, sliders to move the color legend around in the plotting region, numeric inputs to change the size of the plot, inputs to change the relative position of the x- and y-axis titles, an input to rotate the x-axis tick mark labels and boxes to change the x- and y-axis titles.  When two models are estimated, predicted probabilities are presented for both models.  When the effects are varied by year or region, the plot changes to one where each year or region is represented in its own small panel with the name identifying the year or region in the light gray area above each panel.  In both cases, the plot controls work the same. 
  - ***Predicted Probabilities***
    - ***Predicted Probabilities***: The top table presents the predicted probabilities of voting for the selected party for each of the values of the independent variable of interest or for all combinations fo the groups of the independent variable of interest and year or region if the effects are varied by year or region.  Essentially, this table (and the one for model 2 if a second model is estimated) are the data that go into making the plot.  
    - ***Difference in Predicted Probabilities***: The second table gives the difference in predicted probabilities.  If the effects do not vary by year or region, you will see two columns with a label relating to the independent variable of interest.  These two columns define a pairwise difference.  That is, for example if you picked age, the first row would have "18-34" in the first column and "35-54" in the second column.  The number in the "Predicted Probability" column would give the difference in the predicted probability of voting for the selected party for those two groups.  Thus if it was positive, it would mean that the 18-34 group was more likely to vote for the party than the 35-54 group and vice versa if it were negative.  The 95% confidence interval allows you to evaluate whether that difference is statistically significant.  When the effects are varied by year, the tables can get quite long and there will be a page selector at the bottom of the table, because the table will only print 10 rows.  You can also filter the table by clicking in the boxes right under than column headings.  Clicking in the box will either activate a dropdown menu from which you can select the values you want to keep in that column or it will activate a range slider with which you can identify the range of values you want to keep in the column. 
  