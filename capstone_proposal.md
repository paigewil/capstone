### Problem

In recent years, there has been a growing spotlight on the interactions between police and citizens, causing many protests throughout the country and poor relationships with law enforcement. In an effort to provide an objective understanding, this project will use machine learning to investigate what factors predict stop outcome and arrests made. And, which (if any) of those factors are demographic, possibly contributing to the issue of police profiling.

### Client

There are two possible clients who would benefit from the findings of this project:<br />

1.  Police departments seeking to identify areas of bias and improve community relationships.
    -   In an effort to confront implicit bias and learn strategies to combat it, many police departments, like the Madison Police Department (“Are All Cops Racist?” 2015), have implemented police bias training programs. To further aid in this initiative, the findings from this project can help identify areas of improvement for a specific department so the training can be catered and relevant. The self-reflection and acknowledgement of shortcomings also provides law enforcement the opportunity to reach out to affected communities and work to better relationships, something the MPD has already started doing (Vetterkind 2015).
2.  General public/ government groups seeking to investigate and hold offending police departments accountable.
    -   With public scrutiny mounting, having an objective perspective on the offenses of police departments is important for transparency and truth. The project findings also help provide such groups with a way to form a list of departments to focus efforts on. The Justice Department’s Office of Community Oriented Policing Services (COPS) and civil rights division (Barrett 2017) partake in a number of initiatives to investigate and report on police departments failings as well as provide training. The work from this project could be used by those programs, and others similar, as an additional tool to help effectively and efficiently complete inquiries.

Overall, the analysis from this project can provide police, government groups, and concerned citizens with an objective perspective of the existing biases within departments (particularly, with this use case, the Connecticut State Police). This knowledge can help with investigation efforts and inform necessary improvement actions.

#### Cost:

The cost of these trainings can be expensive ($4.5 million for the NYPD (Dimon and Parascandola 2018) and $100-$300 per officer per day for 3 days to several weeks for another (Neuhauser 2016)) so having a clear understanding of a police department's biases will make the training more tailored and effective, ensuring the best investment.

More than the cost of the training, the hours required, as enumerated above, add an additional cost in terms of time away from officers performing their regular policing duties.

Additionally, the analytical techniques utilized in this project provide efficiency to the work of the governmental departments, allowing for both a wider reach to more police departments and a deeper analysis of those they investigate.

### Data

The data I will be using comes from [The Stanford Open Policing Project](https://openpolicing.stanford.edu/) (Pierson et al. 2017) and contains information on traffic stops throughout the United States. For this project, I will be focusing on data from Connecticut since, compared to other states, it includes more features for analysis. In total, the dataset contains 24 features across 318,669 observations (traffic stops), covering the time period from 10/1/2013 through 3/31/2015. The features are largely categorical, including information such as the driver's gender and race, county of stop, if a search was conducted, and resulting violations. The dataset has been standardized from the raw data provided by the [Connecticut Data Collaborative](http://ctrp3.ctdata.org/rawdata/) and includes some new fields, including a unique ID for each stop, county information (name and FIPS ([Federal Information Processing Standard codes](https://en.wikipedia.org/wiki/Federal_Information_Processing_Standards)) codes, which are standard codes used by the US goverment), and stop duration.

Additionally, to conduct an accurate analysis, I will need to calculate proportions of total demographic population for many of the features. I will be using a [2014 US Census Bureau dataset](https://www.census.gov/data/datasets/2016/demo/popest/counties-detail.html#tables) (Bureau, n.d.) to pull the necessary denominators for demographic population proportions of gender, age, and race.

Other external datasets might be incorporated for comparison as needed depending on what the data unveils and what questions arise.

Dataset header:

<table class="table" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
id
</th>
<th style="text-align:left;">
state
</th>
<th style="text-align:left;">
stop\_date
</th>
<th style="text-align:left;">
stop\_time
</th>
<th style="text-align:left;">
location\_raw
</th>
<th style="text-align:left;">
county\_name
</th>
<th style="text-align:right;">
county\_fips
</th>
<th style="text-align:left;">
fine\_grained\_location
</th>
<th style="text-align:left;">
police\_department
</th>
<th style="text-align:left;">
driver\_gender
</th>
<th style="text-align:right;">
driver\_age\_raw
</th>
<th style="text-align:right;">
driver\_age
</th>
<th style="text-align:left;">
driver\_race\_raw
</th>
<th style="text-align:left;">
driver\_race
</th>
<th style="text-align:left;">
violation\_raw
</th>
<th style="text-align:left;">
violation
</th>
<th style="text-align:left;">
search\_conducted
</th>
<th style="text-align:left;">
search\_type\_raw
</th>
<th style="text-align:left;">
search\_type
</th>
<th style="text-align:left;">
contraband\_found
</th>
<th style="text-align:left;">
stop\_outcome
</th>
<th style="text-align:left;">
is\_arrested
</th>
<th style="text-align:left;">
officer\_id
</th>
<th style="text-align:left;">
stop\_duration
</th>
<th style="text-align:left;">
stop\_date\_time
</th>
<th style="text-align:left;">
stop\_date\_time2
</th>
<th style="text-align:left;">
violation\_raw1
</th>
<th style="text-align:left;">
violation\_raw2
</th>
<th style="text-align:left;">
violation\_raw3
</th>
<th style="text-align:left;">
violation\_raw4
</th>
<th style="text-align:left;">
violation\_raw5
</th>
<th style="text-align:right;">
violation\_count
</th>
<th style="text-align:left;">
stop\_duration\_fact
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CT-2013-00001
</td>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
2013-10-01
</td>
<td style="text-align:left;">
0:01
</td>
<td style="text-align:left;">
westport
</td>
<td style="text-align:left;">
Fairfield County
</td>
<td style="text-align:right;">
9001
</td>
<td style="text-align:left;">
00000 N I 95 (WESTPORT, T158) X 18 LL
</td>
<td style="text-align:left;">
State Police
</td>
<td style="text-align:left;">
F
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:left;">
Black
</td>
<td style="text-align:left;">
Black
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
Speeding
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Ticket
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
1000002754
</td>
<td style="text-align:left;">
1-15 min
</td>
<td style="text-align:left;">
10/1/2013 0:01
</td>
<td style="text-align:left;">
2013-10-01 00:01:00
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1-15 min
</td>
</tr>
<tr>
<td style="text-align:left;">
CT-2013-00002
</td>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
2013-10-01
</td>
<td style="text-align:left;">
0:02
</td>
<td style="text-align:left;">
mansfield
</td>
<td style="text-align:left;">
Tolland County
</td>
<td style="text-align:right;">
9013
</td>
<td style="text-align:left;">
rte 195 storrs
</td>
<td style="text-align:left;">
State Police
</td>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
White
</td>
<td style="text-align:left;">
White
</td>
<td style="text-align:left;">
Moving Violation
</td>
<td style="text-align:left;">
Moving violation
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Verbal Warning
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
1000001903
</td>
<td style="text-align:left;">
1-15 min
</td>
<td style="text-align:left;">
10/1/2013 0:02
</td>
<td style="text-align:left;">
2013-10-01 00:02:00
</td>
<td style="text-align:left;">
Moving Violation
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1-15 min
</td>
</tr>
<tr>
<td style="text-align:left;">
CT-2013-00003
</td>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
2013-10-01
</td>
<td style="text-align:left;">
0:07
</td>
<td style="text-align:left;">
franklin
</td>
<td style="text-align:left;">
New London County
</td>
<td style="text-align:right;">
9011
</td>
<td style="text-align:left;">
Rt 32/whippoorwill
</td>
<td style="text-align:left;">
State Police
</td>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Hispanic
</td>
<td style="text-align:left;">
Hispanic
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
Speeding
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Ticket
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
1000002711
</td>
<td style="text-align:left;">
1-15 min
</td>
<td style="text-align:left;">
10/1/2013 0:07
</td>
<td style="text-align:left;">
2013-10-01 00:07:00
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1-15 min
</td>
</tr>
<tr>
<td style="text-align:left;">
CT-2013-00004
</td>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
2013-10-01
</td>
<td style="text-align:left;">
0:10
</td>
<td style="text-align:left;">
danbury
</td>
<td style="text-align:left;">
Fairfield County
</td>
<td style="text-align:right;">
9001
</td>
<td style="text-align:left;">
I-84
</td>
<td style="text-align:left;">
State Police
</td>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:left;">
Black
</td>
<td style="text-align:left;">
Black
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
Speeding
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Written Warning
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
113658284
</td>
<td style="text-align:left;">
1-15 min
</td>
<td style="text-align:left;">
10/1/2013 0:10
</td>
<td style="text-align:left;">
2013-10-01 00:10:00
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1-15 min
</td>
</tr>
<tr>
<td style="text-align:left;">
CT-2013-00005
</td>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
2013-10-01
</td>
<td style="text-align:left;">
0:10
</td>
<td style="text-align:left;">
east hartford
</td>
<td style="text-align:left;">
Hartford County
</td>
<td style="text-align:right;">
9003
</td>
<td style="text-align:left;">
00000 W I 84 (EAST HARTFORD, T043)E.OF XT.56
</td>
<td style="text-align:left;">
State Police
</td>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
White
</td>
<td style="text-align:left;">
White
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
Speeding
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Ticket
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
830814942
</td>
<td style="text-align:left;">
1-15 min
</td>
<td style="text-align:left;">
10/1/2013 0:10
</td>
<td style="text-align:left;">
2013-10-01 00:10:00
</td>
<td style="text-align:left;">
Speed Related
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1-15 min
</td>
</tr>
<tr>
<td style="text-align:left;">
CT-2013-00006
</td>
<td style="text-align:left;">
CT
</td>
<td style="text-align:left;">
2013-10-01
</td>
<td style="text-align:left;">
0:10
</td>
<td style="text-align:left;">
trumbull
</td>
<td style="text-align:left;">
Fairfield County
</td>
<td style="text-align:right;">
9001
</td>
<td style="text-align:left;">
rt 8
</td>
<td style="text-align:left;">
State Police
</td>
<td style="text-align:left;">
M
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Black
</td>
<td style="text-align:left;">
Black
</td>
<td style="text-align:left;">
Defective Lights
</td>
<td style="text-align:left;">
Lights
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
Verbal Warning
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:left;">
230931989
</td>
<td style="text-align:left;">
1-15 min
</td>
<td style="text-align:left;">
10/1/2013 0:10
</td>
<td style="text-align:left;">
2013-10-01 00:10:00
</td>
<td style="text-align:left;">
Defective Lights
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:left;">
NA
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
1-15 min
</td>
</tr>
</tbody>
</table>

### Approach

A supervised machine learning algorithm will be used to predict if a traffic stop will end in arrest and what the stop outcome will be. Additionally, time-series analysis will be conducted to investigate the influence of time/day on traffic stop outcome.

One issue to address is that the demographic spread of predictor variables in the dataset may not align with the state/county demographic spread, potentially altering our perception of a characteristic’s influence on outcome. Therefore, I will need to weight each demographic characteristic by its distribution in the total population (Connecticut and/or county). Additionally, the dataset does not cover complete calendar years, reporting only part of 2013 and 2015. For any summary time analysis done, I will need to apply weights, for example, to account for April only appearing in 2014 while March appears in 2014 and 2015.

### Deliverables

The following items will be available on the project's [GitHub repository](https://github.com/paigewil/capstone):<br />

-   R code for:
    -   Data wrangling
    -   Exploratory data analysis <br />   -&gt; including plots
    -   Machine learning
-   Report explaing the analysis conducted and associated findings
-   Slide deck presentation, catered to a non-technical audience

### References

“Are All Cops Racist?” 2015. The Daily Show with Trevor Noah. October. <https://www.youtube.com/watch?v=3QIWolLM9i8>.

Barrett, Devlin. 2017. “Justice Department Ends Program Scrutinizing Local Police Forces.” *The Washington Post*, September. <https://www.washingtonpost.com/world/national-security/justice-department-ends-program-scrutinizing-local-police-forces/2017/09/15/ee88d02e-9a3d-11e7-82e4-f1076f6d6152_story.html?utm_term=.9f66520651a8>.

Bureau, US Census. n.d. “County Population by Characteristics: 2010-2016.” <https://www.census.gov/data/datasets/2016/demo/popest/counties-detail.html#tables>.

Dimon, Laura, and Rocco Parascandola. 2018. “NYPD to Start ’Implicit Bias’ Training.” *PoliceOne*, February. <https://www.policeone.com/patrol-issues/articles/470642006-NYPD-to-start-implicit-bias-training/>.

Neuhauser, Alan. 2016. “Can Training Really Stop Police Bias?” *U.S. News*, December. <https://www.usnews.com/news/national-news/articles/2016-12-29/can-training-really-stop-police-bias>.

Pierson, E., C. Simoiu, J. Overgoor, S. Corbett-Davies, V. Ramachandran, C. Phillips, and S. Goel. 2017. “A large-scale analysis of racial disparities in police stops across the United States.” *ArXiv E-Prints*, June.

Vetterkind, Riley. 2015. “MPD Employs Unconscious Bias Training to Combat Racism in Force.” *The Badger Herald*, March. <https://badgerherald.com/news/2015/03/23/mpd-employs-unconscious-bias-training-to-combat-racism-in-force/>.
