### Problem

In recent years, there has been a growing spotlight on the treatment (or rather, mistreatment) of citizens by police, particularly, citizens in the black community. This has caused many protests throughout the country and poor relationships with law enforcement. In an effort to police the police, this project will use machine learning to investigate what factors predict stop outcome and arrests made. And, which of those factors are demographic, contributing to the issue of police profiling.

### Client

There are two possible clients who would benefit from the findings of this project:<br />

1.  Police departments seeking to identify areas of bias and improve community relationships.
    -   In an effort to confront implicit bias and learn strategies to combat it, many police departments, like the Madison Police Department (“Are All Cops Racist?” 2015), have implemented police bias training programs. To further aid in this initiative, the findings from this project can help identify areas of improvement for a specific department so the training can be catered and relevant. The self-reflection and acknowledgement of shortcomings also provides law enforcement the opportunity to reach out to affected communities and work to better relationships, something the MPD has already started doing (Vetterkind 2015).
2.  General public/ government groups seeking to investigate and hold offending police departments accountable.
    -   With public scrutiny mounting, having an objective perspective on the offenses of police departments is important for transparency and truth. The project findings also help provide such groups with a way to form a list of departments to focus efforts on. The Justice Department’s Office of Community Oriented Policing Services (COPS) and civil rights division (Barrett 2017) partake in a number of initiatives to investigate and report on police departments failings as well as provide training. The work from this project could be used by those programs, and others similar, as an additional tool to help effectively and efficiently complete inquiries.

Overall, the analysis from this project can provide police, government groups, and concerned citizens with an objective perspective of the existing biases within departments (particularly, with this use case, the Connecticut State Police). This knowledge can help with investigation efforts and inform necessary improvement actions.

#### Cost:

The cost of these trainings can be expensive ($4.5 million for the NYPD (Dimon and Parascandola 2018) and $100-$300 per officer per day for 3 days to several weeks for another (Neuhauser 2016)) so having a clear understanding of a police department's biases will make the training more tailored and effective, ensuring the best investment.

Additionally, the analytical techniques utilized in this project provide efficiency to the work of the governmental departments, allowing for both a wider reach to more police departments and a deeper analysis of those they investigate.

### Data

The data I will be using comes from [The Stanford Open Policing Project](https://openpolicing.stanford.edu/) (Pierson et al. 2017) and contains information on traffic stops throughout the United States. For this project, I will be focusing on data from Connecticut since, compared to other states, it includes more features for analysis. In total, the dataset contains 24 features across 318,669 observations (traffic stops), covering the time period from 10/1/2013 through 3/31/2015. The features are largely categorical, including information such as the driver's gender and race, county of stop, if a search was conducted, and resulting violations. The dataset has been standardized from the raw data provided by the [Connecticut Data Collaborative](http://ctrp3.ctdata.org/rawdata/) and includes some new fields, including a unique ID for each stop, county information (name and FIP), and stop duration.

Additionally, to conduct an accurate analysis, I will need to calculate proportions of total demographic population for many of the features. I will be using a [2014 US Census Bureau dataset](https://www.census.gov/data/datasets/2016/demo/popest/counties-detail.html#tables) (Bureau, n.d.) to pull the necessary denominators for demographic population proportions of gender, age, and race.

Other external datasets might be incorporated for comparison as needed depending on what the data unveils and what questions arise.

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
