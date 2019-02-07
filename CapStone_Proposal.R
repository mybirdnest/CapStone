---
  title: "CapStone Proposal"
author: "Herman Toeante"
date: "January 27, 2019"
output:
  html_document: default
pdf_document: default
---
  
####Analyzing Sales Win or Loss
  
What is the problem you want to solve?
  
  + I want to use sales pipeline data to see opportunities that can be turned into successful deals. 
Better understanding of sales pipeline can help any sales team organization can expect win or lose based on data. 

Who is your client and why do they care about this problem?
  
  + Any sales organization like B2B company. Some examples of business-to-business sales include Nautilus 
selling equipment to Gold's Gym, the cloud-based document storage company Dropbox services businesses as well 
as individuals. General Electric makes plenty of consumer goods, but also provides parts for other enterprises. 
Perhaps you've worked at a company where the paychecks were stamped by ADP, a company that provides payroll and
financial services for businesses. Xerox is a household name but makes billions on providing paper and 
print services to businesses. With this knowledge, any sales team will be able to understand deeper their
sales pipeline and will be able to optimize their sales deals.


What data are you going to use for this? How will you acquire this data?
  
  * The data comes from IBM Watson analytic sample data. 
(https://community.watsonanalytics.com/wp-content/uploads/2015/04/WA_Fn-UseC_-Sales-Win-Loss.csv) 
The csv file contain ~78K row of data. Each row represents sales opportunity for four supplies group; 
Car Accessories, Car Electronic, Performance & Non-auto, Tires & Wheels.

In brief, outline your approach to solving this problem (knowing that this might change later)

+ Data wrangling
+ Exploratory Data Analysis:
  ++ Descriptive statistics such as box plot, histogram, bar chart
+ Regression model to predict most sucessful elements that goes into successful deals
(linear regression, random forrest, etc)

What are your deliverables? Typically, this would include code, along with a paper and/or a slide deck

+ The deliverables will be an R script code containing the code used for data wrangling, data exploration and regression model.
+ The paper will be data story and visualization exploring other external factors 
+ A slide deck power point presentation 




