# Potential_Cusotmer_Profile_Detection
Part 1. Pre-processing
1.1. Location: transform the current location version into (1) City, (2) State, (3) Country
  1.1.1. Applying conditional functions & keywords scanning in the location variable to detect suburbs and classify into corresponding City, creating new "City" variable
  1.1.2. Applying the same process to generate the "States" variable
  1.1.3. Applying the same process to generate the "Country" variable
1.2. Name: 
  1.2.1 remove all non-letter characters & remove contents inside the brackets in both firstName and lastName
  1.2.2 create the new fullName

Part 2. Identify people who are relevant to business development & startup
2.1. Applying keywords search on (1) jobTitle, (2) jobTitle2, and (3) description columns
   2.1.1. Assumptions: people might have 2 jobs at the same time. They can put it into whether jobTitle or jobTitle 2
   2.1.2. List of keywords: star, venture, investor, entrepreneur, founder, business

2.2. Setting the condition that people have to working at Google at the moment
2.3. Removing people with irrelevant profile such as HR Business Partner, Summber Business Intern, Recruiter, etc
