# Continuity Equations explored (continuityequations-thesis) #
This project aims to find the model with the best anomaly detection capability in the continuity equations domain.

Continuous assurance is a methodology to provide assurance on financial data on a near real-time basis. One of the fundamental elements of continuous assurance is continuous data auditing  in which the integrity of the data provided by the client is tested. Continuity equations can be used to evidence assertions regarding data integrity. In order to do so, data is tested by predicting subsequent values based on a fitting model.

This approach is based on a paper by [Kogan et al. (2010)](http://raw.rutgers.edu/docs/Innovations/Continuity%20Equations.pdf) [1] in which several models are tested.

## Models ##
In this package the models proposed by Kogan et al. are implemented. These models are:

- Lineary Regreggion Model (LRM)
- Simultaneous Equations Model (SEM)
- Vector Autoregressive Model (VAR)
- Restricted Autoregressive Model (RVAR)
- Autoregressive Integrated Moving Averge Model (ARIMA)
- Cascaded VAR-RVAR model
- Cascaded ARIMA-RVAR model

You can read more about these models and their (dis)advantages in the research paper of Kogan et al. and on Wikipedia.

## R Environment ##
The R language has recently gathered significant attention from the financial sector. As a language focused on data analysis and statistics, it has the potential to become the language of choice in finance. R has some important advantages compared to the commercially available SAS language. First of all R is an open source language. It is available for free from the
[R Project website](http://www.r-project.org/ "The R Project for Statistical Computing"). Furthermore, due to its open source availability, thousands of practitioners and developers have contributed to the base code and packages related to a plethora of fields including finance.

Another advantage of R is that it can be installed on almost any target platform. Furthermore, R code can be executed without being invoked by a user, but as an automated script via cron or as an additional stage in an existing ETL procedure.

### Lead Developer ###

I am [Erik van Kempen](http://nl.linkedin.com/in/evankempen), a lecturer in Statistics, ERP and Business Intelligence at Fontys Univerity of Applied Sciences in Eindhoven, The Netherlands. Prior to my lectureship, I obtained a Bachelor of Business Administration in Accountancy.

My current research interests are in the areas continuous assurance, smart auditing, business intelligence and process mining.

## References ##
1. A. Kogan, M. G. Alles, M. A. Vasarhelyi, and J.Wu. Analytical Procedures for continuous data level auditing: Continuity equations. 2010.
