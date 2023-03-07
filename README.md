# Judicial Workforce Modelling R Package (jwmodel)

Top tip: if you need to convert an existing R project into an R package use the following code.  
`usethis::create_package("/path/to/project/directory")`  
You can get the path to your current directory by first running `getwd()`.

Test link to [Technical Documentation](./docs/tech-guide.md).

___

Changes have been made to the jwmodel package to create a more efficient process when it comes to producing model outputs. 
For more detail on improvements to the S&D model, please refer to this document.   

The dev branch in jwmodel acts as a temporary master. Changes on working branches should be pushed to dev and then a pull request is needed to merge dev with master.  dev exists to preserve master until code/file structure has been verified and tested by colleagues. 

### Outputs 

The raw outputs from the S&D model are not sent to customers directly. They are imported into Excel, then pivot tables/graphs are produced. 
We are adding to the jwmodel package so that it produces the desired outputs directly. This will save time as the model is often run several times before the final output is passed to the customer. 

The original jwmodel package (model) is run to get the raw results. The following scripts build the tables/graphs which feed the output for customers:

•	days_sat_yj.R – produces a table of total sitting days per year for each judge type.
