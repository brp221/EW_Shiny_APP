# EW_Shiny_APP

Energy Dashboard R Shiny Application is a mini side project for/of the Lehigh Sustainability Dashboard Team.
The primary goals of this application are to 
    
    1.  be an extension of the project's frontend application. For example, the frontend app can, upon user request, direct the main website's traffic to 
        a website hosting the R Shiny App. It is advised that in such a scenario, the app should be deployed on (a) reliable and "big-data-appropriate"
        server(s). See instructions below. The website visitors will be able to easily and efficiently interact with certain metrics such as yearbuilt, square building footage, building type, etc. 
    2.  act as a data management tool. By exposing such an in depth and diverse set of visualizations, statistics and data patterns, the team will be able to provide  
        energy usage statistics, not only to themselves as a way to verify the validity of data ebing presented, but also to many campus entities such as professors and thus students in environmental classes and sustainability groups.    

Advice : 

Deploy the application on Lehigh's server cluster if possible. 3.8 million and counting rows (Initium) is a lot for an R Data Frame to handle. There are also caching techniques that can be applied in the script's server function. 

Code Breakdown: 

Most R Shiny apps are divided in the server and ui (user interface) functions. The server has a field called output which can be though of as an array of visualizations and graphs. The user interface has an input field which can be thought as an array of components( sliders, radios, checkboxes, buttons ...) which allow users to interact with the data. In addition, to be able to run this script, one must do it from a windows environment because the argisbinding package is unavailable on MacOS. Windows development environemnt is highly recommended as ArcGIS Pro (Desktop) is necessary for the importation and conversion of AGOL-hosted Originis and Initium tables into R Dataframes. 

Best of luck, 
Lehigh EW Dashboard Team  

