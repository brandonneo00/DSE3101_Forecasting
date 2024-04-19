# DSE3101 Forecasting Group 3

### 🚀**Where creactivity meets functionality**🚀

---

### **Frontend:**
RShiny


### **Backend:**
R for AR and ADL model
Python for Random Forest model using scikit-learn


### **Set-up:** 
To run the app, please download the main branch of our repository from Github here
In addition, please make sure you have these packages installed locally on your device as these will be required by the app to run:

Shiny, shinyjs, shinycssloaders*, shinythemes, plotly, ggplot2, readxl, dplyr, tidyr, stringr,  BVAR, zoo, DT, dynlm, reticulate

To install these packages you can run this in the console of RStudio:
- install.packages(“packageName”)
- For instance, to install shiny, simply run install.packages(“shiny”)
- Note: As one we are using one feature from the shinycssloaders package that is not available from the CRAN installation via install.packages(), please run this line in your console instead to get the developmental feature directly from the package developer. 
- install.packages(“remotes”)
- library(remotes)
- remotes::install_github("daattali/shinycssloaders", force = TRUE)
- You may be required to enter your github password when installing the shinycssloaders, thereafter you will be be prompted by the console to update some of the existing packages in R and you are encouraged to select the “1. All'' option so that the packages will run without error.

Regarding reticulate and RF model:
- When sourcing the Random Forest py scripts using reticulate, you may be required to install the following python packages: pandas, numpy, scikit-learn (version 1.4.1post1), matplotlib.
- To install these python packages you can run in your console:
- library(reticulate)
- py_install(“pandas”) for example

After setting up, simply click Run App on the top right-hand corner of RStudio and a pop-up of the app will appear on your screen. 
