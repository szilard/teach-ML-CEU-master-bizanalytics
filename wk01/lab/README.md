
## Lab wk01

### Goals

After this first lab every student should have installed and working on their laptops:

- R
- RStudio
- some R packages TBD
- Java (needed for h2o)
- h2o (from R, run [this](h2o-test-installation.R) to test if it is working)

should have 

- a github account
- an AWS account with $100 worth of free student credits

should be able to 

- create a simple Rmarkdown file, run it in RStudio and get html report
- upload files to their github account
- start an EC2 instance, create an image, stop/restart, configure security groups (open ports)
- do same work in RStudio server on EC2 and in RStudio locally on laptop

On EC2 one can use this [AMI](http://www.louisaslett.com/RStudio_AMI/) with R and RStudio server already
installed. Please change the default password for the `rstudio` user (e.g. as described 
at the previous link). Install Java (required by h2o) via command line (e.g. by logging in via ssh): 
```
sudo apt-get install default-jre
```
(then you can install h2o as an R package from RStudio).

Warning: The Rstudio connection from your browser does not use SSL, therefore don't use this server
on projects requiring security (e.g. with confidential data) or set up SSL connection (https) before
you connect to RStudio the very first time.




