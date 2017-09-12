
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

On EC2 one can use this [AMI](http://www.louisaslett.com/RStudio_AMI/) with R and Rstudio server already
installed. Installing Java and the h2o R package is easy:
```
sudo apt-get install default-jre
R
install.packages("h2o")
```

TODO: what else?


