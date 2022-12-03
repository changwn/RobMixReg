## Test environments
* local Window, R 3.5.1
* Ubuntu Xenial 16.04 which is default operation, R 3.6.2 which is default version (on travis-ci)


## R CMD check results
There were no ERRORs or WARNINGs. 



##2020-02-25:
There was 2 NOTE:

* checking CRAN incoming feasibility ... NOTE

  This is the new submittion of the 'RobMixReg' package.

* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘cran-comments.md’
  
  The 'cran-comments.md' file use as the log file. 


##2020-03-04:

This submission revised as below description based on the Jelena Saf's comments.

1. Rewrite the description of title in DESCRIPTION file.

2. Add author information by using the Author@R way.

3. Suppressed or annotated the useless information prited on the console.

4. In the main function ("rmr"), enable automatic testing for the example.

5. Rewrite the document (.Dd documentation) for all the functions.

##2020-03-05

Thanks the response from the Uwe Ligges.

This submission fixed the doi issue in the DESCRIPTION file.

##2020-03-13

Thanks the response from the Jelena Saf's comments.

1. Add explaination for the abbreviation of algorithms in the DESCRIPTION file.

2. Add example for each algorithm and enalble auto testing.


##2020-03-19

Thanks the comments from the Swetlana Herbrandt.

1. Revise the mispelling in the DESCRIPTION file.

2. Replace the \dontrun by \donttest since the test take more than 5 sec and thus cause the NOTE.


##2020-05-10

Thanks the commens from the Uwe Ligges.

This submission fixed the URL issue in the readme file. The URL of the package was given in the canonical form.

##2020-08-03

Thanks the commens from the Uwe Ligges.

This submission update the date field in DESCRIPTION.
