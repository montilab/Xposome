
## API EXPLORER

<p>The API Explorer provides documentation on how users can retrieve data from the Xposome application and use them to create their next project. All successful responses are returned in JSON. Only queries that respond with a 200 response code is considered successful.</p>

<p>Here are standard HTTP codes that you will find in the “Status” element of the response body.</p>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="status-table">
  <tr>
    <td><strong>Status Code</strong></td>
    <td><strong>Description</strong></td>
  </tr>
  <tr>
    <td>200 OK</td>
    <td>Standard HTTP successful response</td>
  </tr>
  <tr>
    <td>404 Bad Request or Source Not Found</td>
    <td>Standard HTTP invalid request response</td>
  </tr>
  <tr>
    <td>500 Internal Server Error</td>
    <td>There was an unexpected error on our server. If you see this error, please notify us about the issue on our <a target="blank" href="https://github.com/montilab/Xposome/">GitHub page</a></td>
  </tr>
</table>

<br>

<p>There are four data sources that are currently returned from the API.</p>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="source-table">
  <tr>
    <td><strong>Data</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Return</strong></td>
  </tr>
  <tr>
    <td>Projects</td>
    <td>a list of projects or chemical screenings available in the GeneHive Database. Visit our <a href="https://montilab.bu.edu/Xposome/">webpage</a> to see on how we define each project</td>
    <td>list</td>
  </tr>
  <tr>
    <td>Chemicals</td>
    <td>a list of chemicals/cas ids available in a particular project in the GeneHive database</td>
    <td>data frame</td>
  </tr>
  <tr>
    <td>Datasets</td>
    <td>a list of data sources for generating the Xposome Portal such as profile and chemical annotation, expression set, gene set enrichment, connectivity set, and K2-taxonomer</a></td>
    <td>data frame or RDS file</td>
  </tr>
  <tr>
    <td>RDS Bundle</td>
    <td>a bundle of RDS files for a given Xposome project, including profile annotation, chemical annotation, expression set, gene set enrichment, connectivity set, and K2-taxonomer</a></td>
    <td>zip file</td>
  </tr> 
  <tr>
    <td>Statistics</td>
    <td>a summary of statistic given by a chemical in a Xposome project of interest. The statistics are calculated based on the scores obtained from the expression set, enrichment set and connectivity set</a></td>
    <td>data frame</td>
  </tr>
</table>

<br>

Below are a list of R packages and GET requests in R to retrieve data from the Xposome projects. However, one can use Python or other programs to implement the REST API calls.

<!-- packages: start -->
<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>


```r
# R packages required for API calls
library(K2Taxonomer)
library(Biobase)
library(jsonlite)
library(httr)
```



<!-- packages: end -->

<!-- Projects: start -->
### Projects

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('projects')" class="btn btn-default action-button">GET</button></td>
    <td>&#47;projects</td>
    <td>Return a list of projects available in the GeneHive database</td>
  </tr>
</table>
  
<br>

<div id="projects-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/projects?orderby=asc">https://montilab.bu.edu/Xposome-API/projects?orderby=asc</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>orderby</td>
    <td>asc (default) or desc</td>
    <td>Sort options</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A list of values</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
  # url for local testing
  url0 <- "https://montilab.bu.edu/Xposome-API/projects?orderby=asc"
  
  # Send GET Request to API
  res <- GET(url = url0, encode = "json")
  
  # Check the status of GET request 
  test_request <- tryCatch({
    
    stop_for_status(res)
    
    "pass"
    
  }, error = function(e) {
    
    "fail"
    
  })
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
  # If GET request is successful, return the results
  if(test_request == "pass"){
    
    projects <- fromJSON(fromJSON(rawToChar(res$content)))
    print(head(projects))
    
  }
</pre>
<pre class='output'> ADIPO HEPG2 MCF10A TG-GATEs </pre>

<br>

</div>
<!-- Projects: end -->

<!-- Chemical: start -->
### Chemicals

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('chemicals')" class="btn btn-default action-button">GET</button></td>
    <td>&#47;chemicals</td>
    <td>Return a list of chemicals/cas ids available in a particular project in the GeneHive database</td>
  </tr>
</table>
  
<br>

<div id="chemicals-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/chemicals?projects=ADIPO&chemical_ids=all">https://montilab.bu.edu/Xposome-API/chemicals?projects=ADIPO&chemical_ids=all</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>all (default)</td>
    <td>a project name or a list of concatenated projects or all projects in the GeneHive database. If a list is provided, they must be concatenated and separated by a comma.</td>
    <td>string</td>
  </tr>
    <tr>
    <td>chemical_ids</td>
    <td>all (default)</td>
    <td>a chemical id/cas id or a list of concatenated chemical ids/cas ids or all chemical ids/cas ids in the GeneHive database. If a list is provided, they must be concatenated and separated by a comma.</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A list of values</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
  # url for local testing
  url <- "https://montilab.bu.edu/Xposome-API/chemicals?projects=ADIPO&chemical_ids=all"
  
  # Send GET Request to API
  res <- GET(url = url, encode = "json")
  
  # Check the status of GET request 
  test_request <- tryCatch({
    
    stop_for_status(res)
    
    "pass"
    
  }, error = function(e) {
    
    "fail"
    
  })
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
  # If GET request is successful, return the results
  if(test_request == "pass"){
    
    chemicals <- fromJSON(fromJSON(rawToChar(res$content)))
    print(head(chemicals))
    
  }
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;"> Chemical_Id </th>
   <th style="text-align:left;"> BUID </th>
   <th style="text-align:left;"> Project </th>
   <th style="text-align:left;"> Chemical_Name </th>
   <th style="text-align:left;"> CAS </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> ADIPO </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BADGE </td>
   <td style="text-align:left;"> BUID_2 </td>
   <td style="text-align:left;"> ADIPO </td>
   <td style="text-align:left;"> Bisphenol A diglycidyl ether </td>
   <td style="text-align:left;"> 1675-54-3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DOSS </td>
   <td style="text-align:left;"> BUID_3 </td>
   <td style="text-align:left;"> ADIPO </td>
   <td style="text-align:left;"> Dioctyl sulfosuccinate sodium </td>
   <td style="text-align:left;"> 577-11-7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MBuP </td>
   <td style="text-align:left;"> BUID_4 </td>
   <td style="text-align:left;"> ADIPO </td>
   <td style="text-align:left;"> Mono-n-butyl phthalate </td>
   <td style="text-align:left;"> 131-70-4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ProPara </td>
   <td style="text-align:left;"> BUID_5 </td>
   <td style="text-align:left;"> ADIPO </td>
   <td style="text-align:left;"> Propylparaben </td>
   <td style="text-align:left;"> 94-13-3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCCP </td>
   <td style="text-align:left;"> BUID_6 </td>
   <td style="text-align:left;"> ADIPO </td>
   <td style="text-align:left;"> Tris(1-chloro-2-propyl) phosphate </td>
   <td style="text-align:left;"> 13674-87-5 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- chemical: end -->

<!-- Datasets - profile annotation: start -->
### Datasets

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('profile-annotation')" class="btn btn-default action-button">GET</button></td>
    <td>/profile_annotation</td>
    <td>Return the profile annotation of a specific project</td>
  </tr>
</table>

<br>

<div id="profile-annotation-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/profile_annotation?project=ADIPO">https://montilab.bu.edu/Xposome-API/profile_annotation?project=ADIPO</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A data frame object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url1 <- paste0("https://montilab.bu.edu/Xposome-API/profile_annotation?project=ADIPO")

# Send GET Request to API
res <- GET(url = url1, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  profile_annotation <- fromJSON(fromJSON(rawToChar(res$content)))
  n_row <- ifelse(nrow(profile_annotation) > 10, 10, nrow(profile_annotation))
  n_col <- ifelse(ncol(profile_annotation) > 10, 10, ncol(profile_annotation))  
  print(profile_annotation[1:n_row, 1:n_col])

}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;"> Sig_Id </th>
   <th style="text-align:left;"> Chemical_Id </th>
   <th style="text-align:left;"> BUID </th>
   <th style="text-align:left;"> Plate </th>
   <th style="text-align:left;"> PPARg_Mod </th>
   <th style="text-align:left;"> unique_ID_by_chem </th>
   <th style="text-align:right;"> TAS </th>
   <th style="text-align:right;"> Number_of_Replicates </th>
   <th style="text-align:left;"> Chemical_Name </th>
   <th style="text-align:left;"> CAS </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Vehicle_A1_plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A2_plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A3_plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A4_plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A5_plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A6_plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 25 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BADGE_A7_plate1 </td>
   <td style="text-align:left;"> BADGE </td>
   <td style="text-align:left;"> BUID_2 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> 10uM </td>
   <td style="text-align:right;"> 0.1324 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Bisphenol A diglycidyl ether </td>
   <td style="text-align:left;"> 1675-54-3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DOSS_A8_plate1 </td>
   <td style="text-align:left;"> DOSS </td>
   <td style="text-align:left;"> BUID_3 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Suspected </td>
   <td style="text-align:left;"> 5uM </td>
   <td style="text-align:right;"> 0.1588 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Dioctyl sulfosuccinate sodium </td>
   <td style="text-align:left;"> 577-11-7 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MBuP_A9_plate1 </td>
   <td style="text-align:left;"> MBuP </td>
   <td style="text-align:left;"> BUID_4 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> 10uM </td>
   <td style="text-align:right;"> 0.2817 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Mono-n-butyl phthalate </td>
   <td style="text-align:left;"> 131-70-4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ProPara_A10_plate1 </td>
   <td style="text-align:left;"> ProPara </td>
   <td style="text-align:left;"> BUID_5 </td>
   <td style="text-align:left;"> plate1 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:left;"> 10uM </td>
   <td style="text-align:right;"> 0.1756 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Propylparaben </td>
   <td style="text-align:left;"> 94-13-3 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Datasets - profile annotation: end -->

<!-- Datasets - chemical annotation: start -->
<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('chemical-annotation')" class="btn btn-default action-button">GET</button></td>
    <td>/chemical_annotation</td>
    <td>Return the chemical annotation of of a specific project</td>
  </tr>
</table>

<br>

<div id="chemical-annotation-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/chemical_annotation?project=ADIPO">https://montilab.bu.edu/Xposome-API/chemical_annotation?project=ADIPO</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A data frame object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url2 <- paste0("https://montilab.bu.edu/Xposome-API/chemical_annotation?project=ADIPO")

# Send GET Request to API
res <- GET(url = url2, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  chemical_annotation <- fromJSON(fromJSON(rawToChar(res$content)))
  n_row <- ifelse(nrow(chemical_annotation) > 10, 10, nrow(chemical_annotation))
  n_col <- ifelse(ncol(chemical_annotation) > 10, 10, ncol(chemical_annotation))  
  print(chemical_annotation[1:n_row, 1:n_col])

}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;"> Chemical_Id </th>
   <th style="text-align:left;"> BUID </th>
   <th style="text-align:left;"> PPARg_Mod </th>
   <th style="text-align:right;"> TAS </th>
   <th style="text-align:left;"> Chemical_Name </th>
   <th style="text-align:left;"> CAS </th>
   <th style="text-align:left;"> Source_Use </th>
   <th style="text-align:right;"> Any_Except_MORT </th>
   <th style="text-align:right;"> Any_Effect </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:left;"> BUID_1 </td>
   <td style="text-align:left;"> Vehicle </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BADGE </td>
   <td style="text-align:left;"> BUID_2 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 0.1324 </td>
   <td style="text-align:left;"> Bisphenol A diglycidyl ether </td>
   <td style="text-align:left;"> 1675-54-3 </td>
   <td style="text-align:left;"> Plastics </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DOSS </td>
   <td style="text-align:left;"> BUID_3 </td>
   <td style="text-align:left;"> Suspected </td>
   <td style="text-align:right;"> 0.1588 </td>
   <td style="text-align:left;"> Dioctyl sulfosuccinate sodium </td>
   <td style="text-align:left;"> 577-11-7 </td>
   <td style="text-align:left;"> Surfactant </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MBuP </td>
   <td style="text-align:left;"> BUID_4 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 0.2817 </td>
   <td style="text-align:left;"> Mono-n-butyl phthalate </td>
   <td style="text-align:left;"> 131-70-4 </td>
   <td style="text-align:left;"> Phthalate </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ProPara </td>
   <td style="text-align:left;"> BUID_5 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 0.1756 </td>
   <td style="text-align:left;"> Propylparaben </td>
   <td style="text-align:left;"> 94-13-3 </td>
   <td style="text-align:left;"> Paraben </td>
   <td style="text-align:right;"> 0.0064 </td>
   <td style="text-align:right;"> 0.0064 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TCCP </td>
   <td style="text-align:left;"> BUID_6 </td>
   <td style="text-align:left;"> Suspected </td>
   <td style="text-align:right;"> 0.1994 </td>
   <td style="text-align:left;"> Tris(1-chloro-2-propyl) phosphate </td>
   <td style="text-align:left;"> 13674-87-5 </td>
   <td style="text-align:left;"> Flame retardant </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 15dPGJ </td>
   <td style="text-align:left;"> BUID_7 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 0.1345 </td>
   <td style="text-align:left;"> 15-deoxy-_12,14-prostaglandin J2 </td>
   <td style="text-align:left;"> 87893-55-8 </td>
   <td style="text-align:left;"> Anti-inflammatory prostagladin </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FM550 </td>
   <td style="text-align:left;"> BUID_8 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 0.3511 </td>
   <td style="text-align:left;"> Firemaster 550 </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> Organophosphate flame retardant mixture </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PCB126 </td>
   <td style="text-align:left;"> BUID_9 </td>
   <td style="text-align:left;"> No </td>
   <td style="text-align:right;"> 0.1931 </td>
   <td style="text-align:left;"> 3,3',4,4',5-Pentachloro-1,1'-biphenyl </td>
   <td style="text-align:left;"> 57465-28-8 </td>
   <td style="text-align:left;"> PCB congener </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TBT </td>
   <td style="text-align:left;"> BUID_10 </td>
   <td style="text-align:left;"> Yes </td>
   <td style="text-align:right;"> 0.2210 </td>
   <td style="text-align:left;"> Tributyltin </td>
   <td style="text-align:left;"> 1461-22-9 </td>
   <td style="text-align:left;"> Organotin </td>
   <td style="text-align:right;"> 0.6400 </td>
   <td style="text-align:right;"> 0.6400 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Datasets - chemical annotation: end -->

<!-- Datasets - expression set: start -->
<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('expression-set')" class="btn btn-default action-button">GET</button></td>
    <td>/expression_set</td>
    <td>Return the gene expression dataset of a specific project</td>
  </tr>
</table>

<br>

<div id="expression-set-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/expression_set?project=ADIPO">https://montilab.bu.edu/Xposome-API/expression_set?project=ADIPO</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>An RDS file that contains a S4 class object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url3 <- paste0("https://montilab.bu.edu/Xposome-API/expression_set?project=ADIPO")

# Send GET Request to API
res <- GET(url = url3, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  temp = tempfile()
  download.file(url3, temp)
  expression_set <- readRDS(temp)
  expression_set = as.data.frame(exprs(expression_set), row.names=rownames(expression_set), col.names=colnames(expression_set))
  unlink(temp)

  n_row <- ifelse(nrow(expression_set) > 10, 10, nrow(expression_set))
  n_col <- ifelse(ncol(expression_set) > 10, 10, ncol(expression_set))  
  print(expression_set[1:n_row, 1:n_col])
  
}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Vehicle </th>
   <th style="text-align:right;"> BADGE </th>
   <th style="text-align:right;"> DOSS </th>
   <th style="text-align:right;"> MBuP </th>
   <th style="text-align:right;"> ProPara </th>
   <th style="text-align:right;"> TCCP </th>
   <th style="text-align:right;"> 15dPGJ </th>
   <th style="text-align:right;"> FM550 </th>
   <th style="text-align:right;"> PCB126 </th>
   <th style="text-align:right;"> TBT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TBC1D19 </td>
   <td style="text-align:right;"> 0.9854783 </td>
   <td style="text-align:right;"> 0.9183977 </td>
   <td style="text-align:right;"> 3.9043683 </td>
   <td style="text-align:right;"> 1.9911483 </td>
   <td style="text-align:right;"> 0.2587427 </td>
   <td style="text-align:right;"> 0.2587427 </td>
   <td style="text-align:right;"> 1.460779 </td>
   <td style="text-align:right;"> 0.9259503 </td>
   <td style="text-align:right;"> 0.8218200 </td>
   <td style="text-align:right;"> 1.785381 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KAT2B </td>
   <td style="text-align:right;"> 1.2936310 </td>
   <td style="text-align:right;"> 1.2313300 </td>
   <td style="text-align:right;"> 0.8276963 </td>
   <td style="text-align:right;"> 0.6086715 </td>
   <td style="text-align:right;"> 0.9896311 </td>
   <td style="text-align:right;"> 1.4387002 </td>
   <td style="text-align:right;"> 1.562328 </td>
   <td style="text-align:right;"> 0.9881016 </td>
   <td style="text-align:right;"> 0.8724393 </td>
   <td style="text-align:right;"> 1.780027 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SENP1 </td>
   <td style="text-align:right;"> 2.7890887 </td>
   <td style="text-align:right;"> 3.8955916 </td>
   <td style="text-align:right;"> 4.0329100 </td>
   <td style="text-align:right;"> 2.2599568 </td>
   <td style="text-align:right;"> 1.9632624 </td>
   <td style="text-align:right;"> 1.7260205 </td>
   <td style="text-align:right;"> 2.591780 </td>
   <td style="text-align:right;"> 0.8670406 </td>
   <td style="text-align:right;"> 1.5178942 </td>
   <td style="text-align:right;"> 1.173074 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NANS </td>
   <td style="text-align:right;"> 4.2344725 </td>
   <td style="text-align:right;"> 4.4043247 </td>
   <td style="text-align:right;"> 5.1700819 </td>
   <td style="text-align:right;"> 3.7570559 </td>
   <td style="text-align:right;"> 5.7290794 </td>
   <td style="text-align:right;"> 4.7660793 </td>
   <td style="text-align:right;"> 5.174618 </td>
   <td style="text-align:right;"> 3.6533242 </td>
   <td style="text-align:right;"> 3.0205792 </td>
   <td style="text-align:right;"> 3.799000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NT5DC1 </td>
   <td style="text-align:right;"> 5.1409356 </td>
   <td style="text-align:right;"> 5.2313958 </td>
   <td style="text-align:right;"> 5.5163142 </td>
   <td style="text-align:right;"> 5.3159728 </td>
   <td style="text-align:right;"> 4.0403332 </td>
   <td style="text-align:right;"> 4.9961953 </td>
   <td style="text-align:right;"> 5.559957 </td>
   <td style="text-align:right;"> 5.0179303 </td>
   <td style="text-align:right;"> 5.1121659 </td>
   <td style="text-align:right;"> 5.349136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BOD1 </td>
   <td style="text-align:right;"> 2.5691829 </td>
   <td style="text-align:right;"> 2.6742158 </td>
   <td style="text-align:right;"> 3.7593142 </td>
   <td style="text-align:right;"> 1.5797618 </td>
   <td style="text-align:right;"> 1.5276108 </td>
   <td style="text-align:right;"> 4.2906855 </td>
   <td style="text-align:right;"> 2.545050 </td>
   <td style="text-align:right;"> 2.5347853 </td>
   <td style="text-align:right;"> 4.1529584 </td>
   <td style="text-align:right;"> 1.819487 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HDGFRP3 </td>
   <td style="text-align:right;"> 5.8861815 </td>
   <td style="text-align:right;"> 5.3245516 </td>
   <td style="text-align:right;"> 6.0714604 </td>
   <td style="text-align:right;"> 5.4574326 </td>
   <td style="text-align:right;"> 5.4791426 </td>
   <td style="text-align:right;"> 5.9288517 </td>
   <td style="text-align:right;"> 5.705752 </td>
   <td style="text-align:right;"> 4.6758954 </td>
   <td style="text-align:right;"> 5.9336105 </td>
   <td style="text-align:right;"> 5.562597 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> AGPS </td>
   <td style="text-align:right;"> 3.6681115 </td>
   <td style="text-align:right;"> 4.3256908 </td>
   <td style="text-align:right;"> 1.8237485 </td>
   <td style="text-align:right;"> 4.3606792 </td>
   <td style="text-align:right;"> 1.7736112 </td>
   <td style="text-align:right;"> 4.0641143 </td>
   <td style="text-align:right;"> 3.198117 </td>
   <td style="text-align:right;"> 4.4455653 </td>
   <td style="text-align:right;"> 4.0651745 </td>
   <td style="text-align:right;"> 3.711304 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> D19ERTD737E </td>
   <td style="text-align:right;"> 5.6285289 </td>
   <td style="text-align:right;"> 6.1064236 </td>
   <td style="text-align:right;"> 6.0820447 </td>
   <td style="text-align:right;"> 4.4899933 </td>
   <td style="text-align:right;"> 6.1825441 </td>
   <td style="text-align:right;"> 5.3199648 </td>
   <td style="text-align:right;"> 6.012787 </td>
   <td style="text-align:right;"> 3.6206528 </td>
   <td style="text-align:right;"> 5.0556158 </td>
   <td style="text-align:right;"> 6.114513 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LRRC39 </td>
   <td style="text-align:right;"> 3.2374099 </td>
   <td style="text-align:right;"> 3.2860924 </td>
   <td style="text-align:right;"> 2.3610915 </td>
   <td style="text-align:right;"> 2.7032397 </td>
   <td style="text-align:right;"> 4.2427019 </td>
   <td style="text-align:right;"> 4.0962116 </td>
   <td style="text-align:right;"> 3.588227 </td>
   <td style="text-align:right;"> 3.2474486 </td>
   <td style="text-align:right;"> 2.8120747 </td>
   <td style="text-align:right;"> 1.581062 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Datasets - expression set: end -->

<!-- Datasets - enrichment set: start -->
<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('enrichment-set')" class="btn btn-default action-button">GET</button></td>
    <td>/enrichment_set</td>
    <td>Return the enrichment dataset of a specific project</td>
  </tr>
</table>

<br>

<div id="enrichment-set-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/enrichment_set?project=ADIPO">https://montilab.bu.edu/Xposome-API/enrichment_set?project=ADIPO</a></p>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>An RDS file that contains a S4 class object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url4 <- paste0("https://montilab.bu.edu/Xposome-API/enrichment_set?project=ADIPO")

# Send GET Request to API
res <- GET(url = url4, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  temp = tempfile()
  download.file(url4, temp)
  enrichment_set <- readRDS(temp)
  hallmark_set = as.data.frame(exprs(enrichment_set[["gsscores_h.all.v7.0_gsva"]]), row.names=rownames(enrichment_set), col.names=colnames(enrichment_set))
  unlink(temp)

  n_row <- ifelse(nrow(hallmark_set) > 10, 10, nrow(hallmark_set))
  n_col <- ifelse(ncol(hallmark_set) > 10, 10, ncol(hallmark_set))  
  print(hallmark_set[1:n_row, 1:n_col])
  
}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> Vehicle </th>
   <th style="text-align:right;"> BADGE </th>
   <th style="text-align:right;"> DOSS </th>
   <th style="text-align:right;"> MBuP </th>
   <th style="text-align:right;"> ProPara </th>
   <th style="text-align:right;"> TCCP </th>
   <th style="text-align:right;"> 15dPGJ </th>
   <th style="text-align:right;"> FM550 </th>
   <th style="text-align:right;"> PCB126 </th>
   <th style="text-align:right;"> TBT </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> HALLMARK_TNFA_SIGNALING_VIA_NFKB </td>
   <td style="text-align:right;"> 0.0904406 </td>
   <td style="text-align:right;"> -0.1119777 </td>
   <td style="text-align:right;"> -0.0493735 </td>
   <td style="text-align:right;"> 0.0029359 </td>
   <td style="text-align:right;"> 0.0376101 </td>
   <td style="text-align:right;"> 0.1257062 </td>
   <td style="text-align:right;"> 0.0908738 </td>
   <td style="text-align:right;"> -0.0084545 </td>
   <td style="text-align:right;"> 0.0394821 </td>
   <td style="text-align:right;"> 0.0885869 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_HYPOXIA </td>
   <td style="text-align:right;"> -0.1267899 </td>
   <td style="text-align:right;"> 0.0771355 </td>
   <td style="text-align:right;"> 0.0732410 </td>
   <td style="text-align:right;"> -0.0489057 </td>
   <td style="text-align:right;"> 0.0821673 </td>
   <td style="text-align:right;"> -0.1448307 </td>
   <td style="text-align:right;"> -0.0769246 </td>
   <td style="text-align:right;"> 0.0044052 </td>
   <td style="text-align:right;"> 0.0846731 </td>
   <td style="text-align:right;"> -0.0254707 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_CHOLESTEROL_HOMEOSTASIS </td>
   <td style="text-align:right;"> 0.2078710 </td>
   <td style="text-align:right;"> -0.0079686 </td>
   <td style="text-align:right;"> 0.0442348 </td>
   <td style="text-align:right;"> -0.1361093 </td>
   <td style="text-align:right;"> -0.0214002 </td>
   <td style="text-align:right;"> 0.0938392 </td>
   <td style="text-align:right;"> -0.2124600 </td>
   <td style="text-align:right;"> -0.0055005 </td>
   <td style="text-align:right;"> -0.2455590 </td>
   <td style="text-align:right;"> -0.0149356 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_MITOTIC_SPINDLE </td>
   <td style="text-align:right;"> -0.0827881 </td>
   <td style="text-align:right;"> 0.0028074 </td>
   <td style="text-align:right;"> -0.0015077 </td>
   <td style="text-align:right;"> 0.0787793 </td>
   <td style="text-align:right;"> 0.0775733 </td>
   <td style="text-align:right;"> 0.1050593 </td>
   <td style="text-align:right;"> 0.0392336 </td>
   <td style="text-align:right;"> 0.0046077 </td>
   <td style="text-align:right;"> 0.0410305 </td>
   <td style="text-align:right;"> 0.0660597 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_WNT_BETA_CATENIN_SIGNALING </td>
   <td style="text-align:right;"> 0.0269559 </td>
   <td style="text-align:right;"> -0.2888269 </td>
   <td style="text-align:right;"> 0.1856439 </td>
   <td style="text-align:right;"> 0.1284651 </td>
   <td style="text-align:right;"> -0.0649737 </td>
   <td style="text-align:right;"> -0.0116333 </td>
   <td style="text-align:right;"> -0.1044106 </td>
   <td style="text-align:right;"> -0.1122471 </td>
   <td style="text-align:right;"> -0.0231503 </td>
   <td style="text-align:right;"> -0.2133597 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_TGF_BETA_SIGNALING </td>
   <td style="text-align:right;"> 0.1666105 </td>
   <td style="text-align:right;"> -0.0930938 </td>
   <td style="text-align:right;"> -0.0707330 </td>
   <td style="text-align:right;"> 0.0391443 </td>
   <td style="text-align:right;"> 0.0961239 </td>
   <td style="text-align:right;"> 0.1272101 </td>
   <td style="text-align:right;"> 0.1527394 </td>
   <td style="text-align:right;"> 0.0183693 </td>
   <td style="text-align:right;"> -0.0604896 </td>
   <td style="text-align:right;"> -0.3115169 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_IL6_JAK_STAT3_SIGNALING </td>
   <td style="text-align:right;"> -0.0738390 </td>
   <td style="text-align:right;"> 0.0542788 </td>
   <td style="text-align:right;"> 0.1193460 </td>
   <td style="text-align:right;"> 0.0537128 </td>
   <td style="text-align:right;"> 0.0218549 </td>
   <td style="text-align:right;"> 0.1029276 </td>
   <td style="text-align:right;"> -0.1791577 </td>
   <td style="text-align:right;"> 0.2564177 </td>
   <td style="text-align:right;"> -0.0221698 </td>
   <td style="text-align:right;"> -0.0849579 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_DNA_REPAIR </td>
   <td style="text-align:right;"> 0.0139045 </td>
   <td style="text-align:right;"> 0.0855870 </td>
   <td style="text-align:right;"> 0.0421739 </td>
   <td style="text-align:right;"> -0.0146089 </td>
   <td style="text-align:right;"> 0.0454078 </td>
   <td style="text-align:right;"> -0.1583284 </td>
   <td style="text-align:right;"> -0.0075522 </td>
   <td style="text-align:right;"> 0.0583319 </td>
   <td style="text-align:right;"> 0.0696810 </td>
   <td style="text-align:right;"> -0.0151977 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_G2M_CHECKPOINT </td>
   <td style="text-align:right;"> 0.0390930 </td>
   <td style="text-align:right;"> 0.0828420 </td>
   <td style="text-align:right;"> 0.0431781 </td>
   <td style="text-align:right;"> 0.0571040 </td>
   <td style="text-align:right;"> 0.2008754 </td>
   <td style="text-align:right;"> -0.0055050 </td>
   <td style="text-align:right;"> 0.0982699 </td>
   <td style="text-align:right;"> -0.1472075 </td>
   <td style="text-align:right;"> -0.0274914 </td>
   <td style="text-align:right;"> -0.1342294 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_APOPTOSIS </td>
   <td style="text-align:right;"> -0.1282330 </td>
   <td style="text-align:right;"> 0.1466123 </td>
   <td style="text-align:right;"> -0.0843802 </td>
   <td style="text-align:right;"> 0.1447913 </td>
   <td style="text-align:right;"> -0.0165607 </td>
   <td style="text-align:right;"> 0.0520612 </td>
   <td style="text-align:right;"> 0.0373809 </td>
   <td style="text-align:right;"> -0.0505509 </td>
   <td style="text-align:right;"> -0.1748600 </td>
   <td style="text-align:right;"> 0.0206368 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Datasets - enrichment set: end -->

<!-- Datasets - connectivity set: start -->
<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('connectivity-set')" class="btn btn-default action-button">GET</button></td>
    <td>/connectivity_set</td>
    <td>Return the connectivity dataset of a specific project</td>
  </tr>
</table>

<br>

<div id="connectivity-set-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/connectivity_set?project=ADIPO">https://montilab.bu.edu/Xposome-API/connectivity_set?project=ADIPO</a></p>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>An RDS file that contains a S4 class object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url5 <- paste0("https://montilab.bu.edu/Xposome-API/connectivity_set?project=ADIPO")

# Send GET Request to API
res <- GET(url = url5, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  temp = tempfile()
  download.file(url5, temp)
  connectivity_set <- readRDS(temp)
  pcl_set = as.data.frame(exprs(connectivity_set[["pcl"]]), row.names=rownames(connectivity_set), col.names=colnames(connectivity_set))
  unlink(temp)

  n_row <- ifelse(nrow(pcl_set) > 10, 10, nrow(pcl_set))
  n_col <- ifelse(ncol(pcl_set) > 10, 10, ncol(pcl_set))  
  print(pcl_set[1:n_row, 1:n_col])
  
}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> BADGE </th>
   <th style="text-align:right;"> DOSS </th>
   <th style="text-align:right;"> MBuP </th>
   <th style="text-align:right;"> ProPara </th>
   <th style="text-align:right;"> TCCP </th>
   <th style="text-align:right;"> 15dPGJ </th>
   <th style="text-align:right;"> FM550 </th>
   <th style="text-align:right;"> PCB126 </th>
   <th style="text-align:right;"> TBT </th>
   <th style="text-align:right;"> BPS </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> CP_ADENOSINE_RECEPTOR_AGONIST </td>
   <td style="text-align:right;"> 4.4700856 </td>
   <td style="text-align:right;"> 16.4012184 </td>
   <td style="text-align:right;"> 0.1973424 </td>
   <td style="text-align:right;"> 2.0960071 </td>
   <td style="text-align:right;"> -27.144148 </td>
   <td style="text-align:right;"> -1.8499944 </td>
   <td style="text-align:right;"> 0.0227324 </td>
   <td style="text-align:right;"> 4.4699912 </td>
   <td style="text-align:right;"> -2.4803405 </td>
   <td style="text-align:right;"> 6.4132800 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_ANDROGEN_RECEPTOR_MODULATOR </td>
   <td style="text-align:right;"> 1.5248661 </td>
   <td style="text-align:right;"> 0.4320985 </td>
   <td style="text-align:right;"> 0.3182541 </td>
   <td style="text-align:right;"> -7.1018229 </td>
   <td style="text-align:right;"> -33.559708 </td>
   <td style="text-align:right;"> 0.0084388 </td>
   <td style="text-align:right;"> -0.0063584 </td>
   <td style="text-align:right;"> 2.2563090 </td>
   <td style="text-align:right;"> -8.8965626 </td>
   <td style="text-align:right;"> -0.0097843 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_ANGIOTENSIN_RECEPTOR_ANTAGONIST </td>
   <td style="text-align:right;"> 11.0661869 </td>
   <td style="text-align:right;"> 26.1897545 </td>
   <td style="text-align:right;"> 9.1982336 </td>
   <td style="text-align:right;"> 2.5659304 </td>
   <td style="text-align:right;"> -10.874252 </td>
   <td style="text-align:right;"> 0.0227324 </td>
   <td style="text-align:right;"> 2.9881787 </td>
   <td style="text-align:right;"> -0.0568311 </td>
   <td style="text-align:right;"> 0.0795635 </td>
   <td style="text-align:right;"> 0.1932257 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_AROMATASE_INHIBITOR </td>
   <td style="text-align:right;"> 8.0039291 </td>
   <td style="text-align:right;"> 0.0568311 </td>
   <td style="text-align:right;"> 0.8553747 </td>
   <td style="text-align:right;"> 9.7627163 </td>
   <td style="text-align:right;"> -17.019583 </td>
   <td style="text-align:right;"> -0.0909298 </td>
   <td style="text-align:right;"> 4.3478723 </td>
   <td style="text-align:right;"> 29.7367363 </td>
   <td style="text-align:right;"> 0.7956353 </td>
   <td style="text-align:right;"> 11.1879578 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_ATP_SYNTHASE_INHIBITOR </td>
   <td style="text-align:right;"> -5.2446504 </td>
   <td style="text-align:right;"> 5.7616372 </td>
   <td style="text-align:right;"> -0.7209885 </td>
   <td style="text-align:right;"> 15.9627619 </td>
   <td style="text-align:right;"> -5.308638 </td>
   <td style="text-align:right;"> 16.7657986 </td>
   <td style="text-align:right;"> 3.0037129 </td>
   <td style="text-align:right;"> 10.2576208 </td>
   <td style="text-align:right;"> -13.0289249 </td>
   <td style="text-align:right;"> -0.9320300 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_ATPASE_INHIBITOR </td>
   <td style="text-align:right;"> -19.9994335 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 48.9207077 </td>
   <td style="text-align:right;"> 21.8302612 </td>
   <td style="text-align:right;"> -36.551388 </td>
   <td style="text-align:right;"> 15.0262060 </td>
   <td style="text-align:right;"> -0.0454649 </td>
   <td style="text-align:right;"> -1.2784840 </td>
   <td style="text-align:right;"> -32.1550369 </td>
   <td style="text-align:right;"> 7.5554342 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_AURORA_KINASE_INHIBITOR </td>
   <td style="text-align:right;"> 22.5136375 </td>
   <td style="text-align:right;"> 11.7280579 </td>
   <td style="text-align:right;"> 7.4438858 </td>
   <td style="text-align:right;"> 0.1022960 </td>
   <td style="text-align:right;"> -50.398445 </td>
   <td style="text-align:right;"> 8.2106857 </td>
   <td style="text-align:right;"> 3.2715340 </td>
   <td style="text-align:right;"> 3.3285046 </td>
   <td style="text-align:right;"> -0.0113662 </td>
   <td style="text-align:right;"> 0.4432826 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_BACTERIAL_30S_RIBOSOMAL_SUBUNIT_INHIBITOR </td>
   <td style="text-align:right;"> 3.3060789 </td>
   <td style="text-align:right;"> 12.3660774 </td>
   <td style="text-align:right;"> -28.3346024 </td>
   <td style="text-align:right;"> -0.0227324 </td>
   <td style="text-align:right;"> -40.260521 </td>
   <td style="text-align:right;"> 0.9888611 </td>
   <td style="text-align:right;"> 0.4773812 </td>
   <td style="text-align:right;"> 9.4720840 </td>
   <td style="text-align:right;"> -3.6658309 </td>
   <td style="text-align:right;"> 2.1860495 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_BACTERIAL_CELL_WALL_SYNTHESIS_INHIBITOR </td>
   <td style="text-align:right;"> 0.3523528 </td>
   <td style="text-align:right;"> 8.3020267 </td>
   <td style="text-align:right;"> 3.7499871 </td>
   <td style="text-align:right;"> -0.2500568 </td>
   <td style="text-align:right;"> -27.002197 </td>
   <td style="text-align:right;"> 0.0087747 </td>
   <td style="text-align:right;"> -0.0227324 </td>
   <td style="text-align:right;"> 0.8989345 </td>
   <td style="text-align:right;"> -1.6273843 </td>
   <td style="text-align:right;"> 2.5708239 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_BACTERIAL_DNA_GYRASE_INHIBITOR </td>
   <td style="text-align:right;"> 12.0927153 </td>
   <td style="text-align:right;"> 27.4688931 </td>
   <td style="text-align:right;"> 0.0795635 </td>
   <td style="text-align:right;"> 1.9394438 </td>
   <td style="text-align:right;"> -16.786955 </td>
   <td style="text-align:right;"> -0.0454649 </td>
   <td style="text-align:right;"> 0.0000000 </td>
   <td style="text-align:right;"> 0.1477609 </td>
   <td style="text-align:right;"> -0.7627183 </td>
   <td style="text-align:right;"> 1.5891547 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Datasets - connectivity set: end -->

<!-- Datasets - k2taxonomer: start -->
<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('k2taxonomer')" class="btn btn-default action-button">GET</button></td>
    <td>/k2_taxonomer</td>
    <td>Return the K2Taxonomer dataset of a specific project</td>
  </tr>
</table>

<br>

<div id="k2taxonomer-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/k2_taxonomer?project=ADIPO">https://montilab.bu.edu/Xposome-API/k2_taxonomer?project=ADIPO</a></p>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>An RDS file that contains a K2 class object. Requires K2Taxonomer package to extract the content of the object.</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url6 <- paste0("https://montilab.bu.edu/Xposome-API/k2_taxonomer?project=ADIPO")

# Send GET Request to API
res <- GET(url = url6, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  temp = tempfile()
  download.file(url6, temp)
  k2_taxonomer <- readRDS(temp)
  k2_eset <- k2_taxonomer@eSet
  k2_eset = as.data.frame(exprs(k2_eset[["pcl"]]), row.names=rownames(k2_eset), col.names=colnames(k2_eset))
  unlink(temp)

  n_row <- ifelse(nrow(k2_eset) > 10, 10, nrow(k2_eset))
  n_col <- ifelse(ncol(k2_eset) > 10, 10, ncol(k2_eset))  
  print(k2_eset[1:n_row, 1:n_col])
  
}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> A4GALT </th>
   <th style="text-align:right;"> AA415398 </th>
   <th style="text-align:right;"> AA987161 </th>
   <th style="text-align:right;"> AI314180 </th>
   <th style="text-align:right;"> AI413582 </th>
   <th style="text-align:right;"> AI429214 </th>
   <th style="text-align:right;"> AI462493 </th>
   <th style="text-align:right;"> AI464131 </th>
   <th style="text-align:right;"> AI597468 </th>
   <th style="text-align:right;"> AI597479 </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Vehicle_A1_plate1 </td>
   <td style="text-align:right;"> 0.3352723 </td>
   <td style="text-align:right;"> 2.5694338 </td>
   <td style="text-align:right;"> 4.668912 </td>
   <td style="text-align:right;"> 5.461662 </td>
   <td style="text-align:right;"> 5.846231 </td>
   <td style="text-align:right;"> 2.0517120 </td>
   <td style="text-align:right;"> 6.338719 </td>
   <td style="text-align:right;"> 5.323084 </td>
   <td style="text-align:right;"> 6.0663552 </td>
   <td style="text-align:right;"> 4.2098021 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A2_plate1 </td>
   <td style="text-align:right;"> 0.3352723 </td>
   <td style="text-align:right;"> 3.2002954 </td>
   <td style="text-align:right;"> 2.528228 </td>
   <td style="text-align:right;"> 5.368768 </td>
   <td style="text-align:right;"> 5.115906 </td>
   <td style="text-align:right;"> 1.8120216 </td>
   <td style="text-align:right;"> 6.203153 </td>
   <td style="text-align:right;"> 5.245233 </td>
   <td style="text-align:right;"> 6.3305907 </td>
   <td style="text-align:right;"> 3.5816866 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A3_plate1 </td>
   <td style="text-align:right;"> 4.8245347 </td>
   <td style="text-align:right;"> 0.0458305 </td>
   <td style="text-align:right;"> 4.123881 </td>
   <td style="text-align:right;"> 0.517876 </td>
   <td style="text-align:right;"> 4.982514 </td>
   <td style="text-align:right;"> 3.4523083 </td>
   <td style="text-align:right;"> 6.135501 </td>
   <td style="text-align:right;"> 0.155168 </td>
   <td style="text-align:right;"> 4.8866486 </td>
   <td style="text-align:right;"> 3.7083779 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A4_plate1 </td>
   <td style="text-align:right;"> 0.3352723 </td>
   <td style="text-align:right;"> 0.0458305 </td>
   <td style="text-align:right;"> 0.109857 </td>
   <td style="text-align:right;"> 4.461609 </td>
   <td style="text-align:right;"> 3.975322 </td>
   <td style="text-align:right;"> 3.9039409 </td>
   <td style="text-align:right;"> 1.358417 </td>
   <td style="text-align:right;"> 4.430164 </td>
   <td style="text-align:right;"> 0.3011908 </td>
   <td style="text-align:right;"> 4.1125698 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A5_plate1 </td>
   <td style="text-align:right;"> 0.3352723 </td>
   <td style="text-align:right;"> 0.0458305 </td>
   <td style="text-align:right;"> 3.329431 </td>
   <td style="text-align:right;"> 5.541420 </td>
   <td style="text-align:right;"> 5.924800 </td>
   <td style="text-align:right;"> 2.6356366 </td>
   <td style="text-align:right;"> 5.721998 </td>
   <td style="text-align:right;"> 4.173408 </td>
   <td style="text-align:right;"> 5.7112642 </td>
   <td style="text-align:right;"> 3.8665003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Vehicle_A6_plate1 </td>
   <td style="text-align:right;"> 4.5802371 </td>
   <td style="text-align:right;"> 0.0458305 </td>
   <td style="text-align:right;"> 3.905445 </td>
   <td style="text-align:right;"> 3.047286 </td>
   <td style="text-align:right;"> 5.443109 </td>
   <td style="text-align:right;"> 2.2621766 </td>
   <td style="text-align:right;"> 6.434071 </td>
   <td style="text-align:right;"> 3.798731 </td>
   <td style="text-align:right;"> 5.6191103 </td>
   <td style="text-align:right;"> 0.0155112 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> BADGE_A7_plate1 </td>
   <td style="text-align:right;"> 0.3352723 </td>
   <td style="text-align:right;"> 3.6453002 </td>
   <td style="text-align:right;"> 3.891804 </td>
   <td style="text-align:right;"> 3.867025 </td>
   <td style="text-align:right;"> 5.141692 </td>
   <td style="text-align:right;"> -0.6739964 </td>
   <td style="text-align:right;"> 5.128981 </td>
   <td style="text-align:right;"> 4.340442 </td>
   <td style="text-align:right;"> 6.1633302 </td>
   <td style="text-align:right;"> 4.4118914 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DOSS_A8_plate1 </td>
   <td style="text-align:right;"> 2.8738926 </td>
   <td style="text-align:right;"> 1.4936811 </td>
   <td style="text-align:right;"> 4.388132 </td>
   <td style="text-align:right;"> 5.470593 </td>
   <td style="text-align:right;"> 5.730850 </td>
   <td style="text-align:right;"> 3.5502775 </td>
   <td style="text-align:right;"> 5.873959 </td>
   <td style="text-align:right;"> 4.262086 </td>
   <td style="text-align:right;"> 6.3623484 </td>
   <td style="text-align:right;"> 4.8857185 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MBuP_A9_plate1 </td>
   <td style="text-align:right;"> 2.3988572 </td>
   <td style="text-align:right;"> 1.8019350 </td>
   <td style="text-align:right;"> 3.297606 </td>
   <td style="text-align:right;"> 5.143850 </td>
   <td style="text-align:right;"> 5.558236 </td>
   <td style="text-align:right;"> 4.1121335 </td>
   <td style="text-align:right;"> 6.003917 </td>
   <td style="text-align:right;"> 4.853262 </td>
   <td style="text-align:right;"> 6.2946636 </td>
   <td style="text-align:right;"> 4.3757919 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ProPara_A10_plate1 </td>
   <td style="text-align:right;"> 0.3352723 </td>
   <td style="text-align:right;"> 1.8175507 </td>
   <td style="text-align:right;"> 4.283674 </td>
   <td style="text-align:right;"> 5.054467 </td>
   <td style="text-align:right;"> 5.262978 </td>
   <td style="text-align:right;"> 2.8390075 </td>
   <td style="text-align:right;"> 5.893924 </td>
   <td style="text-align:right;"> 4.161811 </td>
   <td style="text-align:right;"> 6.4454886 </td>
   <td style="text-align:right;"> 4.7284769 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Datasets - k2taxonomer: end -->

<!-- Datasets - rds_bundle: start -->
### RDS Bundle

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('rds_bundle')" class="btn btn-default action-button">GET</button></td>
    <td>/rds_bundle</td>
    <td>Return a bundle of RDS files for a given project</td>
  </tr>
</table>

<br>

<div id="rds_bundle-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/rds_bundle?project=ADIPO">https://montilab.bu.edu/Xposome-API/rds_bundle?project=ADIPO</a></p>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>An zip file that contains six RDS files for a given project. These RDS files are named as "Profile_Annotation.RDS", "Chemical_Annotation.RDS", "Gene_Expression.RDS", "Gene_Set_Enrichment.RDS", "Connectivity.RDS", and "K2Taxonomer.RDS"</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url7 <- paste0("https://montilab.bu.edu/Xposome-API/rds_bundle?project=ADIPO")

# Send GET Request to API
res <- GET(url = url7, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  file_dir = file.path(tempdir(), "ADIPO-Dataset")
  
  zip_file = paste0(file_dir, ".zip")
  
  download.file(url7, zip_file)
  
  profile_annotation <- readRDS(unzip(zipfile=zip_file, "Profile_Annotation.RDS", exdir=file_dir))
  
  chemical_annotation <- readRDS(unzip(zipfile=zip_file, "Chemical_Annotation.RDS", exdir=file_dir))
  
  expression_set <- readRDS(unzip(zipfile=zip_file, "Gene_Expression.RDS", exdir=file_dir))
  
  enrichment_set <- readRDS(unzip(zipfile=zip_file, "Gene_Set_Enrichment.RDS", exdir=file_dir))
  
  connectivity_set <- readRDS(unzip(zipfile=zip_file, "Connectivity.RDS", exdir=file_dir))
  
  k2_taxonomer <- readRDS(unzip(zipfile=zip_file, "K2Taxonomer.RDS", exdir=file_dir))
  
  unlink(zip_file); unlink(file_dir);

  print(list.files(file_dir))
  
}
</pre>
<pre class='output'><strong> Chemical_Annotation.RDS Connectivity.RDS Gene_Expression.RDS Gene_Set_Enrichment.RDS K2Taxonomer.RDS Profile_Annotation.RDS </strong></pre>

<br>

</div>
<!-- Datasets - rds_bundle: end -->

<!-- Statistics - gene expression: start -->
### Statistics

<table class="get-table">
  <tr>
    <td><button type="button" class="btn btn-default action-button" onclick="ToggleOperation('gene-expression-stat')">GET</button></td>
    <td>/gene_expression</td>
    <td>Return  a collection of differential expressed genes that exposed to a known toxin</td>
  </tr>
</table>

<br>

<div id="gene-expression-stat-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/gene_expression?project=ADIPO&chemical_id=EtHex&summarize.func=median&landmark=TRUE&do.markers=TRUE&do.scorecutoff=TRUE">https://montilab.bu.edu/Xposome-API/gene_expression?project=ADIPO&chemical_id=EtHex&summarize.func=median&landmark=TRUE&do.markers=TRUE&do.scorecutoff=TRUE</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
  <tr>
    <td>chemical_id</td>
    <td>1,2-Dibromo-3-chloropropane or ...</td>
    <td>Name of the chemicals or CAS ids of a chemical in a specific project (see <span class="highlight-text">chemicals API</span>)</td>
    <td>string</td>
  </tr>
  <tr>
    <td>summarize.func</td>
    <td>median (default) or mean/min/max/Q1/Q3</td>
    <td>Name of the summarize functions</td>
    <td>string</td>
  </tr>
  <tr>
    <td>landmark</td>
    <td>TRUE or FALSE (default)</td>
    <td>Whether to include landmark genes</td>
    <td>boolean</td>
  </tr>
  <tr>
    <td>do.nmarkers</td>
    <td>TRUE (default) or FALSE</td>
    <td>Whether to filter by the number of up- and down- regulated genes</td>
    <td>boolean</td>
  </tr>
  <tr>
    <td>nmarkers_up</td>
    <td>1000 (default) if do.nmarkers=TRUE</td>
    <td>Number of up-pregulated genes</td>
    <td>Positive integer</td>
  </tr>
  <tr>
    <td>nmarkers_down</td>
    <td>1000 (default) if do.nmarkers=TRUE</td>
    <td>Number of down-pregulated genes</td>
    <td>Positive integer</td>
  </tr>
  <tr>
    <td>do.scorecutoff</td>
    <td>TRUE (default) or FALSE</td>
    <td>Whether to filter by the z-scores</td>
    <td>boolean</td>
  </tr>
  <tr>
    <td>scorecutoff_lb</td>
    <td>-2 (default) if do.scorecutoff=TRUE</td>
    <td>Lower bound of the z-scores cutoff</td>
    <td>integer</td>
  </tr>
  <tr>
    <td>scorecutoff_ub</td>
    <td>2 (default) if do.scorecutoff=TRUE</td>
    <td>Upper bound of the z-scores cutoff</td>
    <td>integer</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A data frame object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url8 <- paste0("https://montilab.bu.edu/Xposome-API/gene_expression?project=ADIPO&chemical_id=EtHex&summarize.func=median&landmark=TRUE&do.markers=TRUE&do.scorecutoff=TRUE")

# Send GET Request to API
res <- GET(url = url8, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})

</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  gene_expression_stat <- fromJSON(fromJSON(rawToChar(res$content)))
  n_row <- ifelse(nrow(gene_expression_stat) > 10, 10, nrow(gene_expression_stat))
  n_col <- ifelse(ncol(gene_expression_stat) > 10, 10, ncol(gene_expression_stat))
  print(gene_expression_stat[1:n_row, 1:n_col])

}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Gene </th>
   <th style="text-align:left;"> Direction </th>
   <th style="text-align:right;"> Summary Score </th>
   <th style="text-align:right;"> ModZScore 10uM </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TAF5L </td>
   <td style="text-align:left;"> TAF5L </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0014 </td>
   <td style="text-align:right;"> -0.0014 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PYGL </td>
   <td style="text-align:left;"> PYGL </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0016 </td>
   <td style="text-align:right;"> -0.0016 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> MRPL50 </td>
   <td style="text-align:left;"> MRPL50 </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0034 </td>
   <td style="text-align:right;"> -0.0034 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CD3EAP </td>
   <td style="text-align:left;"> CD3EAP </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0054 </td>
   <td style="text-align:right;"> -0.0054 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> YTHDF2 </td>
   <td style="text-align:left;"> YTHDF2 </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0136 </td>
   <td style="text-align:right;"> -0.0136 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TOP3A </td>
   <td style="text-align:left;"> TOP3A </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0139 </td>
   <td style="text-align:right;"> -0.0139 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NRK </td>
   <td style="text-align:left;"> NRK </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0168 </td>
   <td style="text-align:right;"> -0.0168 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> POLR2E </td>
   <td style="text-align:left;"> POLR2E </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0230 </td>
   <td style="text-align:right;"> -0.0230 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> SBNO2 </td>
   <td style="text-align:left;"> SBNO2 </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0253 </td>
   <td style="text-align:right;"> -0.0253 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TRIM24 </td>
   <td style="text-align:left;"> TRIM24 </td>
   <td style="text-align:left;"> Down </td>
   <td style="text-align:right;"> -0.0255 </td>
   <td style="text-align:right;"> -0.0255 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Statistics - gene expression: end -->

<!-- Statistics - gene set enrichment: start -->
<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('gene-set-enrichment-stat')" class="btn btn-default action-button">GET</button></td>
    <td>/gs_enrichment</td>
    <td>Return  a collection of gene set enrichment that was exposed to a given drug</td>
  </tr>
</table>

<br>

<div id="gene-set-enrichment-stat-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/gs_enrichment?project=ADIPO&chemical_id=EtHex&geneset=Hallmark&gsva=gsva&summarize.func=median">https://montilab.bu.edu/Xposome-API/gs_enrichment?project=ADIPO&chemical_id=EtHex&geneset=Hallmark&gsva=gsva&summarize.func=median</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
  <tr>
    <td>chemical_id</td>
    <td>1,2-Dibromo-3-chloropropane or ...</td>
    <td>Name of the chemicals or CAS ids of a chemical in a specific project (see <span class="highlight-text">chemicals API</span>)</td>
    <td>string</td>
  </tr>
  <tr>
    <td>geneset</td>
    <td>Hallmark (default) or C2 or NURSA</td>
    <td>Collection of the gene set enrichment</td>
    <td>string</td>
  </tr>
  <tr>
    <td>gsva</td>
    <td>gsva (default)</td>
    <td>Method of the gene set enrichment analysis</td>
    <td>string</td>
  </tr>
  <tr>
    <td>summarize.func</td>
    <td>median (default) or mean/min/max/Q1/Q3</td>
    <td>Name of the summarize functions</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A data frame object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url9 <- paste0("https://montilab.bu.edu/Xposome-API/gs_enrichment?project=ADIPO&chemical_id=EtHex&geneset=Hallmark&gsva=gsva&summarize.func=median")

# Send GET Request to API
res <- GET(url = url9, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  gs_enrichment_stat <- fromJSON(fromJSON(rawToChar(res$content)))
  n_row <- ifelse(nrow(gs_enrichment_stat) > 10, 10, nrow(gs_enrichment_stat))
  n_col <- ifelse(ncol(gs_enrichment_stat) > 10, 10, ncol(gs_enrichment_stat))
  print(gs_enrichment_stat[1:n_row, 1:n_col])
}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Geneset </th>
   <th style="text-align:right;"> Summary Score </th>
   <th style="text-align:right;"> GS Score 10uM </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> HALLMARK_IL6_JAK_STAT3_SIGNALING </td>
   <td style="text-align:left;"> HALLMARK_IL6_JAK_STAT3_SIGNALING </td>
   <td style="text-align:right;"> 0.2350 </td>
   <td style="text-align:right;"> 0.2350 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_ESTROGEN_RESPONSE_EARLY </td>
   <td style="text-align:left;"> HALLMARK_ESTROGEN_RESPONSE_EARLY </td>
   <td style="text-align:right;"> 0.1892 </td>
   <td style="text-align:right;"> 0.1892 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_ESTROGEN_RESPONSE_LATE </td>
   <td style="text-align:left;"> HALLMARK_ESTROGEN_RESPONSE_LATE </td>
   <td style="text-align:right;"> 0.1542 </td>
   <td style="text-align:right;"> 0.1542 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_TGF_BETA_SIGNALING </td>
   <td style="text-align:left;"> HALLMARK_TGF_BETA_SIGNALING </td>
   <td style="text-align:right;"> 0.1358 </td>
   <td style="text-align:right;"> 0.1358 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_SPERMATOGENESIS </td>
   <td style="text-align:left;"> HALLMARK_SPERMATOGENESIS </td>
   <td style="text-align:right;"> 0.1251 </td>
   <td style="text-align:right;"> 0.1251 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_PI3K_AKT_MTOR_SIGNALING </td>
   <td style="text-align:left;"> HALLMARK_PI3K_AKT_MTOR_SIGNALING </td>
   <td style="text-align:right;"> 0.1249 </td>
   <td style="text-align:right;"> 0.1249 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_UV_RESPONSE_DN </td>
   <td style="text-align:left;"> HALLMARK_UV_RESPONSE_DN </td>
   <td style="text-align:right;"> 0.1240 </td>
   <td style="text-align:right;"> 0.1240 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_ADIPOGENESIS </td>
   <td style="text-align:left;"> HALLMARK_ADIPOGENESIS </td>
   <td style="text-align:right;"> 0.1209 </td>
   <td style="text-align:right;"> 0.1209 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_TNFA_SIGNALING_VIA_NFKB </td>
   <td style="text-align:left;"> HALLMARK_TNFA_SIGNALING_VIA_NFKB </td>
   <td style="text-align:right;"> 0.1135 </td>
   <td style="text-align:right;"> 0.1135 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HALLMARK_IL2_STAT5_SIGNALING </td>
   <td style="text-align:left;"> HALLMARK_IL2_STAT5_SIGNALING </td>
   <td style="text-align:right;"> 0.1008 </td>
   <td style="text-align:right;"> 0.1008 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Statistics - gene set enrichment: end -->

<!-- Statistics - connectivity: start -->
<table class="get-table">
  <tr>
    <td><button type="button" onclick="ToggleOperation('connectivity-stat')" class="btn btn-default action-button">GET</button></td>
    <td>/connectivity</td>
    <td>Return  a collection of gene connectivity that are linked to exposure of a known chemical</td>
  </tr>
</table>

<br>

<div id="connectivity-stat-block" style="display: none;">

<h4>Implementation</h4>

<p><a target="blank" href="https://montilab.bu.edu/Xposome-API/connectivity?project=ADIPO&chemical_id=EtHex&connectivity_classes=pcl&summarize.func=median">https://montilab.bu.edu/Xposome-API/connectivity?project=ADIPO&chemical_id=EtHex&connectivity_classes=pcl&summarize.func=median</a></p>

<br>

<table class="api-table">
  <tr>
    <td><strong>Parameter</strong></td>
    <td><strong>Value</strong></td>
    <td><strong>Description</strong></td>
    <td><strong>Data Type</strong></td>
  </tr>
  <tr>
    <td>project</td>
    <td>ADIPO or HEPG2 or ...</td>
    <td>Name of the projects (see <span class="highlight-text">projects API</span>)</td>
    <td>string</td>
  </tr>
  <tr>
    <td>chemical_id</td>
    <td>1,2-Dibromo-3-chloropropane or ...</td>
    <td>Name of the chemicals or CAS ids of a chemical in a specific project (see <span class="highlight-text">chemicals API</span>)</td>
    <td>string</td>
  </tr>
  <tr>
    <td>connectivity_classes</td>
    <td>pcl (Perturbagen Classes, default) or pert (Perturbagens)</td>
    <td>Name of the connectivity classes</td>
    <td>string</td>
  </tr>
  <tr>
    <td>summarize.func</td>
    <td>median (default) or mean/min/max/Q1/Q3</td>
    <td>Name of the summarize functions</td>
    <td>string</td>
  </tr>
</table>

<br>

<h4>Return</h4>

<p>A data frame object</p>

<br>

<h4>Example in R</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# url for local testing
url10 <- paste0("https://montilab.bu.edu/Xposome-API/connectivity?project=ADIPO&chemical_id=EtHex&connectivity_classes=pcl&summarize.func=median")

# Send GET Request to API
res <- GET(url = url10, encode = "json")

# Check the status of GET request
test_request <- tryCatch({

  stop_for_status(res)

  "pass"

}, error = function(e) {

  "fail"

})
</pre>


<h4>Output</h4>

<div class="knitr-options" data-fig-width="576" data-fig-height="460"></div>

<pre>
# If GET request is successful, return the results
if(test_request == "pass"){

  connectivity_stat <- fromJSON(fromJSON(rawToChar(res$content)))
  n_row <- ifelse(nrow(connectivity_stat) > 10, 10, nrow(connectivity_stat))
  n_col <- ifelse(ncol(connectivity_stat) > 10, 10, ncol(connectivity_stat))
  print(connectivity_stat[1:n_row, 1:n_col])

}
</pre>
<pre class='output'> <table>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Connectivity_Id </th>
   <th style="text-align:right;"> Pcl_size </th>
   <th style="text-align:right;"> Summary Score </th>
   <th style="text-align:right;"> Connectivity Score 10uM </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> CP_HSP_INHIBITOR </td>
   <td style="text-align:left;"> CP_HSP_INHIBITOR </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 97.5249 </td>
   <td style="text-align:right;"> 97.5249 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_HDAC_INHIBITOR </td>
   <td style="text-align:left;"> CP_HDAC_INHIBITOR </td>
   <td style="text-align:right;"> 20 </td>
   <td style="text-align:right;"> 83.8594 </td>
   <td style="text-align:right;"> 83.8594 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KD_POLY_ADP_RIBOSE_POLYMERASES </td>
   <td style="text-align:left;"> KD_POLY_ADP_RIBOSE_POLYMERASES </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 72.3805 </td>
   <td style="text-align:right;"> 72.3805 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_FGFR_INHIBITOR </td>
   <td style="text-align:left;"> CP_FGFR_INHIBITOR </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 70.7205 </td>
   <td style="text-align:right;"> 70.7205 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KD_APOLIPOPROTEINS </td>
   <td style="text-align:left;"> KD_APOLIPOPROTEINS </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 56.3082 </td>
   <td style="text-align:right;"> 56.3082 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KD_WNT_FAMILY </td>
   <td style="text-align:left;"> KD_WNT_FAMILY </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 55.2858 </td>
   <td style="text-align:right;"> 55.2858 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CP_PROTEASOME_INHIBITOR </td>
   <td style="text-align:left;"> CP_PROTEASOME_INHIBITOR </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 52.5642 </td>
   <td style="text-align:right;"> 52.5642 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KD_UBIQUITIN_SPECIFIC_PEPTIDASES </td>
   <td style="text-align:left;"> KD_UBIQUITIN_SPECIFIC_PEPTIDASES </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 49.4315 </td>
   <td style="text-align:right;"> 49.4315 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KD_PROTEASOME_PATHWAY </td>
   <td style="text-align:left;"> KD_PROTEASOME_PATHWAY </td>
   <td style="text-align:right;"> 9 </td>
   <td style="text-align:right;"> 47.2839 </td>
   <td style="text-align:right;"> 47.2839 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> KD_S100_CALCIUM_BINDING_PROTEINS </td>
   <td style="text-align:left;"> KD_S100_CALCIUM_BINDING_PROTEINS </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 46.9016 </td>
   <td style="text-align:right;"> 46.9016 </td>
  </tr>
</tbody>
</table> </pre>

<br>

</div>
<!-- Statistics - connectivity: end -->

<p>Additionally, we provide direct links to query contents on our website. See <a href="#links-to-application">Links to Application</a>.

<br><br>
