## Download and Read Datasets from <code>marginaleffects</code> or Rdatasets {.unnumbered}


### Description

Downloads a dataset from the <code>marginaleffects</code> or the Rdatasets archives, and return it as a data frame. Opens the documentation as an HTML page. Search available datasets.

<a href="https://vincentarelbundock.github.io/Rdatasets/">https://vincentarelbundock.github.io/Rdatasets/</a>



### Usage

<pre><code class='language-R'>get_dataset(dataset = "thornton", package = NULL, docs = FALSE, search = NULL)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="dataset">dataset</code></td>
<td>
String. Name of the dataset to download.


<ul>
<li> <code>marginaleffects</code> archive: <code>affairs</code>, <code>airbnb</code>, <code>ces_demographics</code>, <code>ces_survey</code>, <code>immigration</code>, <code>lottery</code>, <code>military</code>, <code>thornton</code>, <code>factorial_01</code>, <code>interaction_01</code>, <code>interaction_02</code>, <code>interaction_03</code>, <code>interaction_04</code>, <code>polynomial_01</code>, <code>polynomial_02</code>

</li>
<li> Rdatasets archive: The name of a dataset listed on the Rdatasets index. See the website or the <code>search</code> argument.

</li></ul>
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="package">package</code></td>
<td>
String. Package name that originally published the data.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="docs">docs</code></td>
<td>
Logical. If TRUE open the documentation using <code>getOption("viewer")</code> or the Rstudio viewer.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="search">search</code></td>
<td>
Regular expression. Download the dataset index from Rdatasets; search the &quot;Package&quot;, &quot;Item&quot;, and &quot;Title&quot; columns; and return the matching rows.
</td></tr>
</table>


### Value

A data frame containing the dataset.



### Examples
```{r, warning=FALSE, message=FALSE, eval=FALSE}
library("marginaleffects")


dat <- get_dataset("Titanic", "Stat2Data")
head(dat)

get_dataset(search = "(?i)titanic")

# View documentation in the browser
get_dataset("Titanic", "Stat2Data", docs = TRUE)



```
