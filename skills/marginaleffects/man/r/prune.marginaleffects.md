
## Prune marginaleffects objects to reduce memory usage {.unnumbered}


### Description

Remove large attributes from marginaleffects objects to reduce memory usage.
Warning: This will disable many useful post-processing features of <code>marginaleffects</code>



### Usage

<pre><code class='language-R'>## S3 method for class 'marginaleffects'
prune(tree, component = "all", ...)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="tree">tree</code></td>
<td>
A marginaleffects object (predictions, comparisons, slopes, or hypotheses)
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="component">component</code></td>
<td>
A character string indicating which component to prune: &quot;all&quot; or &quot;modeldata&quot;.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>
Unused
</td></tr>
</table>


### Details

...



### Value

A pruned marginaleffects object


