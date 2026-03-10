
## Extract components from marginaleffects objects {.unnumbered}


### Description

Extract components from marginaleffects objects



### Usage

<pre><code class='language-R'>## S3 method for class 'marginaleffects'
components(object, component = NULL, ...)
</code></pre>


### Arguments

<table role = "presentation">
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="object">object</code></td>
<td>
A marginaleffects object (predictions, comparisons, slopes, or hypotheses)
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="component">component</code></td>
<td>
Character string specifying which component to extract. Must be a valid
slot name from the internal S4 object. If <code>NULL</code> (the default), <code>components()</code> prints a message with
all available component names. Common components include: &quot;model&quot;, &quot;newdata&quot;,
&quot;modeldata&quot;, &quot;call&quot;, &quot;jacobian&quot;, &quot;vcov_model&quot;, &quot;type&quot;, &quot;by&quot;, &quot;comparison&quot;, &quot;variables&quot;, etc.
</td></tr>
<tr><td style = "white-space: nowrap; font-family: monospace; vertical-align: top"><code id="...">...</code></td>
<td>
Ignored.
</td></tr>
</table>


### Details

This function provides access to the internal components stored in the <code>mfx</code>
attribute of marginaleffects objects. The <code>mfx</code> attribute contains an S4 object of
class &quot;marginaleffects_internal&quot; with various slots containing model information,
data, and computational details used by the marginaleffects functions.

Warning: the internal slot names are not considered part of the public API and may change
without warning in future versions of the marginaleffects package.



### Value

The requested component from the mfx object


