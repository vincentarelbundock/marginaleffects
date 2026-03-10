## Create a data.frame with all factor or character levels {.unnumbered}


### Description

<code>model.matrix</code> breaks when <code>newdata</code> includes a factor
variable, but not all levels are present in the data. This is bad for us
because we often want to get predictions with one (or few) rows, where some
factor levels are inevitably missing.



### Usage

<pre><code class='language-R'>complete_levels(x, character_levels = NULL)
</code></pre>

