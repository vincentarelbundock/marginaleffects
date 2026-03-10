
Similar to match.call() but works better in lapply, etc.
May be slow due to multiple calls to eval().
importFrom marginaleffects predictions slopes comparisons

### Description

Similar to match.call() but works better in lapply, etc.
May be slow due to multiple calls to eval().
importFrom marginaleffects predictions slopes comparisons



### Usage

<pre><code class='language-R'>construct_call(model, calling_function, env = parent.frame(1L))
</code></pre>

