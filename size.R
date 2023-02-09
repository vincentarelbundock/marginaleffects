library(marginaleffects)
mod <- lm(mpg ~ hp, mtcars)

pkgload::load_all()
cmp <- comparisons(mod)
memsize(cmp)

memsize = function(x) {
    ma = sapply(x, \(k) object.size(serialize(k, NULL)))
    at = sapply(attributes(x), \(k) object.size(serialize(k, NULL)))
    list("object" = ma, "attributes" = at)
}
