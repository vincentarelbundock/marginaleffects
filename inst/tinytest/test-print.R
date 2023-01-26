
pkgload::load_all()
mod <- lm(mpg ~ hp, mtcars)
pre <- predictions(mod)

pre

predictions(mod, by = "cyl")
