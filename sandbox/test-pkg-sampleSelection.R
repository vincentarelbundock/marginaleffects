pkgload::load_all()

requiet("insight")
requiet("sampleSelection")

data(Mroz87, package = "sampleSelection")
Mroz87$kids  <- (Mroz87$kids5 + Mroz87$kids618 > 0)

# weird vcov with NA values
# Two-step estimation
model <- heckit(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                wage ~ exper + I( exper^2 ) + educ + city, 
                data = Mroz87)
marginaleffects(model)

# ML estimation
model <- selection(lfp ~ age + I( age^2 ) + faminc + kids + educ,
                   wage ~ exper + I( exper^2 ) + educ + city, 
                   data = Mroz87)
marginaleffects(model)
