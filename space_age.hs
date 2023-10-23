data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury age = (age/31557600) * (1/0.2408467)
ageOn Venus age = (age/31557600) * (1/0.61519726)
ageOn Earth age = (age/31557600) * (1/1)
ageOn Mars age = (age/31557600) * (1/1.8808158)
ageOn Jupiter age = (age/31557600) * (1/11.862515)
ageOn Saturn age = (age/31557600) * (1/29.447498)
ageOn Uranus age = (age/31557600) * (1/84.016846)
ageOn Neptune age = (age/31557600) * (1/164.79132)
