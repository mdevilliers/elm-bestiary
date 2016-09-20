import Html


-- MAIN
main : Html.Html a
main =
    view model

-- MODEL
model : String
model =
    "World"

-- VIEW
view : String ->  Html.Html a
view person =
    Html.text ("Hello " ++ person)
