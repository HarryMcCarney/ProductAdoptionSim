namespace AdoptionSim

module Main =
    open Sutil
    open Sutil.CoreElements
    open AdoptionSim.Population
    open AdoptionSim.View
    open AdoptionSim.State
    open Sutil.DomHelpers

    Html.div
        [ Attr.style [ Css.marginLeft 5; Css.marginRight 5 ]
          Html.div
              [ unsubscribeOnUnmount [ interval updateStore 500 ]
                Bind.el (peopleStore, (fun ps -> generateGrid (getState "rowCount") (getState "colCount") ps)) ]
          Html.div [ form ] ]
    |> Program.mountElement "adoption-sim"
