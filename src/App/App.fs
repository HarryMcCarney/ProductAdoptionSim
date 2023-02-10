namespace AdoptionSim

module Main =
    open Sutil
    open Sutil.CoreElements
    open AdoptionSim.Population
    open AdoptionSim.View
    open AdoptionSim.State
    open Sutil.DomHelpers


    let grids =
        let divs =
            Html.div
                [ Attr.style "display: inline-block"
                  unsubscribeOnUnmount [ interval updateStore 500 ]
                  Bind.el (peopleStore, (fun ps -> generateGrid (getState "rowCount") (getState "colCount") ps)) ]

        Html.div divs


    Html.div
        [ Attr.style [ Css.marginLeft 5; Css.marginRight 5 ]
          Html.div [ form ]
          grids
          chart ]
    |> Program.mountElement "adoption-sim"
