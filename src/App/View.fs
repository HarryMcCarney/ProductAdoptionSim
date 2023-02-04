namespace AdoptionSim

module View =
    open Sutil
    open Sutil.Styling
    open Sutil.CoreElements
    open type Feliz.length
    open Population
    open State
    open PopulationTracker
    
    let tableStyle  = [
        rule "table, th, td" [
            Css.border (px 1, Feliz.borderStyle.dashed, "grey")
            Css.tableLayoutFixed'
            ]
        rule "table" [
                Css.width (percent 100)
            ]
        rule "td" [
            Css.height (em 5)
        ]
    ]

    let adoptor  = 
        Html.div[Attr.style "   height: 10px;
                                width: 10px;
                                background-color: green;
                                border-radius: 50%;
                                display: inline-block;"]

    let prospect = 
        Html.div[Attr.style "   height: 10px;
                                width: 10px;
                                background-color: red;
                                border-radius: 50%;
                                display: inline-block;"]




    let form = 
        Html.div [
                Html.div [
                    class' "block"
                    Html.label [
                        Html.input [
                            type' "number"
                            Bind.attr ("value",(fun (i) -> setPopulationSize i))
                            Attr.min 0
                            Attr.max 100
                        ]
                        text "Population size"
                    ]
                ]

                Html.div [
                    class' "block"
                    Html.label [
                        Html.input [
                            type' "number"
                            Bind.attr ("value",(fun (i) -> setMarketingSpend stateStore i))
                            Attr.min 0
                            Attr.max 100
                        ]
                        text "Marketing spend"
                    ]
                ]

                Bind.el( stateStore |> Store.mapDistinct (fun sw -> sw["running"]), fun isRunning ->
                    if (isRunning =1) then
                        Html.button [
                            Attr.className "button" 
                            Attr.style [ Css.marginTop (rem 1) ]
                            text "Stop"
                            Ev.onClick (fun _ -> stopSimulation stateStore)
                        ]
                    else
                        Html.button [
                            Attr.className "button" 
                            Attr.style [ Css.marginTop (rem 1) ]
                            text "Start"
                            Ev.onClick (fun _ -> startSimulation stateStore)
                        ]
                )
                Html.button [
                        Attr.className "button" 
                        Attr.style [ Css.marginTop (rem 1) ]
                        text "Reset"
                        Ev.onClick (fun _ -> resetSimulation stateStore)
                    ]
                Html.div [
                    Bind.el( stateStore, fun t -> string t["ticks"] |> text)
                ]

                Html.div [
                    Bind.el( peopleStore, fun ps -> 
                                                        let adopters = ps 
                                                                        |> Array.filter(fun p -> p.Status = Population.Adoptor) 
                                                                        |> Array.length

                                                        let prospects = ps 
                                                                        |> Array.filter(fun p -> p.Status = Population.Prospect) 
                                                                        |> Array.length
                                                        sprintf  "Prospects: %i / Adoptors: %i" prospects adopters 
                                                        |> text)
                                                    
                ]
                
                Html.div [
                    Bind.el( chartStore, fun t ->  
                                                    host (fun re -> Feliz.ReactDOM.render ((t[0]), re) )
                                                   )  
                ]   

            ]

    let createRow rowNumber columns people = 
        let cols = [1..columns]
                    |> Seq.map(fun c -> fetchPeople people rowNumber c
                                        |> Seq.map(fun x -> if x.Status = Population.Prospect then prospect else adoptor)
                                        |> Html.td
                    )
        Html.tableRow cols

    let generateGrid totalRows totalColumns people = 
        [1..totalRows]
        |> Seq.map(fun r -> createRow r totalColumns people)
        |> Html.table
        |> withStyle tableStyle
