namespace AdoptionSim

module PopulationTracker =
    open Feliz.Plotly

    open Sutil

    type Status =
        | Prospect
        | Adoptor

    type Person =
        { Coordinates: int * int
          Status: Status }

    type Record =
        { GridId: int
          Tick: int
          Adoptors: int
          Prospects: int }

    type Grid = int * array<Person>

    let Init () : array<Feliz.ReactElement> = [||]

    let chartStore = Store.make (Init())

    let mutable records = ResizeArray<Record>()

    let logPeople tick (grids: array<Grid>) =

        grids
        |> Array.map (fun g ->
            let adoptors = (snd g) |> Array.filter (fun x -> x.Status = Adoptor) |> Array.length

            let prospects =
                (snd g) |> Array.filter (fun x -> x.Status = Prospect) |> Array.length

            records.Add
                { GridId = (fst g)
                  Tick = tick
                  Adoptors = adoptors
                  Prospects = prospects })

    let drawAdopterCharts () =
        let grids = records |> Seq.map (fun r -> r.GridId) |> Seq.distinct |> Seq.length

        [| 1..grids |]
        |> Array.map (fun i ->
            let rs = records |> Seq.filter (fun x -> x.GridId = i)

            [| Plotly.plot
                   [ plot.traces
                         [ traces.scatter
                               [ scatter.x (rs |> Seq.sortBy (fun x -> x.Tick) |> Seq.map (fun x -> x.Tick))
                                 scatter.y (rs |> Seq.sortBy (fun x -> x.Tick) |> Seq.map (fun x -> x.Adoptors))
                                 scatter.mode.lines
                                 scatter.name "Lines" ] ]
                     plot.layout
                         [ layout.xaxis [ xaxis.title "Time interval" ]
                           layout.yaxis [ yaxis.title "Adoptors" ]
                           layout.title [ title.text "Adoption over time" ] ] ] |])
        |> Array.concat


    let rng = System.Random()

    let drawAdopterHist () =
        let counts =
            records
            |> Seq.toArray
            |> Array.groupBy (fun x -> x.GridId)
            |> Array.map (fun (g, rs) -> rs |> Array.filter (fun r -> r.Prospects = 0))
            |> Array.concat
            |> Array.groupBy (fun x -> x.GridId)
            |> Array.map (fun (g, rs) -> (rs |> Array.minBy (fun y -> y.Tick)))
            |> Array.map (fun r -> r.Tick)


        [| Plotly.plot [ plot.traces [ traces.histogram [ histogram.x counts; histogram.nbinsx 5 ] ] ] |]

    let drawCharts () =
        drawAdopterCharts ()
        |> Array.append (drawAdopterHist ())
        |> Store.set chartStore

        drawAdopterHist () |> Store.set chartStore
