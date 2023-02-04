namespace AdoptionSim

module PopulationTracker =
    open Feliz.Plotly

    open Sutil

    type Status = Prospect | Adoptor

    type Person = {
        Coordinates: int*int
        Status :  Status
    }

    type Record = {
        Tick : int
        Adoptors: int
        Prospects : int
    }  

    let Init() : array<Feliz.ReactElement> = 
        [||]

    let chartStore = Store.make (Init())

    let mutable records = ResizeArray<Record>()

    let logPeople tick (people: array<Person>) = 
        let adoptors = people |> Array.filter(fun x -> x.Status = Adoptor) |> Array.length
        let prospects = people |> Array.filter(fun x -> x.Status = Prospect) |> Array.length
        records.Add{Tick = tick; Adoptors = adoptors; Prospects = prospects}

    let drawAdopterChart() =
        [|
        Plotly.plot [
            plot.traces [
                traces.scatter [
                    scatter.x (records |> Seq.sortBy(fun x -> x.Tick) |> Seq.map(fun x -> x.Tick))
                    scatter.y (records |> Seq.sortBy(fun x -> x.Tick) |> Seq.map(fun x -> x.Adoptors))
                    scatter.mode.lines
                    scatter.name "Lines"
                    ]
                ]
            plot.layout [
                layout.xaxis [
                    xaxis.title "Time interval"
                ]
                layout.yaxis [
                    yaxis.title "Adoptors"
                ]
                layout.title [
                    title.text "Adoption over time"
                ]
            ]
        ]
            |]
        |> Store.set chartStore 
        
        
