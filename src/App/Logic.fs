namespace AdoptionSim

module State =

    open System
    open Sutil

    type Model =
        { ColCount: int
          RowCount: int
          PeopleCount: int
          AdoptorStartCount: int
          PeerPreasureThreshold: int
          Running: bool
          Ticks: int
          NumberSimulations: int
          Id: Guid }

    let rnd = new Random()

    let Init () =
        printfn "inti called "

        { ColCount = 5
          RowCount = 5
          PeopleCount = 100
          AdoptorStartCount = 7
          PeerPreasureThreshold = 3
          Running = false
          Ticks = 0
          NumberSimulations = 12
          Id = Guid.NewGuid() }

    let stateStore = Store.make (Init())

    let getState = Store.get stateStore

    let startSimulation (state: IStore<Model>) =
        printfn "before setting %A" state
        state <~= (fun m -> { m with Running = true })
        printfn "after setting %A" state

    let stopSimulation (state: IStore<Model>) =
        state <~= (fun m -> { m with Running = false })


module Population =

    open Sutil
    open State
    open AdoptionSim.PopulationTracker

    type Status =
        | Prospect
        | Adoptor

    type Person =
        { Coordinates: int * int
          Status: Status }

    type GridId = GridId of int

    type Grid = GridId * array<Person>

    let gId (GridId gridId) = int gridId


    let rec initalisePopulation (grids: array<Grid>) (reqGrids: int) noPeople adoptorStartCount columns rows : Grid[] =
        let gridId = ((grids |> Array.length) + 1) |> GridId

        let adoptors = getState.AdoptorStartCount

        if gId gridId <= reqGrids then
            let results =
                [| 1..noPeople |]
                |> Array.map (fun x ->
                    { Coordinates = rnd.Next(1, rows + 1), rnd.Next(1, columns + 1)
                      Status = (if x < adoptors then Adoptor else Prospect) })
                |> fun p -> gridId, p
                |> fun a -> grids |> Array.append [| a |]

            initalisePopulation results reqGrids noPeople adoptorStartCount columns rows
        else
            grids

    let peopleStore =
        let peopleCount = getState.PeopleCount
        let adoptorStartCount = getState.AdoptorStartCount
        let colCount = getState.ColCount
        let rowCount = getState.RowCount

        Store.make (
            initalisePopulation [||] (getState.NumberSimulations) peopleCount adoptorStartCount colCount rowCount
        )

    let fetchPeople (people: array<Person>) r c =
        people
        |> Array.filter (fun x -> (fst x.Coordinates) = r && (snd x.Coordinates) = c)

    let adoptorThreholdMet p t =
        if (p |> Seq.filter (fun x -> x.Status = Adoptor) |> Seq.length) >= t then
            true
        else
            false

    let convertAdoptors (people: array<Person>) : array<Person> =
        let threshold = getState.PeerPreasureThreshold

        people
        |> Array.groupBy (fun x -> x.Coordinates)
        |> Array.map (fun (co, p) ->
            if (adoptorThreholdMet p threshold) then
                p |> Array.map (fun x -> { Coordinates = co; Status = Adoptor })
            else
                p)
        |> Array.concat

    let movePopulation rows columns (people: array<Person>) : array<Person> =
        people
        |> Array.map (fun x ->
            { Coordinates = rnd.Next(1, rows + 1), rnd.Next(1, columns + 1)
              Status = x.Status })

    let convertToTrackingObject (grids: array<Grid>) : array<PopulationTracker.Grid> =
        grids
        |> Array.map (fun (g, p) ->
            gId g,
            p
            |> Array.map (fun x ->
                { Status =
                    (if x.Status = Prospect then
                         PopulationTracker.Prospect
                     else
                         PopulationTracker.Adoptor)
                  Coordinates = x.Coordinates }))

    let updatePopulations rows columns (grids: array<Grid>) =
        let tick = getState.Ticks

        grids |> convertToTrackingObject |> logPeople tick |> ignore

        grids
        |> Array.map (fun g -> (fst g), (snd g) |> convertAdoptors |> movePopulation rows columns)

    let stopIfComplete (grids: array<Grid>) state =
        if
            (grids
             |> Array.exists (fun (g, p) -> (p |> Array.exists (fun x -> x.Status = Prospect) = true)))
        then
            ()
        else
            stopSimulation state
            drawCharts ()

    let updateStore () =
        printfn "start of update %A" getState

        Store.set
            peopleStore
            (stopIfComplete (Store.get peopleStore) stateStore

             if (getState.Running = true) then
                 let t = getState.Ticks + 1
                 printfn "ticks: %i" t
                 stateStore <~= (fun m -> { m with Ticks = t })
                 updatePopulations (getState.RowCount) (getState.ColCount) (Store.get peopleStore)
             else
                 (Store.get peopleStore))

    let setMarketingSpend (state: IStore<Model>) adoptors =
        state <~= (fun m -> { m with AdoptorStartCount = adoptors })

        peopleStore
        <~= (fun ps ->
            ps
            |> Array.map (fun (g, ps) ->
                g,
                ps
                |> Array.mapi (fun i p ->
                    if i < adoptors then
                        { Coordinates = p.Coordinates
                          Status = Adoptor }
                    else
                        p)))


    let resetSimulation (state: IStore<Model>) =
        stopSimulation state
        state <~= (fun m -> State.Init())
        records.Clear()
        Store.set chartStore (PopulationTracker.Init())
        let peopleCount = getState.PeopleCount
        let adoptorStartCount = getState.AdoptorStartCount
        let colCount = getState.ColCount
        let rowCount = getState.RowCount

        Store.set
            peopleStore
            (initalisePopulation [||] (getState.NumberSimulations) peopleCount adoptorStartCount colCount rowCount)

    let setPopulationSize i =
        stopSimulation stateStore
        let peopleCount = i
        let adoptorStartCount = getState.AdoptorStartCount
        let colCount = getState.ColCount
        let rowCount = getState.RowCount

        Store.set
            peopleStore
            (initalisePopulation [||] (getState.NumberSimulations) peopleCount adoptorStartCount colCount rowCount)

    let setPeerPreasureThreshold (state: IStore<Model>) threshold =
        stopSimulation state

        state
        <~= (fun m ->
            { m with
                PeerPreasureThreshold = threshold })


    let setNumberSimulations (state: IStore<Model>) i =
        stopSimulation state

        state <~= (fun m -> { m with NumberSimulations = i })
