namespace AdoptionSim

module State =

    open System
    open Sutil

    let rnd = new Random()

    let Init () =
        Map
            [ "colCount", 5
              "rowCount", 5
              "peopleCount", 100
              "adoptorStartCount", 7
              "peerPreasureThreshold", 3
              "running", 0
              "ticks", 0
              "numberSimulations", 12 ]

    let stateStore = Store.make (Init())

    let getState k = (Store.get stateStore)[k]

    let startSimulation (state: IStore<Map<string, int>>) =
        state <~= (fun m -> m.Change("running", (fun _ -> Some 1)))

    let stopSimulation (state: IStore<Map<string, int>>) =
        state <~= (fun m -> m.Change("running", (fun _ -> Some 0)))


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

        let adoptors = getState "adoptorStartCount"

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
        let peopleCount = getState "peopleCount"
        let adoptorStartCount = getState "adoptorStartCount"
        let colCount = getState "colCount"
        let rowCount = getState "rowCount"

        Store.make (
            initalisePopulation [||] (getState "numberSimulations") peopleCount adoptorStartCount colCount rowCount
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
        let threshold = getState "peerPreasureThreshold"

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
        let tick = getState "ticks"

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
        Store.set
            peopleStore
            (stopIfComplete (Store.get peopleStore) stateStore

             if ((Store.get stateStore)["running"] = 1) then
                 let t = (Store.get stateStore)["ticks"] + 1
                 stateStore <~= (fun m -> m.Change("ticks", (fun _ -> Some t)))
                 updatePopulations (getState "rowCount") (getState "colCount") (Store.get peopleStore)
             else
                 (Store.get peopleStore))

    let setMarketingSpend (state: IStore<Map<string, int>>) adoptors =
        state <~= (fun m -> m.Change("adoptorStartCount", (fun _ -> Some adoptors)))

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


    let resetSimulation (state: IStore<Map<string, int>>) =
        stopSimulation state
        state <~= (fun m -> State.Init())
        records.Clear()
        Store.set chartStore (PopulationTracker.Init())
        let peopleCount = getState "peopleCount"
        let adoptorStartCount = getState "adoptorStartCount"
        let colCount = getState "colCount"
        let rowCount = getState "rowCount"

        Store.set
            peopleStore
            (initalisePopulation [||] (getState "numberSimulations") peopleCount adoptorStartCount colCount rowCount)

    let setPopulationSize i =
        stopSimulation stateStore
        let peopleCount = i
        let adoptorStartCount = getState "adoptorStartCount"
        let colCount = getState "colCount"
        let rowCount = getState "rowCount"

        Store.set
            peopleStore
            (initalisePopulation [||] (getState "numberSimulations") peopleCount adoptorStartCount colCount rowCount)

    let setPeerPreasureThreshold (state: IStore<Map<string, int>>) threshold =
        stopSimulation stateStore

        state
        <~= (fun m -> m.Change("peerPreasureThreshold", (fun _ -> Some threshold)))


    let setNumberSimulations (state: IStore<Map<string, int>>) i =
        stopSimulation stateStore

        state <~= (fun m -> m.Change("numberSimulations", (fun _ -> Some i)))
