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
              "adoptorStartCount", 0
              "running", 0
              "ticks", 0 ]

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

    type People = array<Person>

    let initalisePopulation noPeople adoptorStartCount columns rows : People =
        [| 1..noPeople |]
        |> Array.map (fun x ->
            { Coordinates = rnd.Next(1, rows + 1), rnd.Next(1, columns + 1)
              Status = Prospect })

    let peopleStore =
        let peopleCount = getState "peopleCount"
        let adoptorStartCount = getState "adoptorStartCount"
        let colCount = getState "colCount"
        let rowCount = getState "rowCount"
        Store.make (initalisePopulation peopleCount adoptorStartCount colCount rowCount)

    let fetchPeople (people: People) r c =
        people
        |> Array.filter (fun x -> (fst x.Coordinates) = r && (snd x.Coordinates) = c)

    let adoptorThreholdMet p t =
        if (p |> Seq.filter (fun x -> x.Status = Adoptor) |> Seq.length) >= t then
            true
        else
            false

    let convertAdoptors (people: People) : People =
        let threshold = getState "peerPreasureThreshold"

        people
        |> Array.groupBy (fun x -> x.Coordinates)
        |> Array.map (fun (co, p) ->
            if (adoptorThreholdMet p threshold) then
                p |> Array.map (fun x -> { Coordinates = co; Status = Adoptor })
            else
                p)
        |> Array.concat

    let movePopulation rows columns (people: People) : People =
        people
        |> Array.map (fun x ->
            { Coordinates = rnd.Next(1, rows + 1), rnd.Next(1, columns + 1)
              Status = x.Status })

    let convertToTrackingObject (people: People) : array<PopulationTracker.Person> =
        people
        |> Array.map (fun x ->
            { Status =
                (if x.Status = Prospect then
                     PopulationTracker.Prospect
                 else
                     PopulationTracker.Adoptor)
              Coordinates = x.Coordinates })

    let updatePopulation rows columns (people: People) =
        let tick = getState "ticks"
        people |> convertToTrackingObject |> logPeople tick |> ignore

        people |> convertAdoptors |> movePopulation rows columns

    let stopIfComplete (people: People) state =
        if (people |> Array.exists (fun p -> p.Status = Prospect)) then
            ()
        else
            stopSimulation state
            PopulationTracker.drawAdopterChart ()

    let updateStore () =
        Store.set
            peopleStore
            (stopIfComplete (Store.get peopleStore) stateStore

             if ((Store.get stateStore)["running"] = 1) then
                 let t = (Store.get stateStore)["ticks"] + 1
                 stateStore <~= (fun m -> m.Change("ticks", (fun _ -> Some t)))
                 updatePopulation (getState "rowCount") (getState "colCount") (Store.get peopleStore)
             else
                 (Store.get peopleStore))

    let setMarketingSpend (state: IStore<Map<string, int>>) adoptors =
        state <~= (fun m -> m.Change("adoptorStartCount", (fun _ -> Some adoptors)))

        peopleStore
        <~= (fun ps ->
            ps
            |> Array.mapi (fun i p ->
                if i < adoptors then
                    { Coordinates = p.Coordinates
                      Status = Adoptor }
                else
                    p))

    let resetSimulation (state: IStore<Map<string, int>>) =
        stopSimulation state
        state <~= (fun m -> State.Init())
        records.Clear()
        Store.set chartStore (PopulationTracker.Init())
        let peopleCount = getState "peopleCount"
        let adoptorStartCount = getState "adoptorStartCount"
        let colCount = getState "colCount"
        let rowCount = getState "rowCount"
        Store.set peopleStore (initalisePopulation peopleCount adoptorStartCount colCount rowCount)

    let setPopulationSize i =
        stopSimulation stateStore
        let peopleCount = i
        let adoptorStartCount = getState "adoptorStartCount"
        let colCount = getState "colCount"
        let rowCount = getState "rowCount"
        Store.set peopleStore (initalisePopulation peopleCount adoptorStartCount colCount rowCount)

    let setPeerPreasureThreshold (state: IStore<Map<string, int>>) threshold =
        stopSimulation stateStore

        state
        <~= (fun m -> m.Change("peerPreasureThreshold", (fun _ -> Some threshold)))
