namespace JsonDemo

open System
open System.Collections.Generic
open WebSharper
open WebSharper.Sitelets

/// This module implements the tutorial REST API from http://websharper.com/tutorials/rest-api
/// It's a full CRUD application maintaining a basic in-memory database of people.
module RestApi =

    /// The type of actions, ie. REST API entry points.
    type Action =
        /// GET /person?id=123
        | [<EndPoint "GET /person">]
            GetPerson of id: int
        /// POST /person (with JSON body)
        | [<EndPoint "POST /person"; Json "personData">]
            PostPerson of personData: PersonData
        /// PUT /person?id=123 (with JSON body)
        | [<EndPoint "PUT /person"; Json "personData">]
            PutPerson of id: int * personData: PersonData
        /// DELETE /person?id=123
        | [<EndPoint "DELETE /person">]
            DeletePerson of id: int
        | [<EndPoint "GET /people">]
            GetPeople

    /// Data about a person. Used both for storage and JSON parsing/writing.
    and PersonData =
        { firstName: string
          lastName: string }

    /// Type used for all JSON responses to indicate success or failure.
    [<NamedUnionCases "result">]
    type Result<'T> =
        /// JSON: {"result": "success", /* fields of 'T... */}
        | [<Name "success">] Success of 'T
        /// JSON: {"result": "failure", "message": "error message..."}
        | [<Name "failure">] Failure of message: string

    /// Result value for PostPerson.
    type Id = { id : int }

    type Error = { error: string }

    module private ApplicationLogic =
        open System.Threading

        /// Lock for accessing the shared dictionary.
        let lck = ReaderWriterLock()
        let timeout = 3000
        /// The people database.
        let people = new Dictionary<int, PersonData>()
        /// The highest id used so far, incremented each time a person is POSTed.
        let lastId = ref 0

        let private withReadLock f =
            lck.AcquireReaderLock timeout
            try f ()
            finally lck.ReleaseReaderLock ()

        let private withWriteLock f =
            lck.AcquireWriterLock timeout
            try f ()
            finally lck.ReleaseWriterLock ()

        let getPerson (id: int) : Result<PersonData> =
            withReadLock <| fun () ->
                match people.TryGetValue(id) with
                | true, person -> Success person
                | false, _ -> Failure "Person not found."

        let postPerson (data: PersonData) : Result<Id> =
            withWriteLock <| fun () ->
                incr lastId
                people.[!lastId] <- data
                Success { id = !lastId }

        let putPerson (id: int) (data: PersonData) : Result<unit> =
            withWriteLock <| fun () ->
                match people.TryGetValue(id) with
                | true, _ ->
                    people.[id] <- data
                    Success ()
                | false, _ -> Failure "Person not found."

        let deletePerson (id: int) : Result<unit> =
            withWriteLock <| fun () ->
                match people.TryGetValue(id) with
                | true, _ ->
                    people.Remove(id) |> ignore
                    Success ()
                | false, _ -> Failure "Person not found."

        let getPeople () : Result<(Id * PersonData) []> =
            withReadLock <| fun () ->
                people
                |> Seq.map (fun e -> ({ id = e.Key }, e.Value))
                |> Seq.toArray
                |> Success

    let MakeContent (result: Result<'T>) =
        match result with
        | Success x ->
            Content.Json x
        | Failure msg ->
            Content.Json { error = msg }
            |> Content.SetStatus (Http.Status.Custom 400 (Some "Bad Request"))

    let ApiContent (ctx: Context<Action>) (action: Action) : Async<Content<Action>> =
        match action with
        | GetPerson id ->
            MakeContent (ApplicationLogic.getPerson id)
        | PostPerson personData ->
            MakeContent (ApplicationLogic.postPerson personData)
        | PutPerson (id, personData) ->
            MakeContent (ApplicationLogic.putPerson id personData)
        | DeletePerson id ->
            MakeContent (ApplicationLogic.deletePerson id)
        | GetPeople ->
            MakeContent (ApplicationLogic.getPeople ())

    let Sitelet = Sitelet.Infer ApiContent

    // Pre-fill the database with a few people.
    do Seq.iter (ApplicationLogic.postPerson >> ignore) [
        { firstName = "Alonzo"
          lastName = "Church" }
        { firstName = "Alan"
          lastName = "Turing" }
        { firstName = "Bertrand"
          lastName = "Russell" }
        { firstName = "Noam"
          lastName = "Chomsky" }
    ]
