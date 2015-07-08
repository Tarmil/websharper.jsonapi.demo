namespace JsonDemo

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI.Next
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Notation
open WebSharper.UI.Next.Templating

module Resources =
    type CssResource() =
        inherit Resources.BaseResource("/css/style.css")

[<JavaScript>]
module Client =
    open RestApi

    type Result<'T> =
        | Success of 'T
        | Failure of message: string

    module Result =
        let map f res =
            match res with
            | Success r -> Success <| f r
            | Failure f -> Failure f

    type View with
        static member Sequence views =
            views
            |> Seq.fold (View.Map2 (fun a b -> 
                seq { yield! a; yield b })) (View.Const Seq.empty)

    let private toError s =
        s |> View.Map (function
            | Success _ -> None
            | Failure f -> Some f)

    let private validate pred msg v = 
        v |> View.Map (fun e -> if pred e then Success e else Failure msg)

    type private ApiData<'T> =
        {
            Type : RequestType
            Url  : string
            Data : string option
            OnSuccess : string -> 'T
        }

    let private mkApiData t u d ons =
        { 
            Type = t
            Url = u
            Data = d
            OnSuccess = ons
        }

    let private apiCall data =
        Async.FromContinuations <| fun (ok, ko, _) ->
            let settings =
                AjaxSettings(
                    Type = data.Type,
                    Url = data.Url,
                    ContentType = "application/json",
                    DataType = DataType.Text,
                    Success = (fun (respData, _, _) ->
                        ok <| data.OnSuccess (respData :?> string)),
                    Error = (fun (_, _, error) ->
                        ko <| System.Exception(error))
                )
            data.Data |> Option.iter (fun d -> settings.Data <- d)
            JQuery.Ajax(settings) |> ignore

    let private fetchPeople () =
        mkApiData RequestType.GET "/api/people" None 
        <| Json.Deserialize<Result<(Id * PersonData) []>>
        |> apiCall

    let private getPerson (id : int) =
        mkApiData RequestType.GET ("/api/person/" + string id) None 
        <| Json.Deserialize<Result<PersonData>>
        |> apiCall

    let private postPerson (data : PersonData) =
        mkApiData RequestType.POST "/api/person"
        <| Some (Json.Serialize data )
        <| Json.Deserialize<Result<Id>>
        |> apiCall

    let private putPerson (id : int) (data : PersonData) =
        mkApiData RequestType.PUT ("/api/person" + string id)
        <| Some (Json.Serialize data)
        <| Json.Deserialize<Result<unit>>
        |> apiCall

    let private deletePerson (id : int) =
        mkApiData RequestType.DELETE ("/api/person" + string id) None
        <| Json.Deserialize<Result<unit>>
        |> apiCall

    type PeopleInfo = Template<"PeopleInfo.html">

    [<Require(typeof<Resources.CssResource>)>]
    let Main () =
        let people : ListModel<int, Id * PersonData> = 
            ListModel.Create (fun (id, _) -> id.id) []

        async {
            let! res = fetchPeople ()
            match res with
            | Success ppl ->
                for p in ppl do
                    people.Add p
            | Failure f -> 
                Console.Log f
        }
        |> Async.Start

        let isEmpty (s : string) = s.Trim () = ""
        let validDate f d =
            let d = Date.Parse(d)
            if JS.IsNaN d then Failure f
            else Success <| Date(d).Self

        let firstName = Var.Create ""
        let lastName = Var.Create ""
        let born = Var.Create ""
        let bornV = 
            born.View
            |> View.Map (validDate "Bad date format: date of birth.")
        let died = Var.Create ""
        let diedV =
            died.View
            |> View.Map (fun d ->
                if d.Trim() = "" then Success None
                else validDate "Bad date format: date of death." d |> Result.map Some)

        let resultErr : Var<string option> = Var.Create None
        let submitPressed = Var.Create false

        let errors =
            [
                firstName.View |> validate (not << isEmpty) "First name cannot be empty."
                lastName.View |> validate (not << isEmpty) "Last name cannot be empty."
                born.View |> validate (not << isEmpty) "Date of birth cannot be empty."
            ]
            |> Seq.map toError
            |> fun e ->
                Seq.append e
                    [ bornV |> toError
                      diedV |> toError ]
            |> View.Sequence
            |> View.Map (Seq.choose id)

        let valid = errors |> View.Map Seq.isEmpty

        let submitButton valid born died =
            match born, died with
            | Success b, Success d ->
                Doc.Button "Add" [] <| fun () ->
                    submitPressed := true
                    if valid then
                        let data =
                            { firstName = firstName.Value
                              lastName = lastName.Value
                              born = b
                              died = d }
                        async {
                            let! res = postPerson data
                            match res with
                            | Success id ->
                                people.Add (id, data)
                            | Failure f ->
                                Console.Log f
                            submitPressed := false
                        }
                        |> Async.Start

            | _ -> Doc.Button "Add" [] <| fun () -> submitPressed := true

        PeopleInfo.Doc(
            FirstName = firstName,
            LastName = lastName,
            Born = born,
            Died = died,
            SubmitButton =
                (valid
                |> View.Bind (fun vld ->
                    View.Map2 (fun born died -> 
                        submitButton vld born died
                    ) bornV diedV
                )
                |> Doc.EmbedView),
            People =
                (people
                |> ListModel.View
                |> Doc.Convert (fun (id, pd) ->
                    let vc : string -> View<string> = View.Const
                    let died = defaultArg (pd.died |> Option.map (fun e -> e.JS.ToLocaleString())) ""
                    Console.Log died
                    if pd.died.IsSome then Console.Log pd.died.Value

                    PeopleInfo.Info.Doc(
                        FirstName = vc pd.firstName,
                        LastName = vc pd.lastName,
                        Born = vc (pd.born.JS.ToLocaleString()),
                        Died = vc died,
                        OnDeleteClick = fun ev ->
                            async {
                                let! res = deletePerson id.id
                                match res with
                                | Success () ->
                                    people.RemoveByKey id.id
                                | Failure f ->
                                    Console.Log f
                            }
                            |> Async.Start
                    )
                )),
            Errors = 
                (errors
                |> View.Map2 (fun (se : string option) errs ->
                    submitPressed.View 
                    |> View.Map (fun sp ->
                        if sp then // show errors only if submit was pressed
                            // append error from result (if any) to the list of errors
                            seq { yield! errs; if se.IsSome then yield se.Value }
                            |> Seq.map (fun e -> PeopleInfo.Error.Doc(Message = View.Const e))
                            |> Doc.Concat
                        else Doc.Empty
                    )
                ) resultErr.View
                |> View.Join
                |> Doc.EmbedView)
        )
