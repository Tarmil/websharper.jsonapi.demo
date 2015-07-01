namespace JsonDemo

open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.Html.Client

[<JavaScript>]
module Client =
    open RestApi

    type ApiData<'T> =
        {
            Type : RequestType
            Url  : string
            UrlParams : (string * string) seq
            Data : string option
            OnSuccess : string -> 'T
        }

    let mkApiData t u up d ons =
        { 
            Type = t
            Url = u
            UrlParams = up
            Data = d
            OnSuccess = ons
        }

    let apiCall data =
        Async.FromContinuations <| fun (ok, ko, _) ->
            let url = 
                let parts = 
                    data.UrlParams 
                    |> Seq.map (fun (a, b) -> a + "=" + b)
                    |> String.concat "&"
                data.Url + "?" + parts
            let settings =
                AjaxSettings(
                    Type = data.Type,
                    Url = url,
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
        Async.FromContinuations <| fun (ok, ko, _) ->
            JQuery.Ajax(
                AjaxSettings(
                    Type = RequestType.GET,
                    Url = "/api/people",
                    ContentType = "application/json",
                    DataType = DataType.Text,
                    Success = (fun (respData, _, _) ->
                        ok <| Json.Deserialize<Result<(Id * RestApi.PersonData) []>> (respData :?> string)),
                    Error = (fun (_, _, error) ->
                        ko <| System.Exception(error))
                )
            )
            |> ignore

    let private fetchPeople2 () =
        mkApiData RequestType.GET "/api/people" [] None 
        <| Json.Deserialize<Result<(Id * RestApi.PersonData) []>>
        |> apiCall

    let private getPerson (id : int) =
        Async.FromContinuations <| fun (ok, ko, _) ->
            JQuery.Ajax(
                AjaxSettings(
                    Type = RequestType.GET,
                    Url = "/api/person?id=" + string id,
                    ContentType = "application/json",
                    DataType = DataType.Text,
                    Success = (fun (respData, a, b) ->
                        ok <| Json.Deserialize<Result<RestApi.PersonData>> (respData :?> string)),
                    Error = (fun (_, _, error) ->
                        ko <| System.Exception(error))
                )
            )
            |> ignore

    let private postPerson (data : PersonData) =
        Async.FromContinuations <| fun (ok, ko, _) ->
            JQuery.Ajax(
                AjaxSettings(
                    Type = RequestType.POST,
                    Url = "/api/person",
                    ContentType = "application/json",
                    DataType = DataType.Text,
                    Data = Json.Serialize data,
                    Success = (fun (respData, a, b) ->
                        ok <| Json.Deserialize<Result<Id>> (respData :?> string)),
                    Error = (fun (_, _, error) ->
                        ko <| System.Exception(error))
                )
            )
            |> ignore

    let private putPerson (id : int) (data : PersonData) =
        Async.FromContinuations <| fun (ok, ko, _) ->
            JQuery.Ajax(
                AjaxSettings(
                    Type = RequestType.PUT,
                    Url = "/api/person?id=" + string id,
                    ContentType = "application/json",
                    DataType = DataType.Text,
                    Data = Json.Serialize data,
                    Success = (fun (respData, a, b) ->
                        ok <| Json.Deserialize<Result<unit>> (respData :?> string)),
                    Error = (fun (_, _, error) ->
                        ko <| System.Exception(error))
                )
            )
            |> ignore

    let private deletePerson (id : int) =
        Async.FromContinuations <| fun (ok, ko, _) ->
            JQuery.Ajax(
                AjaxSettings(
                    Type = RequestType.DELETE,
                    Url = "/api/person?id=" + string id,
                    ContentType = "application/json",
                    DataType = DataType.Text,
                    Success = (fun (respData, a, b) ->
                        ok <| Json.Deserialize<Result<unit>> (respData :?> string)),
                    Error = (fun (_, _, error) ->
                        ko <| System.Exception(error))
                )
            )
            |> ignore

    let Main () =
        async {
            let! people = fetchPeople ()
            Console.Log people
        }
        |> Async.Start

        Text ""
