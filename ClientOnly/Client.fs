namespace ClientOnly

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI.Next
open WebSharper.UI.Next.Client
open WebSharper.UI.Next.Html
open WebSharper.UI.Next.Notation
open WebSharper.UI.Next.Templating

module Resources =
    type CssResource() =
        inherit Resources.BaseResource("style.css")

    [<assembly:System.Web.UI.WebResource("style.css", "text/css")>]
    do ()

[<JavaScript>]
module Client =

    type Result<'T> =
        | Success of 'T
        | Failure of message: string

    module Result =
        let map f res =
            match res with
            | Success r -> Success <| f r
            | Failure f -> Failure f

    let private toError s =
        s |> View.Map (function
            | Success _ -> None
            | Failure f -> Some f)

    let private validate pred msg v = 
        v |> View.Map (fun e -> if pred e then Success e else Failure msg)

    type Id = Key

    /// Data about a person.
    type PersonData =
        { firstName: string
          lastName: string }

    type PeopleInfo = Template<"../CommonResources/PeopleInfo.html">

    [<Require(typeof<Resources.CssResource>)>]
    let Main() =
        let people : ListModel<Id, Id * PersonData> = 
            ListModel.Create (fun (id, _) -> id)
                <| List.map (fun x -> Key.Fresh(), x) [
                    { firstName = "Alonzo"
                      lastName = "Church" }
                    { firstName = "Alan"
                      lastName = "Turing" }
                    { firstName = "Bertrand"
                      lastName = "Russell" }
                    { firstName = "Noam"
                      lastName = "Chomsky" }
                ]

        let postPerson data =
            async {
                let id = Key.Fresh()
                return people.Add(id, data)
            }

        let deletePerson id =
            async {
                return people.RemoveByKey(id)
            }

        let isEmpty (s : string) = s.Trim () = ""
        let validDate f d =
            let d = Date.Parse(d)
            if JS.IsNaN d then Failure f
            else Success <| Date(d).Self

        let firstName = Var.Create ""
        let lastName = Var.Create ""

        let resultErr : Var<string option> = Var.Create None
        let submitPressed = Var.Create false

        let errors =
            [
                firstName.View |> validate (not << isEmpty) "First name cannot be empty."
                lastName.View |> validate (not << isEmpty) "Last name cannot be empty."
            ]
            |> Seq.map toError
            |> View.Sequence
            |> View.Map (Seq.choose id)

        let valid = errors |> View.Map Seq.isEmpty

        let submitButton valid =
            Doc.Button "Add" [] <| fun () ->
                submitPressed := true
                if valid then
                    let data =
                        { firstName = firstName.Value
                          lastName = lastName.Value }
                    async {
                        do! postPerson data
                        submitPressed := false
                    }
                    |> Async.Start

        PeopleInfo.Doc(
            FirstName = firstName,
            LastName = lastName,
            SubmitButton = [valid |> Doc.BindView submitButton],
            People = [
                people
                |> ListModel.View
                |> Doc.BindSeqCached (fun (id, pd) ->
                    PeopleInfo.Info.Doc(
                        FirstName = pd.firstName,
                        LastName = pd.lastName,
                        OnDeleteClick = (fun el ev ->
                            deletePerson id
                            |> Async.Start
                        )
                    )
                )
            ],
            Errors = [
                View.Map2 (fun (se : string option) errs ->
                    submitPressed.View 
                    |> View.Map (fun sp ->
                        if sp then // show errors only if submit was pressed
                            // append error from result (if any) to the list of errors
                            seq { yield! errs; if se.IsSome then yield se.Value }
                            |> Seq.map (fun e -> PeopleInfo.Error.Doc(Message = e))
                            |> Doc.Concat
                        else Doc.Empty
                    )
                ) resultErr.View errors
                |> View.Join
                |> Doc.EmbedView
            ]
        )

    /// Single-Page Applications need to have their entry point
    /// called by a top-level value.
    let RunMain = Main() |> Doc.RunById "main"
