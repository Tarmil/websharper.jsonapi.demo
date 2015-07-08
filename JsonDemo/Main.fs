namespace JsonDemo

open WebSharper.Html.Server
open WebSharper
open WebSharper.Sitelets

type Action =
    | Home
    | [<CompiledName "api">] Api of RestApi.Action

module Skin =
    open System.Web

    type Page =
        {
            Title : string
            Body : list<Element>
        }

    let MainTemplate =
        Content.Template<Page>("~/Main.html")
            .With("title", fun x -> x.Title)
            .With("body", fun x -> x.Body)

    let WithTemplate title body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Title = title
                Body = body context
            }

module Site =

    let HomePage =
        Skin.WithTemplate "HomePage" <| fun ctx ->
            [
                Div [ClientSide <@ Client.Main() @>]
            ]

    let Main =
        Sitelet.Sum [
            Sitelet.Content "/" Home HomePage
            Sitelet.Shift "api" (Sitelet.EmbedInUnion <@ Api @> RestApi.Sitelet)
        ]

[<Sealed>]
type Website() =
    interface IWebsite<Action> with
        member this.Sitelet = Site.Main
        member this.Actions = [Home]

type Global() =
    inherit System.Web.HttpApplication()

    member g.Application_Start(sender: obj, args: System.EventArgs) =
        ()

[<assembly: Website(typeof<Website>)>]
do ()
