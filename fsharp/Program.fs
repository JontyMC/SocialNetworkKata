open System
open System.Collections.Generic

type Subject = string
type Message = string
type Action = Post of Message | Read | Follow of Subject | Wall
type Post = Subject * Message * DateTimeOffset
type Command = Subject * Action

let formatDateTime (now: DateTimeOffset) (postDateTime: DateTimeOffset) =
    let dateFormat = " ({0} {1} ago)"
    let postAge = now.Subtract(postDateTime)
    match postAge with
    | x when x.Days > 0 -> String.Format(dateFormat, x.Days, (if x.Days = 1 then "day" else "days"))
    | x when x.Hours > 0 -> String.Format(dateFormat, x.Hours, (if x.Hours = 1 then "hour" else "hours"))
    | x when x.Minutes > 0 -> String.Format(dateFormat, x.Minutes, (if x.Minutes = 1 then "minute" else "minutes"))
    | x -> String.Format(dateFormat, x.Seconds, (if x.Seconds = 1 then "second" else "seconds"))

let formatPost now message postDateTime = message + (formatDateTime now postDateTime)

let formatWallPost now subject message postDateTime = subject + " - " + (formatPost now message postDateTime)

let readPosts subject posts now =
    let subjectPosts = posts |> List.filter (fun (x, _, _) -> x = subject)
    if subjectPosts.IsEmpty
    then ["No posts for " + subject]
    else subjectPosts |> List.map (fun (_, x, y) -> formatPost now x y)

let wallPosts subject (posts: Post list) (subscriptionsBySubject: Map<Subject, Set<Subject>>) now =
    let noWallPosts subject = ["No wall posts for " + subject]
    let subjectSubscriptions = subscriptionsBySubject |> Map.tryFind subject
    match subjectSubscriptions with
    | Some subscriptions ->
        let wallPosts = posts |> List.filter (fun (x, _, _) -> x = subject || subscriptions.Contains x)
        if wallPosts.IsEmpty
        then noWallPosts subject
        else wallPosts |> List.map (fun (x, y, z) -> formatWallPost now x y z)
    | None -> noWallPosts subject

let subscribe (subscriptionsBySubject: Map<Subject, Set<Subject>>) subject followee =
    match subscriptionsBySubject.TryFind subject with
    | Some subscriptions -> subscriptionsBySubject.Add (subject, subscriptions.Add followee)
    | None -> subscriptionsBySubject.Add (subject, set [followee])

let processCommand (posts: Post list) subscriptionsBySubject now (command: Command) =
    match command with
    | (subject, Post message) -> ((subject, message, now) :: posts, subscriptionsBySubject, None)
    | (subject, Read) -> (posts, subscriptionsBySubject, Some(readPosts subject posts now))
    | (subject, Follow followee) -> (posts, subscribe subscriptionsBySubject subject followee, None)
    | (subject, Wall) -> (posts, subscriptionsBySubject, Some(wallPosts subject posts subscriptionsBySubject now))

let parseCommand (commandText: String) =
    let parts = commandText.Trim().Split([|' '|], 3)
    match parts with
    | [| subject; "->"; message |] -> Some (subject, Action.Post message)
    | [| subject |] -> Some (subject, Read)
    | [| subject; "follows"; followee |] -> Some (subject, Follow followee)
    | [| subject; "wall" |] -> Some (subject, Wall)
    | _ -> None

let rec processInput posts subscriptionsBySubject getDateTime read (write: string -> unit) =
    let command = read() |> parseCommand
    match command with
    | Some command ->
        let now = getDateTime()
        let (posts, subscriptionsBySubject, output) = processCommand posts subscriptionsBySubject now command
        match output with
        | Some output -> output |> List.iter write
        | None -> ()
        processInput posts subscriptionsBySubject getDateTime read write
    | None ->
        write "Invalid input"
        processInput posts subscriptionsBySubject getDateTime read write

[<EntryPoint>]
let main argv =
    processInput List.empty Map.empty (fun () -> DateTimeOffset.UtcNow) (fun () -> Console.ReadLine()) (fun (x: String) -> Console.WriteLine(x))
    0