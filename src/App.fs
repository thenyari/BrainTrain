// BrainTrain – Final Full Version
module App

open Feliz
open Feliz.UseElmish
open Elmish
open Elmish.React
open Browser.Dom
open Browser
open System
open Fable.Core.JsInterop

// -------------------- TYPES --------------------

type Topic = Animals | Fruits | Countries

type Difficulty = Easy | Medium | Hard

let difficultyLabel = function Easy -> "Easy" | Medium -> "Medium" | Hard -> "Hard"

type Question = {
    Text: string
    Options: string list
    Correct: string
    Difficulty: Difficulty
    IsBonus: bool
}

type Phase = ChooseName | ChooseTopic | ChooseDifficulty | InGame | Practice | GameOver | TestMode

type Stats = {
    Correct: int
    Incorrect: int
    TotalTime: float
    Mistakes: Question list
}

type Player = {
    Name: string
    Score: int
}

type Theme = Light | Dark

let defaultStats = { Correct = 0; Incorrect = 0; TotalTime = 0.0; Mistakes = [] }

// New for timer
let testDuration = 30.0 // seconds
let questionTimeLimit = 10.0 // seconds

// -------------------- MODEL --------------------

type Model = {
    Name: string
    Topic: Topic option
    Phase: Phase
    Questions: Question list
    Current: int
    SelectedAnswer: string option
    ShowResult: bool
    Stats: Stats
    Theme: Theme
    Leaderboard: Player list
    DifficultyFilter: Difficulty option
    Timer: float
    TestStartTime: float
    PracticeQueue: Question list
}

type Msg =
    | EnterName of string
    | SelectTopic of Topic
    | SelectDifficulty of Difficulty
    | StartGame
    | ChooseAnswer of string
    | Next
    | Restart
    | ToggleTheme
    | LoadLeaderboard
    | Tick of float
    | StartTestMode
    | EndTest

// -------------------- DATA --------------------

let allQuestions =
    Map [
        Animals, [
            { Text = "Largest land mammal?"; Options = ["Elephant"; "Lion"; "Hippo"]; Correct = "Elephant"; Difficulty = Easy; IsBonus = false }
            { Text = "Animal with pouch?"; Options = ["Kangaroo"; "Crocodile"; "Horse"]; Correct = "Kangaroo"; Difficulty = Medium; IsBonus = true }
            { Text = "Which barks?"; Options = ["Dog"; "Cat"; "Mouse"]; Correct = "Dog"; Difficulty = Easy; IsBonus = false }
        ]
        Fruits, [
            { Text = "Citrus fruit?"; Options = ["Orange"; "Apple"; "Grapes"]; Correct = "Orange"; Difficulty = Easy; IsBonus = false }
            { Text = "Has a pit?"; Options = ["Peach"; "Strawberry"; "Pear"]; Correct = "Peach"; Difficulty = Medium; IsBonus = false }
            { Text = "Yellow & long?"; Options = ["Banana"; "Fig"; "Blueberry"]; Correct = "Banana"; Difficulty = Easy; IsBonus = true }
        ]
        Countries, [
            { Text = "Eiffel Tower?"; Options = ["Italy"; "France"; "Belgium"]; Correct = "France"; Difficulty = Easy; IsBonus = false }
            { Text = "Island nation?"; Options = ["Japan"; "China"; "Mexico"]; Correct = "Japan"; Difficulty = Medium; IsBonus = false }
            { Text = "Pyramids?"; Options = ["Greece"; "Egypt"; "Russia"]; Correct = "Egypt"; Difficulty = Hard; IsBonus = true }
        ]
    ]

let shuffle rng list =
    list |> List.map (fun x -> rng.Next(), x) |> List.sortBy fst |> List.map snd

// -------------------- INIT --------------------

let init () =
    { Name = ""
      Topic = None
      Phase = ChooseName
      Questions = []
      Current = 0
      SelectedAnswer = None
      ShowResult = false
      Stats = defaultStats
      Theme = Light
      Leaderboard = []
      DifficultyFilter = None
      Timer = questionTimeLimit
      TestStartTime = 0.0
      PracticeQueue = [] }, Cmd.ofMsg LoadLeaderboard

// -------------------- UPDATE --------------------

let update msg model =
    match msg with
    | EnterName name -> { model with Name = name }, Cmd.none
    | SelectTopic topic -> { model with Topic = Some topic; Phase = ChooseDifficulty }, Cmd.none
    | SelectDifficulty diff ->
        let q =
            model.Topic
            |> Option.bind (fun t -> Map.tryFind t allQuestions)
            |> Option.defaultValue []
            |> List.filter (fun q -> q.Difficulty = diff)
            |> shuffle (Random())
        { model with Questions = q; DifficultyFilter = Some diff; Phase = InGame; Stats = defaultStats; Current = 0; Timer = questionTimeLimit }, Cmd.none
    | StartGame -> model, Cmd.none
    | ChooseAnswer answer ->
        { model with SelectedAnswer = Some answer; ShowResult = true }, Cmd.none
    | Next ->
        let currentQ = model.Questions[model.Current]
        let isCorrect = model.SelectedAnswer = Some currentQ.Correct
        let bonus = if currentQ.IsBonus && isCorrect then 4 else 0
        let nextIndex = model.Current + 1
        let updatedStats = {
            model.Stats with
                Correct = model.Stats.Correct + (if isCorrect then 1 + bonus else 0)
                Incorrect = model.Stats.Incorrect + (if isCorrect then 0 else 1)
                Mistakes = if isCorrect then model.Stats.Mistakes else currentQ :: model.Stats.Mistakes
        }
        if nextIndex >= model.Questions.Length then
            { model with Phase = Practice; Stats = updatedStats; PracticeQueue = updatedStats.Mistakes }, Cmd.none
        else
            { model with Current = nextIndex; SelectedAnswer = None; ShowResult = false; Stats = updatedStats; Timer = questionTimeLimit }, Cmd.none
    | Restart -> fst (init ())
    | ToggleTheme ->
        let newTheme = if model.Theme = Light then Dark else Light
        { model with Theme = newTheme }, Cmd.none
    | LoadLeaderboard ->
        let players =
            [ for i in 0..100 do
                let key = "braintrain-score-" + string i
                match window.localStorage.getItem key with
                | null -> ()
                | s ->
                    try
                        let json = ofJson<{| name: string; score: int |}> s
                        yield { Name = json.name; Score = json.score }
                    with _ -> () ]
            |> List.sortByDescending (fun p -> p.Score)
        { model with Leaderboard = players }, Cmd.none
    | Tick dt ->
        let newTimer = model.Timer - dt
        if newTimer <= 0.0 then
            update Next { model with Timer = questionTimeLimit }
        else
            { model with Timer = newTimer }, Cmd.none
    | StartTestMode ->
        let allQs =
            allQuestions
            |> Map.values
            |> Seq.concat
            |> shuffle (Random())
            |> List.take 100
        { model with
            Phase = TestMode
            Questions = allQs
            Stats = defaultStats
            Current = 0
            Timer = testDuration
            TestStartTime = performance.now()
        }, Cmd.none
    | EndTest ->
        let total = model.Stats.Correct + model.Stats.Incorrect
        let entry = {| name = model.Name; score = model.Stats.Correct |} |> toJson
        window.localStorage.setItem("braintrain-score-" + model.Name, entry)
        { model with Phase = GameOver }, Cmd.none

// -------------------- VIEW --------------------

let view () =
    let model, dispatch = React.useElmish(init, update, [||])
    let bgColor = if model.Theme = Dark then "#1e1e1e" else "white"
    let txtColor = if model.Theme = Dark then "#f0f0f0" else "black"

    Html.div [
        prop.style [ style.padding 20; style.backgroundColor bgColor; style.color txtColor ]
        prop.children [
            Html.h1 "🧠 BrainTrain Final"
            Html.button [
                prop.text (if model.Theme = Light then "🌙 Dark Mode" else "☀️ Light Mode")
                prop.onClick (fun _ -> dispatch ToggleTheme)
            ]
            match model.Phase with
            | ChooseName ->
                Html.div [
                    Html.p "Enter name:"
                    Html.input [ prop.value model.Name; prop.onChange (EnterName >> dispatch) ]
                    Html.button [ prop.text "Start"; prop.onClick (fun _ -> dispatch (SelectTopic Animals)) ]
                ]
            | ChooseTopic ->
                Html.div ([ Html.h3 "Choose topic:" ] @
                    ([ Animals; Fruits; Countries ] |> List.map (fun t ->
                        Html.button [ prop.text (string t); prop.onClick (fun _ -> dispatch (SelectTopic t)) ])))
            | ChooseDifficulty ->
                Html.div ([ Html.h3 "Select difficulty:" ] @
                    ([ Easy; Medium; Hard ] |> List.map (fun d ->
                        Html.button [ prop.text (difficultyLabel d); prop.onClick (fun _ -> dispatch (SelectDifficulty d)) ])) @
                    [ Html.button [ prop.text "Test Mode"; prop.onClick (fun _ -> dispatch StartTestMode) ] ])
            | InGame | Practice ->
                let q = model.Questions[model.Current]
                Html.div [
                    Html.p $"Q{model.Current + 1}/{model.Questions.Length} ({difficultyLabel q.Difficulty}) {(if q.IsBonus then "⭐" else "")}"
                    Html.p $"⏱️ {model.Timer |> int} seconds left"
                    Html.h3 q.Text
                    Html.div (
                        q.Options |> List.map (fun opt ->
                            Html.button [
                                prop.text opt
                                prop.onClick (fun _ -> dispatch (ChooseAnswer opt))
                                prop.style [ if Some opt = model.SelectedAnswer then style.backgroundColor "lightblue" ]
                            ]))
                    if model.ShowResult then Html.button [ prop.text "Next"; prop.onClick (fun _ -> dispatch Next) ] else Html.none
                ]
            | TestMode ->
                Html.div [
                    Html.h3 $"⏱️ {int model.Timer} seconds left"
                    Html.p $"Score: {model.Stats.Correct}"
                    if model.Timer <= 0.0 then Html.button [ prop.text "End Test"; prop.onClick (fun _ -> dispatch EndTest) ]
                    else Html.button [ prop.text "Next (test)"; prop.onClick (fun _ -> dispatch Next) ]
                ]
            | GameOver ->
                Html.div [
                    Html.h2 "🎉 Game Over!"
                    Html.p $"✅ Correct: {model.Stats.Correct}"
                    Html.p $"❌ Incorrect: {model.Stats.Incorrect}"
                    Html.button [ prop.text "🔁 Restart"; prop.onClick (fun _ -> dispatch Restart) ]
                    Html.hr []
                    Html.h3 "🏆 Leaderboard"
                    model.Leaderboard |> List.truncate 5 |> List.map (fun p -> Html.p $"{p.Name}: {p.Score}") |> Html.div
                ]
        ]
    ]
