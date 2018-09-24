module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (..)
import List exposing (..)


---- MODEL ----


type alias Model =
    { todoList : List Todo
    , inputText : String
    , maxId : Int
    , displayType : DisplayType
    }


type alias Todo =
    { id : Int
    , description : String
    , completed : Bool
    }


type DisplayType
    = NEW
    | COMPLETE


init : () -> ( Model, Cmd Msg )
init _ =
    ( { todoList = []
      , inputText = ""
      , maxId = 0
      , displayType = NEW
      }
    , Cmd.none
    )


newTodo : Int -> String -> Todo
newTodo id desc =
    { id = id
    , description = desc
    , completed = False
    }



---- UPDATE ----


type Msg
    = NoOp
    | UpdateInputText String
    | AddTodo
    | CompleteTodo Int
    | RemoveTodo Int
    | ToggleDisplayType DisplayType


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateInputText t ->
            ( { model | inputText = t }, Cmd.none )

        AddTodo ->
            ( { model
                | maxId = model.maxId + 1
                , todoList = model.todoList ++ [ newTodo model.maxId model.inputText ]
                , inputText = ""
              }
            , Cmd.none
            )

        CompleteTodo id ->
            ( { model
                | todoList =
                    List.filter (\i -> i.id /= id) model.todoList
                        ++ (model.todoList
                                |> List.filter (\i -> i.id == id)
                                |> List.map (\i -> { i | completed = True })
                           )
              }
            , Cmd.none
            )

        RemoveTodo id ->
            ( model, Cmd.none )

        ToggleDisplayType type_ ->
            ( { model | displayType = type_ }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { todoList, inputText, displayType } =
    div [ class "container", align "center" ]
        [ h1 [] [ text "elm-todo" ]
        , div []
            [ label []
                [ input [ type_ "radio", name "type", onClick (ToggleDisplayType NEW), checked (displayType == NEW) ] []
                , text "New"
                ]
            , label []
                [ input [ type_ "radio", name "type", onClick (ToggleDisplayType COMPLETE) ] []
                , text "Completed"
                ]
            ]
        , table [] (list2tr todoList (displayType == COMPLETE))
        , div []
            [ input [ type_ "text", value inputText, onInput (\w -> UpdateInputText w) ] []
            , input [ type_ "button", value "add", onClick AddTodo ] []
            ]
        ]


list2tr : List Todo -> Bool -> List (Html Msg)
list2tr todoList completed =
    todoList
        |> List.filter (\item -> item.completed == completed)
        |> List.map
            (\item ->
                tr []
                    [ td [] [ text item.description ]
                    , td [] [ input [ type_ "button", value "complete!", onClick (CompleteTodo item.id) ] [] ]
                    ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
