module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import List


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
                    List.filter (\item -> item.id /= id) model.todoList
                        ++ (model.todoList
                                |> List.filter (\item -> item.id == id)
                                |> List.map (\item -> { item | completed = True })
                           )
              }
            , Cmd.none
            )

        RemoveTodo id ->
            ( { model
                | todoList = List.filter (\item -> item.id /= id) model.todoList
              }
            , Cmd.none
            )

        ToggleDisplayType type_ ->
            ( { model | displayType = type_ }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view { todoList, inputText, displayType } =
    div [ class "container" ]
        [ h1 [ class "display-4 text-center" ] [ text "Elm Todo" ]
        , div [ class "row" ]
            [ div [ class "col-md-3" ]
                []
            , label
                [ class "col-md-3" ]
                [ input [ type_ "radio", name "type", onClick (ToggleDisplayType NEW), checked (displayType == NEW) ] []
                , text "New"
                ]
            , label [ class "col-md-3" ]
                [ input [ type_ "radio", name "type", onClick (ToggleDisplayType COMPLETE) ] []
                , text "Completed"
                ]
            ]
        , table [ class "table table-striped" ]
            [ thead []
                [ tr []
                    [ th [ class "col-md-8", scope "col" ] [ text "Todo" ]
                    , th [ class "col-md-4", scope "col" ] [ text "#" ]
                    ]
                ]
            , tbody [] (list2tr (displayType == COMPLETE) todoList)
            ]
        , div [ class "input-group" ]
            [ input
                [ type_ "text", class "form-control", value inputText, onInput (\w -> UpdateInputText w) ]
                []
            , div [ class "input-group-append" ]
                [ input
                    [ type_ "button", class "btn btn-primary", value "add", onClick AddTodo ]
                    []
                ]
            ]
        ]


list2tr : Bool -> List Todo -> List (Html Msg)
list2tr completed todoList =
    let
        ( btnCls, btnMsg, evt ) =
            if completed then
                ( "btn btn-danger", "delete", RemoveTodo )
            else
                ( "btn btn-primary", "Complete!", CompleteTodo )
    in
        todoList
            |> List.filter (\item -> item.completed == completed)
            |> List.map
                (\item ->
                    tr []
                        [ td [ class "col-md-8" ] [ text item.description ]
                        , td [ class "col-md-4" ]
                            [ input
                                [ type_ "button", class btnCls, value btnMsg, onClick (evt item.id) ]
                                []
                            ]
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
