port module Main exposing (main)

import Browser exposing (sandbox,element)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autofocus, class, value,placeholder,style)
import Html.Events exposing (onClick, onInput)




import Browser
import Bulma.Components exposing (..)
import Bulma.Elements exposing (tableRow,tableHead,tableFoot,tableCell,table,tableCellHead,tableBody)
import Bulma.Form exposing (..)
import Bulma.Layout exposing (..)
import Bulma.Modifiers exposing (..)


-- We added a new AddTodo message type.

type alias Location = {  name:String , description:String }

type Msg
    = UpdateTextOne String
    |UpdateTextTwo String
    |AddLocation
    |Remove Location
    |EditLocation Location 
    |SubmitEditLocation 
    |CancelEditLocation




type alias Model =
    { textOne : String
    , textTwo : String
    , locations : List Location
    , validation : String
    , editState :  Bool
    , temporaryLocation :Maybe Location
    }






view : Model -> Html Msg
view model =
    div [ class "text-center" ]
        [ case model.editState of 
           True ->  editForm model
           False -> addForm model
           --_ -> addForm model
        , locationTable model
        , validMsg model.validation
       -- , div [] (List.map (\location -> div [] [ text String.concat [location.name, " ", location.description ] ) model.locations)
        ]
        
validMsg : String -> Html Msg 
validMsg st =
        div [ class "text-center text-danger" ] [ text st ]
        
        
        
editForm : Model -> Html Msg
editForm model =
    div [ class "text-center" ]
        [ input [placeholder "Name ", onInput UpdateTextOne, value model.textOne,  autofocus True  ] []
        , input [ onInput UpdateTextTwo, value model.textTwo, placeholder " description ",  autofocus True ] [] 
        , button [ onClick AddLocation, class "btn btn-primary" ] [ text "Edit Location" ]
        , button [ onClick CancelEditLocation , class "btn btn-success" ] [ text "Cancel Edit" ] 
        ]
        
addForm : Model -> Html Msg
addForm model =
    div [ class "text-center" ]
        [ input ([ placeholder "Name" ,onInput UpdateTextOne, value model.textOne, autofocus True  ]++ inputStyle) []
        , input ([ placeholder "description" , onInput UpdateTextTwo, value model.textTwo,  autofocus True ]++ inputStyle) [] 
        , button [ onClick AddLocation, class "btn btn-primary" ] [ text "Add Location" ]
        ]


inputStyle : List (Html.Attribute msg)
inputStyle =
    [ style "display" "inline"
    , style "width" "260px"
    , style "padding" "12px 20px"
    , style "margin" "8px 0"
    , style "border" "solid black 1px"
    , style "border-radius" "4px"
    ]


locationTable model =
    table
        { bordered = True
        , striped = True
        , narrow = True
        , hoverable = True
        , fullWidth = True
        }     
        []
        [ tableHead []
            [ tableRow False
                []
                [ tableCellHead [ class "text-center" ] [ text "NAME" ]
                , tableCellHead [ class "text-center" ] [ text "DESCRIPTION" ]
                , tableCellHead [ class "text-center" ] [ text "ops" ]
                ]
            ]
        , tableBody [] (List.map formatRow model.locations)
        , tableFoot [] []
        ]
     
formatRow : Location -> Html Msg
formatRow location =
    tableRow False
        []
        [ tableCell [] [ text location.name ]
        , tableCell [] [ text location.description ]
        , tableCell [] [button [ onClick (Remove location), class "btn btn-success" ] [ text "Remove Location" ] ]
        , tableCell [] [button [ onClick (EditLocation location ), class "btn btn-success" ] [ text "Edit Location" ] ]
        ]



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateTextOne newText ->
            ({ model | textOne = newText }, Cmd.none)

        UpdateTextTwo newText ->
            ({ model | textTwo = newText }, Cmd.none)
            
        Remove remove ->
            let newLocations = List.filter (\x -> not ( remove.name == x.name)) model.locations
            in
            ({ model | locations = newLocations}, saveLocations newLocations)
        
        AddLocation ->
           
            if not (String.isEmpty model.textOne ) && not (String.isEmpty model.textTwo) then
         
                let newLocations = model.locations ++ [ {name = model.textOne  , description= model.textTwo}]
                in(
                    {model | textOne = "",textTwo = "",validation ="",editState = False,temporaryLocation=Nothing, locations = newLocations },
                    saveLocations newLocations
                  )
            else
            ({ model | validation = "Veuillez remplir les deux champs avant de proceder !!" }, Cmd.none)
            
        EditLocation location ->
            ({ model | textOne = location.name ,textTwo = location.description, temporaryLocation =Just location  ,editState =True,locations = List.filter (\x -> not ( location.name == x.name)) model.locations}
                , Cmd.none)
        CancelEditLocation ->
            case model.temporaryLocation of 
                Just location -> ({model | locations = model.locations ++ [location],textOne = "", textTwo = "" , temporaryLocation = Nothing, editState = False }, Cmd.none)
                Nothing -> ({model | validation = "Edit issue arised" }, Cmd.none)
                
            --{model | textOne = "", textTwo = "", locations = model.locations ++ [model.temporaryLocation] , temporaryLocation = Nothing, editState = False }
       
        _-> ({model | textOne = ""}, Cmd.none)


port saveLocations : List Location -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : Flags -> (Model, Cmd Msg)
init flags = 
        ( { textOne = "",textTwo = "",  locations = flags.locations, validation = "", editState = False, temporaryLocation = Nothing } , Cmd.none )

type alias Flags = { locations : List Location }

main : Program Flags Model Msg
main =
    element
        { init =init
        , view = view
        , update = update
        ,subscriptions = subscriptions
        }
