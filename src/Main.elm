import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Url
import Http exposing (..)
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


-- MAIN


main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL
type alias Vocab = 
  {
    id : Int, 
    english : String ,
    vietnamese : String
  }



type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , new: Vocab
  , vocabs : List Vocab
  }

initModel :  Url.Url -> Nav.Key -> Model
initModel url key = 
  {
    key = key
  , url = url
  , new = {id = 0, english = "", vietnamese = ""}
  , vocabs = []
  }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( initModel url key, fecthRequest )


fecthRequest: Cmd Msg
fecthRequest = 
  Http.get
    { url = "http://localhost:1323/api/vocabs"
    , expect = Http.expectJson GotVocab decodeVocabs
    }

submitNewVocab: Vocab -> Cmd Msg
submitNewVocab vocab =
  let 
    data = 
        Encode.object
          [ ( "english", Encode.string vocab.english )
          , ( "vietnamese", Encode.string vocab.vietnamese )
          ]
    body = Http.jsonBody data
  in 
    Http.post
      { url = "http://localhost:1323/api/vocab"
      , body = body
      , expect = Http.expectJson GotNewVocab decodeVocab
      }
  

deleteVoc : Int -> Cmd Msg 
deleteVoc id =
  let
    stringId = Debug.toString id
    url = String.concat ["http://localhost:1323/api/vocab/", stringId]
  in 
    Http.request
    { method = "DELETE"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson GotVocab decodeVocabs
    , timeout = Nothing
    , tracker = Nothing
    }
  -- Http.get
  --   { url = url
  --   , expect = Http.expectJson GotVocab decodeVocabs
  --   }

-- UPDATE

decodeVocabs: Decode.Decoder (List Vocab)
decodeVocabs =
  Decode.list decodeVocab


decodeVocab: Decode.Decoder Vocab
decodeVocab = 
  Decode.map3 Vocab
    (field "id" Decode.int)
    (field "english" Decode.string)
    (field "vietnamese" Decode.string)


type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | ChangeEnglish String
  | ChangeVietnamese String
  | SubmitVocab
  | GotVocab (Result Http.Error (List Vocab))
  | GotNewVocab (Result Http.Error Vocab)
  | DeleteVocab Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      ( { model | url = url }
      , Cmd.none
      )
    ChangeEnglish text -> 
      ({model | new = changeEnglish model.new text }, Cmd.none)
    ChangeVietnamese text -> 
      ({model | new = changeVietnamese model.new text }, Cmd.none)
    SubmitVocab -> 
      (model, submitNewVocab model.new)
    DeleteVocab id ->
      (model, deleteVoc id)
    GotVocab (Err error) ->
        (model, Cmd.none)
    GotVocab (Ok listresult) ->
        ({model | vocabs = listresult } , Cmd.none)
    GotNewVocab (Err error) ->
        (model, Cmd.none)
    GotNewVocab (Ok result) ->
        ({model | vocabs = List.append model.vocabs [result], new = {id = 0, english = "", vietnamese = ""}  } , Cmd.none)





changeEnglish: Vocab -> String -> Vocab
changeEnglish vocab text = 
    {
      vocab | english = text
    }




changeVietnamese: Vocab -> String -> Vocab
changeVietnamese vocab text = 
    {
      vocab | vietnamese = text
    }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
  { title = "Vocabulary Store"
  , body =
      [ 
        div [class "app"]
        [
          div [class "container"]
            [
              div [class "row justify-content-md-center"] 
              [
                div [class "col-md-6"]
                [
                  div [class "submit-form"]
                      [
                        div [ class "row"]
                            [
                              div [ class "form-group col-md-12"] 
                              [
                                label [for "english"] [text "English"],
                                input [type_ "text",  class "form-control",  id "email",placeholder "Enter english", Html.Attributes.value model.new.english, onInput ChangeEnglish] []
                              ],
                              div [ class "form-group col-md-12"] 
                              [
                                label [for "vietnamese"] [text "Vietnamese"],
                                input [type_ "text",  class "form-control",  id "vietnamese", Html.Attributes.value model.new.vietnamese, placeholder "Enter vietnamese", onInput ChangeVietnamese] []
                              ],
                              div [ class "col-md-12"]
                              [
                                div [class "button"] 
                                    [
                                      button [type_ "submit", class "btn btn-primary", onClick SubmitVocab ] [text "Submit"]
                                    ]
                              ] 
                            ]
                      ]
                ]
                , div [class "col-md-6"]
                    [
                      listview model.vocabs
                    ]
              ]
            ]
        ]
      ]
  }


listview: List Vocab -> Html Msg 
listview vocabs = 
    ul [class "list-vocab"]  (List.map renderview vocabs)


renderview:  Vocab -> Html Msg
renderview vocab =
    li [] [
      span [] [text vocab.english]
    , span [] [text vocab.vietnamese]
    , button [class "delete-vocab", onClick (DeleteVocab vocab.id)] 
      [
        i [class  "fas fa-times-circle"] []
      ]
    ]
