module UserData exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)


type alias UserData = 
    { country : String
    , display_name : String
    , email : String
    , id : String }
type Msg
    = GotUserData (Result Http.Error UserData)


    -- Gets User Data 



decodeUserData : Decoder UserData
decodeUserData = 
    Json.Decode.map4 UserData
        (field "country" string)
        (field "display_name" string)
        (field "email" string)
        (field "id" string)
        
getUserData : String -> Cmd Msg  
getUserData token =
  Http.request
    { method = "GET"
    , headers = [Http.header "Authorization" token]
    , url = "https://api.spotify.com/v1/me"
    , body = Http.emptyBody
    , expect = Http.expectJson GotUserData decodeUserData
    , timeout = Nothing
    , tracker = Nothing
    }
