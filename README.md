This is just a little MusicApp that connects you to Spotify and is able to acccess the Spotify Data you request.

This project is written in ELM. So if you do not have ELM installed on your device go do that [here](https://guide.elm-lang.org/install/elm.html).

Then you want to clone this repository by navigating to your desired destination in your Terminal and cloen this code with `git clone https://github.com/xfronske/MusicApp_uni.git`



NOTE: Redirect URIs are only for localhost yet!

#### working: 
<ul>
  <li> Login to Spotify (AccessToken weiil be generated using PKCE)</li>
  <li> Show the Time via Elm </li>
</ul>

#### almost working:

<ul>
  <li> Logout from Spotify (sometimes you get redirected to the root folder of localhost:8000)</li>
  <li> Recieving the Token on ELM side is only working manually</li>
  <li> GetUserData 401 Unauthorized</li>
</ul>
