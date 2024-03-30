# Connection 


## Player side modules
- Script to start all player side modules
    - Prompt for a name 
    - start the input sender
    - start position listener 
- Erlang Input sender
    - Will send hello to the server to establish identity and connection
- Erlang Position listener + Python Display element 
    - Will send hello to server to establish identity and connection
    - spin up a pygame display for the user 

## Server side
- Erlang element to handle incoming messages from players 
- Erlang element to send updated game state to players
- Keep track of the game state + other players 