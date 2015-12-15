# Draooitz Server
This repo contains the code for the server for an android game, written in erlang. It is build on top of the Cowboy server framework, and uses websockets for full-duplex communication. 

## Socket handler
The socket handler is the starting point of the application. This is where the websocket connections are created. Each websocket is handled by a separate process. A player process is created if a correct combination of username and password is provided. This process id is then stored in the state of the websocket process. This variable is accessible by each subsequent call to and from the WS.

## Player process
A player process contains a player name, the current room the player is in and the WS process id to be able to send messages back to the client, such as notifications. The player process is also responsible for adding itself to and removing itself from a room by sending the corresponding messages. 

Each process is added to the supervisor tree.

## Room process
A room process contains a name and a list with player process id's to make it easy to broadcast any message to all connected players in that room. By sending a message to a room process id, the current player count can also be accessed. 

Each process is added to the supervisor tree.

A room process is automatically destroyed if no message has been received for a certain time (10 minutes).

## Players table & auth
The players module keeps track of the passwords of each created player and every connected, online player by storing its player process id. As such, a broadcast to every player is as simple as sending messages to each of the online player id's. Each username-password combination is stored in a dets, which is stored on the disk instead of the RAM, making it non-volitale. High performance is not (yet?) needed. The players module also contains helper functions for the socket handler and the player processes.

The ets and dets table are both registered and started from within the survivor process.

## Rooms table 
The rooms table is somewhat similar to the players table. The rooms module mostly contains helper functions for the socket handler and the room processes.

The rooms ets is registered and started from within the survivor process.

# API
The following sketches depicts the possible client-server and server-client messages.

## client-server

```
State: [Not logged in]
    - LOGIN:<username>,<password>
        ok -> go to [Logged in]
        not_ok -> go to [Not logged in]

State: [Logged in]
    - LOGIN:<username>,<password>
        ok -> go to [Logged in]
        not_ok -> go to [Not logged in]

    - GETROOMLIST:ALL
    - NEWROOM:<roomname>
    - ENTERROOM:<roomname>
    - LEAVEROOM
    - DRAWPATH:<json path data>
    - CLEARDRAWING
```

### Drawpath class
The drawpath json message contains the following data:
```
Integer[] Xx;   // array of x-positions
Integer[] Yy;   // array of y-positions
int c;      // color
```
