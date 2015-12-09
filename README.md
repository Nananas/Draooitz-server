- check port
- put in android
- test echo

# observer on node
$ erl -sname observer -setcookie server

TODO

- [ ] logger
- [ ] logout
- [?] at startup, clean up process id's of players (dets)
- [x] Rooms list
    - [x] refresh / push update from server
- [x] Room
    - [x] Floatingactionbutton (x2)
    - [x] back button in titlebar
    - [ ] Room name in titlebar
    - [ ] push people count to clients
    - [ ] when last person leaves, quit the room after a certain time (& send message)
- [.] docs
