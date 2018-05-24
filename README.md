# pongserv

A Pong game server I wrote for a hack afternoon challenge to my teams
at Ticketmaster - the being to write clients that connect to the
server and play Pong against each other.

Have restarted it for my new team at Expedia, added an uberjar for the
server and a proper main method so it doesn't have to be started from
the REPL.

Connection is to port 6000 on the running server.

After entering a name, 'u' to move the paddle up, 'd' to move it down,
'x' to stop the paddle where it is.

Default is first to three, loser has their socket closed so someone
else can have a go.

To the victor goes the pizza.
