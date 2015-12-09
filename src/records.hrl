-record (room, {
	name 			:: bitstring()
	, pid 			:: pid()
	, data=clean 	:: bitstring()	% not used?
}).

-record (player, {
	name		:: bitstring()
	, pass=none	:: bitstring()
}).