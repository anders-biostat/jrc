//get current url
var url = window.location.href.split("/")[2];

// establish WebSocket link and handlers 
ws = new WebSocket( "ws://" + url + "/" ); //, "RLC-0" );
ws.addEventListener( "open", function(event) { 
   // ...
} ); 
ws.addEventListener( "message", function(event) {
	eval(event.data);
} );
ws.addEventListener( "close", function(event) { 
   window.close()
} );
