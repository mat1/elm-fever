const WebSocket = require('ws');

const port = 8080
const wss = new WebSocket.Server({ port: port });

const players = [];

console.log("Start server on port:" + port);

// Broadcast to all.
wss.broadcast = function broadcast(data) {
  wss.clients.forEach(function each(client) {
    if (client.readyState === WebSocket.OPEN) {
      client.send(data);
    }
  });
};

wss.on('connection', function connection(ws) {
  console.log("New connection")
  ws.on('message', function incoming(data) {
    console.log("Received message: " + data);
    var command = JSON.parse(data);
    console.log(command);

    if (command.name === "REGISTER") {
      console.log ("REGISTER");
      players.push(command.player);
      broadcast(players);
    }
  });
});

function broadcast(data) {
  // Broadcast to everyone else.
  wss.clients.forEach(function each(client) {
    if (client.readyState === WebSocket.OPEN) {
      client.send(JSON.stringify(data));
    }
  });
}
