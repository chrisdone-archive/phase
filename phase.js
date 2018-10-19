

$(document).ready(function(){
  console.log("Document ready. Starting phase ...");
  var phase = new Phase({
    port: 4006,
    host: "127.0.0.1",
    parent: $('body')[0]
  });
});

function Phase(config){
  this.port = config.port;
  this.host = config.host;
  this.parent = config.parent;
  this.logs = [];
  this.lastlog = window.performance.now();
  this.container = $('<div class="phase-container"></div>');
  $(this.parent).append(this.container);
  this.connect();
}

Phase.prototype.connect = function(){
  var self = this;
  var loc = window.location;
  var new_uri = "ws://" + this.host + ":" + this.port;
  var conn = this.conn = new WebSocket(new_uri);
  this.send = function(x){
    this.log("WebSocket send: ",x);
    this.conn.send(x);
  }
  conn.onopen = function () {
    self.log("Connected to websocket!");
    self.log("Sending ping!");
    self.send("Hello, World!");
  };
  conn.onerror = function (error) {
    self.log('WebSocket error: ', error);
  };
  conn.onmessage = function (msg) {
    self.log('WebSocket message: ', msg);
  }
}

Phase.prototype.log = function(){
  var self = this;
  var now = window.performance.now();
  var diff = now - self.lastlog;
  var args = Array.prototype.slice.call(arguments);
  args.unshift("Phase: ");
  args.unshift(diff.toFixed(3) + "ms");
  self.logs.push(args);
  clearTimeout(self.logger);
  self.logger = setTimeout(function(){ self.flushlog(); }, 500);
}

Phase.prototype.flushlog = function(){
  var logs = this.logs, len = logs.length;
  this.logs = [];
  for (var i = 0; i < len; i++)
    console.log.apply(console, logs[i]);
}
