$(document).ready(function(){
  console.log("Document ready. Starting phase ...");
  var phase = new Phase({
    port: 4006,
    host: "127.0.0.1",
    parent: $('body')[0]
  });
});

function Phase(config){
  // Properties
  this.port = config.port;
  this.host = config.host;
  this.parent = config.parent;
  this.logs = [];
  this.buffers = Object.create(null);
  this.lastlog = window.performance.now();
  this.container = $('<div class="phase-container"></div>');

  // Init
  $(this.parent).append(this.container);
  this.connect();
}

Phase.prototype.connect = function(){
  var self = this;
  var loc = window.location;
  var new_uri = "ws://" + this.host + ":" + this.port;
  var conn = this.conn = new WebSocket(new_uri);
  this.send = function(x){
    this.log("Send:",x);
    this.conn.send(x);
  }
  conn.onopen = function () {
    self.log("Connected to websocket!");
  };
  conn.onerror = function (error) {
    self.log('WebSocket error: ', error);
  };
  conn.onmessage = function (msg) {
    self.log('Receive:', msg);
    var event = JSON.parse(msg.data);
    self.log('Message:', event);
    var handler = self[event.tag];
    if (handler)
      handler.call(self,event);
    else
      throw "No handler for: " + event.tag;
  }
}

Phase.prototype.setWindowConfiguration = function(event){
  var buffers = [];
  this.buffersFromTree(event.tree, buffers);
  this.send(JSON.stringify({ tag: "get-buffers", names: buffers }));
}

Phase.prototype.setBuffers = function(event){
  this.log("TODO: setBuffers: ", event.buffers);
  var buffers = event.buffers;
  for (var i = 0; i < buffers.length; i++) {
    this.buffers[buffers[i].name] = buffers[i];
  }
  this.log("Buffers:", this.buffers);
}

Phase.prototype.buffersFromTree = function(tree, out){
  var self = this;
  if (tree.tag == "split") {
    self.buffersFromSplit(tree, out);
  } else if (tree.tag == "window") {
    out.push(tree.buffer);
  }
}

Phase.prototype.buffersFromSplit = function(split, out){
  for (var i = 0, len = split.windows.length; i < len; i++) {
    this.buffersFromTree(split.windows[i],out);
  }
};

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
