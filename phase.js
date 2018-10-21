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
  this.tree = [];
  this.windows = Object.create(null);
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
  this.tree = event.tree;
  if (buffers.length > 0) {
    this.send(JSON.stringify({ tag: "get-buffers", names: buffers }));
  } else {
    this.applyWindowConfiguration();
  }
}

// Next:
//
// * Every window has a CodeMirror editor.
// * setWindowConfiguration calls linkDoc (on the Doc) + swapDoc
//   (on the CodeMirror) for every displayed buffer in each
//   window.
//
//
// > You can create new documents by calling the
// > CodeMirror.Doc(text: string, mode: Object, firstLineNumber:
// > ?number, lineSeparator: ?string) constructor. The last three
// > arguments are optional and can be used to set a mode for the
// > document, make it start at a line number other than 0, and set
// > a specific line separator respectively.
//
// And then:
//
// doc.linkedDoc(options: object) → Doc
//
// > Break the link between two documents. After calling this,
// > changes will no longer propagate between the documents, and, if
// > they had a shared history, the history will become separate.
//
// cm.swapDoc(doc: CodeMirror.Doc) → Doc
//
// > Attach a new document to the editor. Returns the old document,
// > which is now no longer associated with an editor.

Phase.prototype.applyWindowConfiguration = function(){
  var container = this.container;
  var dim = {
    width: container.width(),
    height: container.height(),
    left: 0,
    top: 0
  };
  this.log("Container dimensions", dim);
  var usedWindows = Object.create(null);
  this.applyTree(
    this.tree,
    dim,
    usedWindows);
  // Cleanup unused windows
  for (var key in this.windows) {
    if (!usedWindows[key]) {
      this.log("GC window", key);
      this.windows[key].dom.remove();
      delete this.windows[key];
    }
  }
}

Phase.prototype.applyTree = function(tree, dim, out){
  var self = this;
  if (tree.tag == "split") {
    self.applySplit(tree, dim, out);
  } else if (tree.tag == "window") {
    out[tree.key] = true;
    this.setWindow(tree, dim);
  }
}

Phase.prototype.setWindow = function(window, dim) {
  this.log("Make window for", window, "dimensions", dim);
  if (this.windows[window.key]) {
    this.log("Window",window.key,"already present.");
    this.windows[window.key].dom.css(dim);
  } else {
    this.windows[window.key] = window;
    this.log("New window",window);
    var buffer = this.buffers[window.buffer];
    var windowdom = $('<div class="phase-window"></div>');
    windowdom.css(dim).css('font-family','monospace').css('white-space','pre');
    windowdom.text(buffer.string);

    this.windows[window.key].dom = windowdom;

    this.container.append(windowdom);
  }
}

Phase.prototype.applySplit = function(split, dim, out){
  var width = split.vertical? dim.width : dim.width / split.windows.length;
  var height = split.vertical? dim.height / split.windows.length : dim.height;
  var left = dim.left;
  var top = dim.top;
  for (var i = 0, len = split.windows.length; i < len; i++) {
    this.applyTree(split.windows[i],{
      width: width,
      height: height,
      left: left,
      top: top
    }, out);
    if (split.vertical) top = top + height;
    else left = left + width;
  }
};

Phase.prototype.setBuffers = function(event){
  var buffers = event.buffers;
  for (var i = 0; i < buffers.length; i++) {
    this.log("New buffer:",buffers[i].name);
    var buffer = buffers[i];
    this.buffers[buffers[i].name] = buffer;
    buffer.doc = CodeMirror.Doc({
      text: buffer.string
    });
  }
  this.applyWindowConfiguration();
}

Phase.prototype.buffersFromTree = function(tree, out){
  var self = this;
  if (tree.tag == "split") {
    self.buffersFromSplit(tree, out);
  } else if (tree.tag == "window") {
    if (!self.buffers[tree.buffer]) {
      out.push(tree.buffer);
    }
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
