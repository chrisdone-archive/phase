$(document).ready(function(){
  console.log("Document ready. Starting phase ...");
  var phase = new Phase({
    port: 80,
    host: window.location.hostname,
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
  this.cursorColor = "#ff0000";
  this.windows = Object.create(null);
  this.minibuffer = Object.create(null);
  this.buffers = Object.create(null);
  this.faces = Object.create(null);
  this.lastlog = window.performance.now();
  this.container = $('<div class="phase-container"></div>');
  this.styledom = $('<style></style>');

  // Init
  $(this.parent).append(this.container);
  this.container.append(this.styledom);
  this.connect();
}

Phase.prototype.connect = function(){
  var self = this;
  var loc = window.location;
  var new_uri = "ws://" + this.host + ":" + this.port + "/ws";
  var conn = this.conn = new WebSocket(new_uri);
  this.send = function(x){
    this.log("Send:",x);
    this.conn.send(x);
  }
  conn.onopen = function () {
    self.log("Connected to websocket!");
    this.send(JSON.stringify({ tag: "get-faces", names: ["default"] }));
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

Phase.prototype.jitLock = function(event){
  if (this.buffers[event.buffer]) {
    var buffer = this.buffers[event.buffer];
    var doc = buffer.doc;
    var missing = Object.create(null);
    var faces = [];
    this.applyProperties(doc, event.properties, missing, faces);
    var self = this;
    if (faces.length > 0)
      self.fetchFaces(faces);
  }
}

Phase.prototype.replaceRange = function(event){
  if (this.buffers[event.buffer]) {
    var buffer = this.buffers[event.buffer];
    var doc = buffer.doc;
    var pos1 = doc.posFromIndex(event.from-1);
    var pos2 = doc.posFromIndex(event.to-1);
    doc.replaceRange(event.replacement,pos1,pos2);
    var missing = Object.create(null);
    var faces = [];

    this.applyProperties(doc, event.properties, missing, faces);

    var self = this;
    if (faces.length > 0)
      self.fetchFaces(faces);
  }
}

Phase.prototype.killBuffer = function(event){
  if (this.buffers[event.buffer]) {
    delete this.buffers[event.buffer];
  }
}

Phase.prototype.setWindowPoints = function(event){
  for (var key in this.windows) {
    var sel = event.windows[key];
    var win = this.windows[key];
    if (sel) {
      win.cm.setSelection(sel, sel);
    }
  }
}

Phase.prototype.setWindowConfiguration = function(event){
  var buffers = [];
  this.minibuffer = event.minibuffer;
  if (!this.buffers[this.minibuffer.buffer])
    buffers.push(this.minibuffer.buffer)
  this.buffersFromTree(event.tree, buffers);
  this.tree = event.tree;
  if (buffers.length > 0) {
    this.send(JSON.stringify({ tag: "get-buffers", names: buffers }));
  } else {
    this.applyWindowConfiguration();
  }
}

Phase.prototype.applyWindowConfiguration = function(){
  var container = this.container;
  var usedWindows = Object.create(null);
  var minibufferHeight = 20;
  usedWindows[this.minibuffer.key] = this.minibuffer;
  this.log('set minibuffer',this.minibuffer);
  this.setWindow(this.minibuffer, {
    width: container.width(),
    height: minibufferHeight,
    left: 0,
    top: container.height() - minibufferHeight
  });
  var dim = {
    width: container.width(),
    height: container.height() - minibufferHeight,
    left: 0,
    top: 0
  };
  this.applyTree(
    this.tree,
    dim,
    usedWindows);
  // Cleanup unused windows
  for (var key in this.windows) {
    if (!usedWindows[key]) {
      this.windows[key].doc.unlinkDoc(this.windows[key].linkedDoc);
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
  if (this.windows[window.key]) {
    this.windows[window.key].dom.css(dim);
    var oldbuffer = this.windows[window.key].buffer;
    this.windows[window.key].buffer = window.buffer;
    if (oldbuffer == window.buffer) {
      this.windows[window.key].cm.refresh();
    } else {
      var buffer = this.buffers[window.buffer];
      var linkedDoc = buffer.doc.linkedDoc();
      var oldLinkedDoc = this.windows[window.key].cm.swapDoc(linkedDoc);
      this.windows[window.key].doc.unlinkDoc(oldLinkedDoc);
      this.windows[window.key].cm.setSelection(window.point, window.point);
    }
  } else {
    this.windows[window.key] = window;

    // Create DOM container
    var buffer = this.buffers[window.buffer];
    var windowdom = $('<div class="phase-window"></div>');
    windowdom.css(dim).css('font-family','monospace').css('white-space','pre');
    this.container.append(windowdom);

    // Create CodeMirror instance
    var cm = CodeMirror(windowdom[0], { readOnly: true });
    var linkedDoc = buffer.doc.linkedDoc();
    cm.swapDoc(linkedDoc);
    cm.setSelection(window.point, window.point);

    this.windows[window.key].cm = cm;
    this.windows[window.key].linkedDoc = linkedDoc;
    this.windows[window.key].doc = buffer.doc;
    this.windows[window.key].dom = windowdom;
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
  var missing = Object.create(null);
  var faces = [];
  for (var i = 0; i < buffers.length; i++) {
    var buffer = buffers[i];
    this.buffers[buffers[i].name] = buffer;
    buffer.doc = CodeMirror.Doc(buffers[i].string);
    this.applyProperties(buffer.doc, buffer.properties, missing, faces);
  }
  if (faces.length > 0)
    this.fetchFaces(faces);
  this.applyWindowConfiguration();
}

Phase.prototype.applyProperties = function(doc, props, missing, faces){
  if (props) {
    for (var p = 0, len = props.length; p < len; p+=3){
      if (props[p+2]) {
        var start = doc.posFromIndex(props[p]);
        var end = doc.posFromIndex(props[p+1]);
        var faceNames = props[p+2];
        if (typeof faceNames == 'string') faceNames = [faceNames];

        var classNames = [];

        for (var n = 0; n < faceNames.length; n++) {
          var faceName = faceNames[n];
          if (!this.faces[faceName] && !missing[faceName]) {
            missing[faceName] = true;
            faces.push(faceName);
          }
          classNames.push("face-" + faceName);
        }
        Phase.markTextReplacing(this,doc, start, end, { className: classNames.join(" "), shared: true });
      }
    }
  }
}

// Mark the text, but also adjust/delete any existing marks that
// overlap with the region.
Phase.markTextReplacing = function(self, doc, start, end, props){
  var marks = doc.findMarks(start, end), len = marks.length;
  if (len) {
    for (var i = 0; i < len; i++) {
      var marker = marks[i];
      var pos = marker.find();
      if (pos) {
        marker.clear(); // FIXME: TODO: re-create but out of the region.
      }
    }
  }
  //self.log("Marking text",JSON.stringify(doc.getRange(start,end,"\\n")),"with",props);
  doc.markText(start, end, props);
}

Phase.prototype.fetchFaces = function(faces){
  this.send(JSON.stringify({ tag: "get-faces", names: faces }));
}

Phase.prototype.setFaces = function(event){
  var faces = this.faces;
  Object.assign(faces, event.faces);
  var generated = [];
  for (var name in faces) {
    var props = [];
    for (var prop in faces[name]) {
      props.push(prop + ":" + faces[name][prop]);
    }
    var classSpec = ".face-" + name;
    if (name == "default") {
      generated.push(".phase-window > .CodeMirror{" + props.join(";") + "}");
      generated.push(classSpec + "{" + props.join(";") + "}");
    } else {
      generated.push(classSpec + "{" + props.join(";") + "}");
    }
  }
  var cursorColor = "div.CodeMirror .CodeMirror-cursor {background:" + this.cursorColor + "}";
  this.styledom.text(generated.join("\n") + cursorColor);
}

Phase.prototype.setCursorColor = function(event){
  this.cursorColor = event.color;
  this.setFaces({faces: []});
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
  if (false) {
    console.log.apply(console, args);
  } else {
    self.logs.push(args);
    clearTimeout(self.logger);
    self.logger = setTimeout(function(){ self.flushlog(); }, 500);
  }
}

Phase.prototype.flushlog = function(){
  var logs = this.logs, len = logs.length;
  this.logs = [];
  for (var i = 0; i < len; i++)
    console.log.apply(console, logs[i]);
}
