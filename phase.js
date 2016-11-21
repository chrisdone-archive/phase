// Globals

var cursor = document.createElement('i');
var point = {};

var debug = false;
var oldline = 0;

$(function(){

  var loc = window.location;
  var new_uri = "ws://" + loc.host + "/w";
  var conn = new WebSocket(new_uri);
  conn.onopen = function () {
  };
  conn.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  };
  conn.onmessage = function (e) {
    var reply = JSON.parse(e.data);
    // console.log(reply);
    if (reply['type'] == 'frame-parameters') {
      if (reply.background_color)
        $(document.body).css('background-color',reply.background_color);
      if (reply.foreground_color)
        $(document.body).css('color',reply.foreground_color);
      if (reply.cursor_color) {
        cursor.style.backgroundColor = reply.cursor_color;
        cursor.style.color = reply.background_color;
      }
    } else if (reply['type'] == 'point') {
      var buffer = document.getElementById('buffer');
      var xs = buffer.children;
      var line = reply.line;
      point = reply;
      restoreCursor();
      for (var i = 0; i < xs.length; i++) {
        if (i+1 == line) {
          var root = xs[i];
          insertCursor(root, reply.column);
        }
      }
    } else if (reply['type'] == 'faces') {
      var faces = reply.faces;
      var rules = [];
      for (var i =0; i < faces.length; i+=2) {
        rules.push('.face-' + faces[i] + ' { color: ' + faces[i+1] + '; }');
      }
      var style = document.createElement('style');
      style.innerHTML = rules.join('\n');
      document.body.appendChild(style);
    } else if (reply['type'] == 'region') {
      if(debug) console.log(reply);
      var buffer = document.getElementById('buffer');
      var start = reply.beg;
      var end = reply.end;
      if (debug) console.log('From line %d to line %d',start,end);
      var lines = reply.lines, len = lines.length;
      var anchor = null;
      var offset = 0;
      for (var i = 0 ; i < buffer.children.length; i++) {
        var j = i + offset;
        if (j+1 >= start && j+1 <= end) {
          if (debug) console.log("Deleting line %d (%o)",j+1,buffer.children[i].innerText);
          anchor = buffer.children[i+1];
          buffer.removeChild(buffer.children[i]);
          i--;
          offset++;
        }
        if (j+1>end) break;
      }
      for (var i = 0, line_number = start; i < len; i++, line_number++) {
        var el = document.createElement('div');
        var line = lines[i];
        var text = line.text;
        if (!text) {
          el.innerHTML = '&nbsp;';
        } else if (line.faces) {
          var faces = line.faces;
          for (var j = 0; j < faces.length; j++) {
            var start = faces[j]; j++;
            var end = faces[j]; j++;
            var face = faces[j];
            var e = document.createElement('span');
            e.innerText = text.substring(start,end);
            if(face) e.className = 'face-' + face;
            el.appendChild(e);
          }
        } else {
          el.innerText = text;
        }
        if (anchor){
          if (debug) console.log("Inserting line %o before %o", text, anchor.innerText);
        } else {
          if (debug) console.log("Appending line %o", text);
        }
        buffer.insertBefore(el,anchor);
        if (line_number == point.line) {
          var curcol = point.column < line.text.length? point.column : line.text.length;
          insertCursor(el, curcol);
        }
      }
    }
  };
});

// Restore the content at the current position of the cursor.
function restoreCursor() {
  if (cursor.parentNode && cursor.className == 'phase-cursor') {
    cursor.parentNode.insertBefore(cursor.firstChild, cursor);
  }
}

// Insert a cursor in the given line.
function insertCursor(root, targetColumn) {
  for (var currentColumn = 0, currentNode = root; currentNode;) {
    if (currentNode.nodeType === Node.TEXT_NODE) {
      var text = currentNode.textContent;
      if (currentColumn + text.length > targetColumn) {
        var parent = currentNode.parentNode;
        var relativeColumn = targetColumn - currentColumn;
        var before = document.createTextNode(text.substring(0,relativeColumn));
        var middle = text.substring(relativeColumn, relativeColumn + 1);
        var after = document.createTextNode(text.substring(relativeColumn + 1));
        cursor.innerText = middle;
        if (middle.length == 0) {
          cursor.className = 'phase-cursor-empty';
          cursor.innerHTML = '&nbsp;';
        }
        else
          cursor.className = 'phase-cursor';
        parent.insertBefore(after, currentNode);
        parent.insertBefore(cursor, after);
        parent.insertBefore(before, cursor);
        parent.removeChild(currentNode);
        return;
      } else {
        currentColumn += text.length;
        for (; !currentNode.nextSibling && currentNode.parentNode && currentNode.parentNode !== root; currentNode = currentNode.parentNode);
        currentNode = currentNode.nextSibling;
        continue;
      }
    } else {
      if (currentNode.firstChild) {
        currentNode = currentNode.firstChild;
        continue;
      } else if (currentNode.nextSibling) {
        currentNode = currentNode.nextSibling;
        continue;
      }
    }
    break;
  }
  cursor.innerHTML = '&nbsp;';
  cursor.className = 'phase-cursor-empty';
  root.appendChild(cursor);
}
