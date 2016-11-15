$(function(){
  var debug = false;
  var oldline = 0;
  var conn = new WebSocket('ws://127.0.0.1:2016/');
  conn.onopen = function () {
    conn.send('Ping');
  };
  conn.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  };
  conn.onmessage = function (e) {
    var reply = JSON.parse(e.data);
    // console.log(reply);
    if (reply['type'] == 'point') {
      var buffer = document.getElementById('buffer');
      var xs = buffer.children;
      var line = reply.line;
      if (xs[oldline-1] && oldline != line) {
        xs[oldline-1].innerHTML = xs[oldline-1].innerHTML.replace(/<i>(.*?)<\/i>/,'$1');
      }
      oldline = line;
      for (var i = 0; i < xs.length; i++) {
        if (i+1 == line) {
          var curcol = 0;
          var html = xs[i].innerHTML;
          var column = reply.column;
          xs[i].innerHTML =
            html.replace(/<i>(.*?)<\/i>/,'$1').replace(
              /<span([^>]*)>([^<]+)<\/span>/g,
            function(orig,attrs,x0){
              var x = x0;
              if (curcol + x0.length > column && curcol <= column) {
                x = x.substring(0, column - curcol) + '<i>' +
                x.substring(column - curcol,(column - curcol)+1) + '</i>' +
                x.substring((column - curcol)+1);
                curcol += x0.length;
                return '<span' + attrs + '>' + x + '</span>';
              } else {
                curcol += x0.length;
                return orig;
              }
            })
          break;
        }
      }
    } else if (reply['type'] == 'faces') {
      var faces = reply.faces;
      // console.log(faces);
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
      for (var i = 0; i < len; i++) {
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
      }
    }
  };
});
