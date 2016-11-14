$(function(){
  var conn = new WebSocket('ws://127.0.0.1:2016/');
  conn.onopen = function () {
    conn.send('Ping');
  };
  conn.onerror = function (error) {
    console.log('WebSocket Error ' + error);
  };
  conn.onmessage = function (e) {
    var reply = JSON.parse(e.data);
    if (reply['type'] == 'faces') {
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
      var buffer = document.getElementById('buffer');
      var start = reply.beg;
      var end = reply.end;
      console.log('%d - %d',start,end);
      var lines = reply.lines, len = lines.length;
      var anchor = null;
      for (var i = 0 ; i < buffer.children.length; i++) {
        if (i+1 >= start && i+1 <= end) {
          anchor = buffer.children[i+1];
          buffer.removeChild(buffer.children[i]);
        }
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
        buffer.insertBefore(el,anchor);
      }
    }
  };
});
