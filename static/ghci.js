$(function() {

  /* ************************************ */
  /* Server... */
  /* ************************************ */

  function sendToServer(content, callback) {
    $.ajax({
      type: 'POST',
      url: "/ghci",
      data: {'data' : content},
      success: callback
    });
  }

  function startsWith(bigger, smaller) {
    if (smaller.length > bigger) { return false; }
    return bigger.slice(0, smaller.length) === smaller;
  }

  function htmlDecode(input){
    var e = document.createElement('div');
    e.innerHTML = input;
    return e.childNodes.length === 0 ? "" : e.childNodes[0].nodeValue;
  }

  var highlighter = new Sunlight.Highlighter();

  function highlight(input) {
    var out;

    try {
      context = highlighter.highlight(content['msg'], "haskell");
    } catch (e) {
      context = undefined;
    }

    if (context == undefined || context.getNodes() == undefined) {
      out = $("<pre></pre>");
      out.text(content['msg']);
      out.html(out.html().replace(/\n/g,'<br/>'));
    } else {
      out = document.createElement("pre");
      var nodes = context.getNodes();
      for (var i = 0; i < nodes.length; i++) {
        var n = $(nodes[i]);
        n.html(n.html().replace(/\n/g, '<br/>'));
        out.appendChild(nodes[i]);
      }
    }

    return out;
  }

  function decodeResponse(odata, cn) {
    content = $.parseJSON(odata);
    var out;

    if (content['type'] == "error") {
      out = [{
        msg: htmlDecode(content['msg'][2]),
        className: "output-error"
      }];
    } else if (content['type'] == "doc") {
      out = [{
        msg: htmlDecode(content['msg']),
        className: "output-doc"
      }];
    } else if (content['type'] == "value") {
      var context;
      if (content['msg'] == undefined || content['msg'].length <= 0) {
        return undefined;
      }
      out = highlight(content['msg']);
      if (content['prompt'] && content['prompt'].length > 0) {
        cn.promptLabel = content['prompt'] + ' ';
      }
    } else {
      out = [{
        msg: htmlDecode(odata),
        className: "output-doc"
      }];
    }

    return out;
  }

  /* ************************************ */
  /* Timers... */
  /* ************************************ */

  // Keep-Alive (once every 10 seconds)
  setInterval(function () {
    $.post('/ghci');
  }, 10 * 1000);

  /* ************************************ */
  /* JQuery Console... */
  /* ************************************ */

  var cn = $("#console").console({
    welcomeMessage:'Welcome to GHC.IO!',
    promptLabel: 'Prelude> ',
    autofocus: true,
    animateScroll: false,
    promptHistory: true,
    blink: true,

    commandValidate:function(line) {
      if (line == "") return false;
      else return true;
    },

    commandHandle: function(line, report) {
      sendToServer(line, function(data){
        report(decodeResponse(data, cn));
        $(".jquery-console-cursor").scrollintoview();
      });
    },

    charInsertTrigger: function(keycode,line) {
      $(".jquery-console-cursor").scrollintoview();
      return true;
    }
  });

  $(".brand").click(function() {
    $("#console").click();
  });

});
