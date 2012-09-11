$(function() {

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
      out = $('<code class="prettyprint lang-hs"></code>');
      // out = $('<code class="sunlight-highlight-haskell"></code>');
      out.text(content['msg']);
      out.html(out.html().replace(/\n/g,'<br/>'));
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
    animateScroll: true,
    promptHistory: true,

    commandValidate:function(line) {
      if (line == "") return false;
      else return true;
    },

    commandHandle: function(line, report) {
      sendToServer(line, function(data){
        report(decodeResponse(data, cn));
        prettyPrint(); // prettyPrintOne may be better here...
        // Sunlight.highlightAll();
        window.scrollTo(0, document.body.scrollHeight);
      });
    },

    charInsertTrigger: function(keycode,line) {
      window.scrollTo(0, document.body.scrollHeight);
      return true;
    }
  });

});
