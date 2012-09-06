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

  function decodeResponse(content) {
    var out;

    if (startsWith(content, "ERR: ")) {
      var tuple = eval(content.slice(5));
      out = [{
        msg: htmlDecode(tuple[2]),
        className: "output-error"
      }];
    } else if (startsWith(content, "DOC")){
      out = [{
        msg: htmlDecode(content.slice(3)),
        className: "output-doc"
      }];
    } else {
      out = $('<code class="prettyprint lang-hs"></code>');
      out.text(content);
      out.html(out.html().replace(/\n/g,'<br/>'));
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

  $("#console").console({
    welcomeMessage:'Welcome to GHC.IO!',
    promptLabel: '>  ',
    autofocus: true,
    animateScroll: true,
    promptHistory: true,

    commandValidate:function(line) {
      if (line == "") return false;
      else return true;
    },

    commandHandle: function(line, report) {
      sendToServer(line, function(data){
        report(decodeResponse(data));
        prettyPrint(); // prettyPrintOne may be better here...
        window.scrollTo(0, document.body.scrollHeight);
      });
    },

    charInsertTrigger: function(keycode,line) {
      window.scrollTo(0, document.body.scrollHeight);
      return true;
    }
  });

});
