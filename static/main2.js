$(function() {
  var BACKSPACE = 8;
  var TAB       = 9;
  var ENTER     = 13;

  /* Call to send output "through the socket" (which is really not a socket at
   * all and is instead just AJAX. */
  function addToConsole(value) {
    var $content = $("#active #content");
    var txt = $content.text();

    if (value === ENTER) {
      addOutputLine(txt);
      return;
    } else if (value === BACKSPACE) {
      if (txt.length == 0) {
        return;
      }
      txt = txt.slice(0, txt.length - 1);
    } else {
      txt = txt + value;
    }

    $content.text(txt);
  }

  /* 
   * Generic way to add a new line to the console. `yours` indicates whether it
   * "belongs to you" - i.e., whether the cursor should be on it or not. 
   */
  function addLine(content, yours) {
    var $old_elem = $("#active");
    var $new_elem = $old_elem.clone();

    $new_elem.css({'class': ''});

    if (yours) {
      $("#console").append($new_elem);
      $old_elem.attr("id", ""); //remove #active id.
      $old_elem.children("#cursor").remove();
      $old_elem.css({'class': 'input'});
      $old_elem.children("#prompt").html("$ ");

      $new_elem.children("#content").html("");

      // do_type_annotations($old_elem.children("#content"));
    } else {
      if (startsWith(content, "ERR: ")) {
        var tuple = eval(content.slice(5));
        content = htmlDecode(tuple[2]);
        $new_elem.children("#content").css({'color' : 'red'});
        $new_elem.children("#prompt").replaceWith($("<img src='static/fail.png'></img>"));
      } else if (startsWith(content, "DOC")){
        var tuple = htmlDecode(content.slice(3));
        content = tuple;
        $new_elem.children("#content").css({'color' : 'green'});
      } else {
        $new_elem.children("#prompt").replaceWith($("<img src='static/ok.png'></img>"));
      }


      $new_elem.attr("class", "output");
      $new_elem.children("#content").html(content);
      $new_elem.attr("id", ""); //remove #active id.
      $new_elem.children("#cursor").remove();
      $new_elem.insertBefore($("#console #active"));

      // do_type_annotations($new_elem.children("#content"));
    }
  }

  function addOutputLine(content) {
    sendToServer(content, function(data){
      addLine(content, true);
      addLine(data, false);
      window.scrollTo(0, document.body.scrollHeight);
      setTimeout(function(){
        window.scrollTo(0, document.body.scrollHeight);
      }, 300);
    });
  }

  /* sends `content` as an ajax request to `link`, calling `callback`
   * when we receive data from the request. */
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


  /* ************************************ */
  /* Keyboard Event Handlers... */
  /* ************************************ */

  /* Handle key input */
  $(document).bind('keypress', function(e) {
    if (!(e.which === BACKSPACE || e.which === ENTER || e.which === TAB || e.metaKey || e.ctrlKey)) {
      key = String.fromCharCode(e.which);
      addToConsole(key);
      return false;
    }
  });

  /* Backspace cannot be detected by a keypress event. */
  $(document).bind('keydown', function(e) {
    if (e.which === BACKSPACE) {
      addToConsole(BACKSPACE);
      return false;
    } else if (e.which === ENTER) {
      addToConsole(ENTER);
      return false;
    }
  });


  /* ************************************ */
  /* Timers... */
  /* ************************************ */

  // Keep-Alive (once every 10 seconds)
  setInterval(function () {
    $.post('/ghci');
  }, 10 * 1000);

  // Blink the cursor...
  setInterval(function(){
    /* We don't want to set display: none since then we wouldn't be able to
     * position the autocomplete at the cursor when the cursor was not
     * visible. So we blink the cursor in this way instead. */
    if ($("#cursor").css('color') == 'rgb(255, 255, 255)'){
      $("#cursor").css({'color': 'black'});
    } else {
      $("#cursor").css({'color': 'white'});
    }
  }, 500);

});

