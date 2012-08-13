$(function() {
  var BACKSPACE = 8;
  var TAB       = 9;
  var ENTER     = 13;

  var ticks             = 0;
  var autocomplete_info = {};
  var type_info         = {};

  var showing_calltips  = false;
  var calltips_word     = "";

  var keywords = ["as", "case,", "class", "data", "default", "deriving", "deriving", "do", "forall", "foreign", "hiding", "if,", "import", "infix,", "instance", "let", "mdo", "module", "newtype", "proc", "qualified", "rec", "type", "type", "type", "where", "then", "else", "infixl", "infixr", "in" ];

  var blink_cursor = function() {
    ++ticks;
    if (ticks % 3 === 0) {
      /* We don't want to set display: none since then we wouldn't be able to
       * position the autocomplete at the cursor when the cursor was not
       * visible. So we blink the cursor in this way instead. */
      if ($("#cursor").css('color') == 'rgb(255, 255, 255)'){
        $("#cursor").css({'color': 'black'});
      } else {
        $("#cursor").css({'color': 'white'});
      }
    }
  }

  var move_calltips_box = function() {
    var cursor_position = $("#cursor").offset();
    cursor_position.top += 18;
    $("#calltips").css(cursor_position);
  }

  var move_autocomplete = function() {
    var cursor_position = $("#cursor").offset();
    $("#autocomplete").css(cursor_position);
  }

  var dict_to_list = function(dict) {
    var result = [];
    for (var x in dict) {
      result.push(x);
    }
    return result;
  }

  var uid        = 0;
  var colors     = ["#faa", "#afa", "#aaf", "#ffa", "#aff", "#faf", "#ddd"];
  var color_idx  = 0;
  var color_dict = {};

  var get_uid = function() {
    return uid++;
  }

  var get_color = function(type) {
    if (!(type in color_dict)) {
      color_dict[type] = colors[++color_idx % colors.length];
    }
    return color_dict[type];
  }

  var surround_word = function(word) {
    var $elem = $("<span;id='" + get_uid() + "'>" + word + "</span>");
    var my_value = $.trim(word);

    if (my_value in type_info){
      $elem.css('color', get_color(type_info[my_value][0]));

      $elem.mousedown(function(){
        var my_position = $elem.offset();
        my_position.top += 18;
        if (my_value in type_info) {
          var vals = type_info[my_value];
          var annotation = my_value + " :: " + vals[0] + " = " + vals[1];
          var elem = $("#typeannotations").css(my_position).show().html(annotation);
        }
      }).mouseleave(function(){
        $("#typeannotations").hide();
      });

    } else if (keywords.indexOf(word) != -1) {
      return $elem;
    } else {
      var my_type = undefined;

      if (my_value.match(/[0-9]+/)) {
        my_type = "Integer";
      } else if (my_value.match(/[0-9]+\.[0-9]+/)) {
        my_type = "Double";
      } else if (my_value.match(/'.*'/)) {
        my_type = "Char";
      } else if (my_value in autocomplete_info) {
        my_type = autocomplete_info[my_value];
      } else {
        return $elem; //we can't figure out what type it is.
      }

      $elem.css('color', get_color(my_type));

      $elem.mousedown(function(){
        var my_position = $elem.offset();
        my_position.top += 18;
        var elem = $("#typeannotations").css(my_position).show().html(my_type);
      }).mouseleave(function(){
        $("#typeannotations").hide();
      });
    }

    return $elem;
  }


  var add_colors = function(element) {
    var lines = element.html().split('\n');

    element.html('');

    for (var i = 0; i < lines.length; i++) {
      var words = lines[i].split(' ');

      for (var j = 0; j < words.length; j++) {
        var word = $.trim(words[j]) + " ";

        element.append(surround_word(word));
      }
      if (i != lines.length - 1) {
        element.append($("<br></br>"));
      }
    }
  }

  function htmlDecode(input){
    var e = document.createElement('div');
    e.innerHTML = input;
    return e.childNodes.length === 0 ? "" : e.childNodes[0].nodeValue;
  }

  /* 
   * Generic way to add a new line to the console. `yours` indicates whether it
   * "belongs to you" - i.e., whether the cursor should be on it or not. 
   */
  var add_line = function(content, yours) {
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
      if (starts_with(content, "ERR: ")) {
        var tuple = eval(content.slice(5));
        content = tuple[2];
        $new_elem.children("#content").css({'color' : 'red'});
        $new_elem.children("#prompt").replaceWith($("<img src='static/fail.png'></img>"));
      } else if(starts_with(content, "DOC")){
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

  /* sends `content` as an ajax request to `link`, calling `callback`
   * when we receive data from the request. */
  var send_to_server = function(content, callback) {
    var strip_and_callback = function(content) {
      var initial_crap = "";
      var final_crap   = "";

      var stripped_content = content.slice(initial_crap.length, content.length - final_crap.length);
      callback(stripped_content);
    }

    $.ajax({
      type: 'POST',
      url: "/ghci",
      data: {'data' : content},
      success: strip_and_callback
    });
  }

  /* TODO: Should also be called when user loads in new modules. */
  var populate_autocomplete = function() {
    send_to_server(":browse", function(data){
      var lines = data.split("\n");
      for (var i = 0; i < lines.length; i++){
        var line = lines[i];
        //autocomplete_info["map"] = "(a -> b) -> [a] -> [b]"

        var browse_info = /(.+) :: (.+)/g;
        var match = browse_info.exec(line);
        if (match === null) continue;

        autocomplete_info[match[1]] = match[2];
      }
    });
  }

  var strip_libs = function(str) {
    if (str.indexOf("ERR") != -1) {
      return str;
    }
    if (str.indexOf("DOC") != -1) {
      return str;
    }
    return str.slice(str.indexOf("&gt;") + 4);
  }

  var add_output_line = function(content) {
    send_to_server(content, function(data){
      add_line(content, true);
      add_line(strip_libs(data), false);
      window.scrollTo(0, document.body.scrollHeight);
      setTimeout(function(){
        window.scrollTo(0, document.body.scrollHeight);
      }, 300);
    });
  }

  /* Call to insert new output (presumably from ghci) into the console. */
  var receive = function(output) {
    add_line(output, false);
  }

  var starts_with = function(bigger, smaller) {
    if (smaller.length > bigger) return false;
    return bigger.slice(0, smaller.length) === smaller;
  }

  var current_word = function() {
    var current_line = $("#active #content").html();
    return current_line.slice(current_line.lastIndexOf(" ") + 1);
  }

  var show_autocomplete = function(list, is_calltips) {
    var autocomplete_visible = false;
    if (showing_calltips) {
      $("#autocomplete").hide();
      return;
    }

    $("#autocomplete").children().remove();

    //TODO: Sort alphabetically.
    for (var i = 0; i < list.length; i++) {
      if (starts_with(list[i], current_word())) {
        autocomplete_visible = true;

        $("#autocomplete").append($("<li><span class='completed'>" + list[i].slice(0, current_word().length) + "</span>" + list[i].slice(current_word().length, list[i].length) + "</li>"));
      }
    }

    if (current_word() === "") {
      autocomplete_visible = false;
    }

    if (autocomplete_visible) {
      $("#autocomplete").show();
    } else {
      $("#autocomplete").hide();
    }

    if (autocomplete_visible) {
      move_autocomplete();
    }
  }

  function getSelText() {
    var txt = '';
    if (window.getSelection) {
      txt = window.getSelection();
    } else if (document.getSelection) {
      txt = document.getSelection();
    } else if (document.selection) {
      txt = document.selection.createRange().text;
    } else return;
    return htmlDecode(txt);
  }


  var fill_sidebar = function(bindings) {
    $("#sidelist").children().remove();
    for (var name in bindings) {
      var info = name + " :: " + bindings[name][0] + " = " + bindings[name][1];
      $("#sidelist").append($("<li>" + info + "</li>"))
    }
  }

  var do_type_annotations = function(elem) {
    send_to_server(":show bindings\n", function(data){
      var lines = data.split("\n");
      type_info = {};
      lines[0] = strip_libs(lines[0]);
      for (var i = 0; i < lines.length; i++){
        var line = lines[i];
        var browse_info = /(.+) :: (.+) = (.+)/g;
        var match = browse_info.exec(line);
        if (match === null) continue;
        type_info[match[1]] = [match[2], match[3]];
      }

      add_colors(elem);
      fill_sidebar(type_info);
    });
  }

  var autocomplete = function() {
    var completed_word = $($("#autocomplete :first-child")[0]).text();
    var rest = completed_word.slice(current_word().length);
    $("#active #content").html($("#active #content").html() + rest + " ");

    showing_calltips = true;
    calltips_word = $.trim(completed_word);
    move_calltips_box();
  }

  /* Call to send output "through the socket" (which is really not a socket at
   * all and is instead just AJAX. */
  var add_to_console = function(value) {
    var $content = $("#active #content");
    var old_txt = $content.text();
    var new_txt;

    if (value === ENTER) {
      add_output_line(old_txt);
      showing_calltips = false;
      return;
    } else if (value === BACKSPACE) {
      if (old_txt.length == 0) {
        return;
      }

      new_txt = old_txt.slice(0, old_txt.length - 1);
    } else if (value === TAB) { 
      autocomplete();
    } else {
      new_txt = old_txt + value;
    }

    $content.text(new_txt);
    move_autocomplete();
  }

  /* Backspace cannot be detected by a keypress event. */
  $(document).bind('keydown', function(e) {
    if (e.which === BACKSPACE) {
      add_to_console(BACKSPACE);
		return false;
    } else if (e.which === ENTER) {
      add_to_console(ENTER);
		return false;
    } else if (e.which === TAB) {
      autocomplete();
		return false;
    }
  });

  /* Handle key input */
  $(document).bind('keypress', function(e) {
    if (!(e.which === BACKSPACE || e.which === ENTER || e.which === TAB || e.metaKey || e.ctrlKey)) {
      key = String.fromCharCode(e.which);
      add_to_console(key);
      return false;
    }
  });

  var show_calltips = function() {
    if (!showing_calltips) {
      $("#calltips").hide();
      return;
    }
    $("#calltips").show();
    $("#calltips").html(autocomplete_info[calltips_word]);
    
  }

  setInterval(function(){
    show_autocomplete(dict_to_list(autocomplete_info), false);
    show_calltips();
    blink_cursor();
  }, 100);

  function clearSelection() {
    if ( document.selection ) {
        document.selection.empty();
    } else if ( window.getSelection ) {
        window.getSelection().removeAllRanges();
    }
  }

  // $(document).mouseup(function() {
  //   var selection = getSelText();
  //   send_to_server(":t " + selection, function(result) {
  //     if (result.indexOf("ERR") != -1) return;
  //     result = strip_libs(result);

  //     var my_position = $("#cursor").offset();
  //     my_position.top += 18;

  //     var annotation = result;
  //     var elem = $("#typeannotations").css(my_position).show().html(annotation);

  //     $(document).mousedown(function(){
  //       $("#typeannotations").hide();
  //       //TODO: Destroy this function
  //     });

  //   });
  //   clearSelection();
  // });

  function initialize() {
    // populate_autocomplete();
  }

  // Keep-Alive
  setInterval(function () {
    $.post('/ghci');
  }, 10 * 1000);

  initialize();
});

