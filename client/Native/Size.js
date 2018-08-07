var _user$project$Native_Size = {
  /* Measure the size of the html string. In order to get the size, we
   * have to actually render it, which we do offscreen. */
  size: function(htmlStr) {
    var div = document.createElement('div');
    div.innerHTML = htmlStr;
    div.style.position = 'absolute';
    div.style.top = '-1000px';
    div.style.left = '-1000px';
    document.body.appendChild(div);
    var bb = div.getBoundingClientRect();
    document.body.removeChild(div);
    return bb;
  },

  positions: function(tlid) {
    let find = function (tl, nested) {
      let results = [];
      var selector;
      if (nested) {
        selector = ".blankOr.nested";
      } else {
        selector = ".blankOr:not(.nested)";
      }
      for (var bor of tl.querySelectorAll(selector)) {
        let bb = bor.getBoundingClientRect();
        var id = -1;
        for (var c of bor.classList) {
          let match = c.match(/id-(\d+)/);
          if (match) {
            id = parseInt(match[1]);
          }
        }
        if (id == -1) {
          console.log(bor.classList);
          throw ("Couldn't find id property");
        }
        var v = { x: bb.x
                , y: bb.y
                , width : bb.width
                , height: bb.height
                , top: bb.top
                , bottom: bb.bottom
                , left: bb.left
                , right: bb.right
                , id: id
                };
        results.push(v);
      }
      return _elm_lang$core$Native_List.fromArray(results);
    }

    let tls = document.getElementsByClassName("toplevel tl-" + tlid);
    if (tls.length != 1) {
      console.log(tls);
      throw ("Couldn't find toplevel: " + tlid);
    }

    let tl = tls[0];
    return { nested: find(tl, true)
           , atoms: find(tl, false)
           }
  }
};
