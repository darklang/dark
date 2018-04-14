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
  }
};
