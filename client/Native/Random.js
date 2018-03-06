var _user$project$Native_Random = {
  random: function(a) {
    var array = new Uint32Array(1);
    window.crypto.getRandomValues(array);
    return array[0];
  }
}
