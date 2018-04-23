var _user$project$Native_Random = {
  random: function(a) {
    if (window && window.crypto) {
      var array = new Uint32Array(1);
      window.crypto.getRandomValues(array);
      return array[0];
    }
    else {
      // testing
      return Math.floor(Math.random() * Number.MAX_SAFE_INTEGER)
    }
  }
}
