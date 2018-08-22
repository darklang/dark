var _user$project$Native_Random = {
  random: function(a) {
    const max = 2147483647;
    if (window && window.crypto) {
      var array = new Uint32Array(1);
      do {
        window.crypto.getRandomValues(array);
      }
      while (array[0] > max);
      return array[0];
    }
    else {
      // testing
      return Math.floor(Math.random() * max)
    }
  }
}
