// TODO: store LRU info, and limit the size to maybe 50MB.
let store = {};

var _user$project$Native_Cache = {
  set : F2(function(key, val) {
    store[key] = val;
    return _elm_lang$core$Maybe$Nothing;
  }),

  get : function(key) {
    let result = store[key];
    if (result == null) {
      return _elm_lang$core$Maybe$Nothing;
    } else {
      return _elm_lang$core$Maybe$Just(result);
    }
  }
};
