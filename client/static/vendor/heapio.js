(window.heapio = window.heapio || []),
  (heapio.load = function (e, t) {
    (window.heapio.appid = e), (window.heapio.config = t = t || {});
    var r = document.createElement("script");
    (r.type = "text/javascript"),
      (r.async = !0),
      (r.src = "https://cdn.heapanalytics.com/js/heap-" + e + ".js");
    var a = document.getElementsByTagName("script")[0];
    a.parentNode.insertBefore(r, a);
    for (
      var n = function (e) {
          return function () {
            heapio.push([e].concat(Array.prototype.slice.call(arguments, 0)));
          };
        },
        p = [
          "addEventProperties",
          "addUserProperties",
          "clearEventProperties",
          "identify",
          "resetIdentity",
          "removeEventProperty",
          "setEventProperties",
          "track",
          "unsetEventProperty",
        ],
        o = 0;
      o < p.length;
      o++
    )
      heapio[p[o]] = n(p[o]);
  });
