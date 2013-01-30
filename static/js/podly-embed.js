(function () {
  var d = document;
  var x = d.getElementsByClassName('podly-player')[0];
  var ga = "getAttribute";
  var e = x[ga]('data-episode-id');
  var t = x[ga]('data-start-time');

  var f = d.createElement('iframe');
  f.src = '//podly.co/embed/' + e + (t ? "?t=" + t : "");
  f.setAttribute('frameBorder', 0);
  f.width = 660;
  f.height = 598;
  d.body.insertBefore(f, x);
  d.body.removeChild(x);
})()