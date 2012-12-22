app = angular.module('filters', [])

app.filter 'formatOffset', -> (input) ->
  n = parseInt input
  return '--:--:--' if _.isNaN(n)

  c = [
    (n / 3600) % 60, #hh
    (n / 60) % 60, #mm
    n % 60 #ss
  ]

  c = c.map (x) ->
    x = Math.floor x
    x = "0#{x}" if x < 10
    return x

  c.join ':'