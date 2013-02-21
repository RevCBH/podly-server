app = angular.module('util', [])
app.directive 'stopEvent', ->
  restrict: 'A'
  link: (scope, element, attr) ->
    element.bind(attr.stopEvent, (e) -> e.stopPropagation())

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

app.filter 'range', ->
  (start, end) ->
    start = parseInt(start)
    end = parseInt(end)
    num for num in [start..end]

app.filter 'categoryTitle', -> (txt) ->
  prefix = 'sponsor: '
  if txt.toLowerCase().indexOf(prefix) is 0
    'Sponsor'
  else
    txt

app.directive 'showTab', ->
  link: (scope, element, attrs) ->
    element.click (e) ->
      e.preventDefault()
      e.stopPropagation()
      $(element).tab('show')