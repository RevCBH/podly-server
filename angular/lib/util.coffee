app = angular.module('util', [])
app.directive 'stopEvent', ->
  restrict: 'A'
  link: (scope, element, attr) ->
    element.bind(attr.stopEvent, (e) -> e.stopPropagation())