window.PlayerController = ($scope) ->
  $scope.time = 0
  $scope.mediaPlaybackOffset = 0
  $scope.nodes = [{_id: 0, title: "foo", nodeType: {icon: "Fake/Icon"}}]
  $scope.player = undefined

  guardPlayer = (f) -> f($scope.player) if $scope.player

  $scope.seek = (time) -> guardPlayer (p) ->
    p.api 'seekTo', time + $scope.mediaPlaybackOffset
    p.api 'play'

  $scope.play = -> guardPlayer (p) ->
    p.api 'play'

  $scope.pause = -> guardPlayer (p) ->
    p.api 'pause'


vimeo = angular.module('vimeo', ['ngResource'])

vimeo.filter 'formatOffset', -> (input) ->
  n = parseInt input
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

vimeo.directive 'podlyVimeo', ($resource) ->
  Episode = $resource('podcasts/The%20Joe%20Rogan%20Experience/episodes/:episodeNumber')

  (scope, element, attrs) ->
    scope.player = $f 'vimeo-player'
    scope.player.addEvent 'ready', (x) ->
      scope.player.addEvent 'playProgress', (data, id) ->
        scope.$apply "time = #{data.seconds}"

    data = Episode.get episodeNumber: 275, ->
      scope.mediaPlaybackOffset = 1
      scope.nodes = data.nodes

