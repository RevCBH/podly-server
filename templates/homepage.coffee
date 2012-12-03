window.PlayerController = ($scope) ->
  lastNode = null

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

# TODO - rate-limit, caching ?
  $scope.currentNode = ->
    t = $scope.time - $scope.mediaPlaybackOffset
    result = _.chain($scope.nodes).filter((n) -> n.time <= t).last().value()
    if (result != lastNode)
      lastNode = result
      $scope.scrollTo result
    result

  $scope.classesForNode = (n) ->
    cur = $scope.currentNode()
    if cur?._id == n._id
      "highlighted"
    else
      ""

  $scope.scrollTo = (n) ->
    target = ($ "##{n._id}")
    container = ($ "#nodes")
    scrollAmount = target.offset().top - container.offset().top + container.scrollTop() - 40

    container.css 'overflow', 'hidden'
    container.animate {scrollTop: scrollAmount}, {
      duration: 1000
      complete: -> container.css 'overflow', 'auto'
    }

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

