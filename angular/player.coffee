console.log "player.coffee init"
app = angular.module 'playerMod'

app.service 'scrollManager', () ->
  isAutoScrolling = false
  finishedAutoScrolling = false
  shouldAutoScroll = true
  this.disable = () -> shouldAutoScroll = false
  this.enable = () -> shouldAutoScroll = true
  this.watch = (container) =>
    container.scroll () =>
      if finishedAutoScrolling
        finishedAutoScrolling = false
        isAutoScrolling = false
        return

      @disable() unless isAutoScrolling

  this.getShouldAutoScroll = () -> shouldAutoScroll

  beginAutoScroll = (container) ->
    container.css 'overflow', 'hidden'
    isAutoScrolling = true
    finishedAutoScrolling = false

  endAutoScroll = (container) ->
    container.css 'overflow', 'auto'
    finishedAutoScrolling = true

  this.scrollTo = (opts) ->
    return unless shouldAutoScroll

    container = opts.container
    elem = opts.target
    top = elem.offset().top - container.offset().top + container.scrollTop()

    if opts.animate
      beginAutoScroll(container)
      container.animate {scrollTop: top},
        duration: 1000
        complete: () -> endAutoScroll(container)
    else
      container.scrollTop(top)
      endAutoScroll(container)

  return this


app.filter 'formatOffset', -> (input) ->
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

app.directive 'podlyVimeo', () ->
  (scope, element, attrs) ->
    scope.player = $f 'vimeo-player'
    scope.player.addEvent 'ready', (x) ->
      scope.player.addEvent 'playProgress', (data, id) ->
        scope.$apply "time = #{data.seconds}"
      scope.play()

return ($scope, $routeParams, scrollManager) ->
  window.sc = $scope
  window.sm = scrollManager
  $scope.episode = %{epJson}

  scrollManager.watch (jQuery '#listOfNodes')

  $scope.player = undefined
  $scope.mediaPlaybackOffset = 0
  $scope.time = 0

  guardPlayer = (f) -> f($scope.player) if $scope.player

  $scope.seek = (time) -> guardPlayer (p) ->
    p.api 'seekTo', time + $scope.mediaPlaybackOffset
    p.api 'play'

    scrollOpts = null
    if $scope.nodeFilter?.length > 0
      $scope.nodeFilter = ""
      scrollOpts = animate: false

    setTimeout (->
        $scope.time = time + $scope.mediaPlaybackOffset
        scrollManager.disable()
        $scope.currentNode()
        scrollManager.enable()
        $scope.scrollTo $scope.currentNode(), scrollOpts
      ), 0

  $scope.play = -> guardPlayer (p) ->
    p.api 'play'

  $scope.pause = -> guardPlayer (p) ->
    p.api 'pause'

  lastNode = null
  # TODO - rate-limit, caching ?
  $scope.currentNode = ->
    t = $scope.time - $scope.mediaPlaybackOffset
    result = _.chain($scope.episode.nodes).filter((n) -> n.time <= t).last().value()

    if (result != lastNode)
      lastNode = result
      $scope.scrollTo result
    result

  $scope.currentNodeTitle = -> ($scope.currentNode() or {title: "Now Playing"}).title
  $scope.shouldHideSnapButton = ->
    return true unless $scope.currentNode()
    scrollManager.getShouldAutoScroll()

  $scope.classesForNode = (n) ->
    cur = $scope.currentNode()
    if cur?.relId == n?.relId
      "info"
    else
      ""

  $scope.scrollTo = (n, opts={animate: true}) ->
    return unless n
    opts.container = jQuery '#listOfNodes'
    opts.target = jQuery "tr[data-node-id=#{n.relId}]"
    scrollManager.scrollTo opts

  $scope.scrollToCurrent = ->
    scrollManager.enable()
    $scope.scrollTo $scope.currentNode()