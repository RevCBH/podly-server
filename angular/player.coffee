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

app.directive 'podlyVimeo', ($http) ->
  (scope, element, attrs) ->
    # scope.player = $f 'vimeo-player'
    # scope.player.addEvent 'ready', (x) ->
    #   scope.player.addEvent 'playProgress', (data, id) ->
    #     scope.$apply "time = #{data.seconds}"
    #   scope.play()

    scope.episodePages =
      current: 1
      max: 1
      episodes: []

    scope.loadEpisodePage = (n) ->
      console.log "loadEpisodePage:", n
      n = Math.max 0, Math.min(n, scope.episodePages?.max)
      q = $http.get "/episodes?page=#{n}"
      q.success (data) =>
        scope.episodePages.current = data.pageInfo?.page
        scope.episodePages.max = data.pageInfo?.of
        scope.episodePages.episodes = data.episodes

    scope.loadEpisodePage(1)

app.filter 'range', ->
  (start, end) ->
    start = parseInt(start)
    end = parseInt(end)
    num for num in [start..end]

return ($scope, $routeParams, $http, scrollManager, MediaPlayer, PublishedState) ->
  window.sc = $scope
  window.sm = scrollManager

  $scope.mediaPlayer = new MediaPlayer($('#videoContainerCell'), $scope, true)
  $scope.episode = {title: "loading...", number: $routeParams.episodeNumber}

  q = $http.get "/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}"
  q.success (data) ->
    # HACK - should be integrated to model
    data.published = PublishedState.fromJSON(data.published)
    # END HACK
    $scope.episode = data
    $scope.mediaPlayer.loadVimeoPlayer(data.mediaSources[0].resource, $('#videoContainerCell'))

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