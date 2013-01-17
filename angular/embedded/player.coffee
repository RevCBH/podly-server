app = angular.module 'playerMod'

app.service 'scrollManager', () ->
  isAutoScrolling = false
  finishedAutoScrolling = false
  this.shouldAutoScroll = true
  this.disable = => this.shouldAutoScroll = false
  this.enable = => this.shouldAutoScroll = true

  this.makeTwitterButtons = (container) ->
    # TODO - this is a specific function that should be moved elsewhere
    height = container.height()
    inner = $(container.children()[0])
    top = container.offset().top
    bottom = top + container.height()
    elems = inner.find('.nodeElements').parent()
    for x in elems
      x = $(x)
      xtop = x.offset().top
      xbot = xtop + x.height()
      if (bottom >= xtop >= top) or (bottom >= xbot >= top)
        x.addClass('visible-node')
      else
        x.removeClass('visible-node')
    # END TODO

  this.watch = (container) =>
    container.on 'mouseover', '.visible-node', {}, (evt) ->
      Socialite.load(evt.target)

    @makeTwitterButtons(container)
    container.scroll () =>
      @makeTwitterButtons(container)
      if finishedAutoScrolling
        finishedAutoScrolling = false
        tmpShould = this.shouldAutoScroll
        setTimeout (=>
            isAutoScrolling = false
            this.shouldAutoScroll = tmpShould
          ), 100
        return

      @disable() unless isAutoScrolling

  this.getShouldAutoScroll = => this.shouldAutoScroll

  beginAutoScroll = (container) ->
    container.css 'overflow', 'hidden'
    isAutoScrolling = true
    finishedAutoScrolling = false

  endAutoScroll = (container) ->
    container.css 'overflow', 'auto'
    finishedAutoScrolling = true

  this.scrollTo = (opts) ->
    return if (not this.shouldAutoScroll) or isAutoScrolling

    container = opts.container
    elem = opts.target
    top = elem.offset().top - container.offset().top + container.scrollTop()

    if opts.animate
      beginAutoScroll(container)
      container.stop().animate {scrollTop: top},
        duration: 1000
        complete: ->
          endAutoScroll(container)
    else
      container.stop().scrollTop(top)
      endAutoScroll(container)

  return this

app.directive 'podlyVimeo', ($http) ->
  (scope, element, attrs) ->
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

  $scope.mediaPlayer = new MediaPlayer($('#videoContainerCell'), $scope, {start: %{startAt}})
  $scope.episode = {title: "loading...", number: $routeParams.episodeNumber}

  q = $http.get "/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}"
  q.success (data) ->
    # HACK - should be integrated to model
    data.published = PublishedState.fromJSON(data.published)
    # END HACK
    $scope.episode = data
    source = _(data.mediaSources).find (x) -> x.kind.VideoYouTube
    source ||= _(data.mediaSources).find (x) -> x.kind.VideoVimeo
    source ||= _(data.mediaSources).find (x) -> x.kind.AudioMp3
    $scope.mediaPlayer.loadSource(source || data.mediaSources[0])
    setTimeout (-> scrollManager.makeTwitterButtons(jQuery '#listOfNodes')), 0

  scrollManager.watch (jQuery '#listOfNodes')
  $scope.$watch "nodeFilter", ->
    setTimeout (-> scrollManager.makeTwitterButtons(jQuery '#listOfNodes')), 0

  $scope.player = undefined
  $scope.mediaPlaybackOffset = 0
  $scope.time = 0

  guardPlayer = (f) -> f($scope.player) if $scope.player

  $scope.seek = (node) -> guardPlayer (p) ->
    time = node.time
    mixpanel?.track 'Seek Node',
      'node.time': node.time
      'node.id': node.relId
      'episode.id': $scope.episode._id
      'episode.number': $scope.episode.number
      'podcast': $scope.episode.podcast

    p.seekTo(time + $scope.mediaPlaybackOffset)
    p.play()

    scrollOpts = null
    if $scope.nodeFilter?.length > 0
      $scope.nodeFilter = ""
      scrollOpts = animate: false
    setTimeout (-> scrollManager.shouldAutoScroll = true), 0

  $scope.play = -> guardPlayer (p) -> p.play()
  $scope.pause = -> guardPlayer (p) -> p.pause()

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

  $scope.linkToNode = (n) -> "%{approot}ni/#{n.relId}"
  $scope.tweetText = (n)->
    # 96 chars avail, 44 taken
    # ISSUE, TODO - don't hardcode the number of available characters
    avail = 96 - n.nodeType.title.length

    template = "Check out '@1'@2"
    topicTxt = n.title
    # TODO - support short names for podcasts
    # episodeTxt = "in #{$scope.episode.podcast} #{'#' + $scope.episode.number}"
    episodeTxt = " in JRE #{'#' + $scope.episode.number}"

    txt = template.replace('@1', topicTxt).replace('@2', episodeTxt)
    return txt unless txt.length > avail
    txt = template.replace('@1', topicTxt).replace('@2', '')
    return txt unless txt.length > avail
    over = txt.length - avail
    topicTxt = topicTxt.substring(0, topicTxt.length - (over + 3)) + '...'
    return template.replace('@1', topicTxt).replace('@2', '')

  $scope.trackExit = (node) ->
    mixpanel?.track 'Exit Node',
      'node.url': node.url
      'node.id': node.relId
      'episode.id': $scope.episode._id
      'episode.number': $scope.episode.number
      'podcast': $scope.episode.podcast