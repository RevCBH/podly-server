app = angular.module 'playerMod'

return ($scope, $routeParams, $http, scrollManager, MediaPlayer, PublishedState) ->
  window.sc = $scope
  window.sm = scrollManager

  $scope.mediaPlayer = new MediaPlayer('#videoContainerCell', $scope, {start: `%{rawJS $ show startAt}`})
  $scope.episode = {title: "loading...", number: $routeParams.episodeNumber}

  # ISSUE, HACK - work around chrome caching issue
  q = $http.get "/episode/#{$routeParams.episodeId}?chromesux=true"
  q.success (data) ->
    # HACK - should be integrated to model
    data.published = PublishedState.fromJSON(data.published)
    # END HACK
    $scope.episode = data
    $scope.loadMedia()

  $scope.loadMedia = ->
    data = $scope.episode
    source = _(data.mediaSources).find (x) -> x.kind.VideoYouTube
    source ||= _(data.mediaSources).find (x) -> x.kind.VideoVimeo
    source ||= _(data.mediaSources).find (x) -> x.kind.AudioMp3
    $scope.mediaPlayer.loadSource(source || data.mediaSources[0])
    setTimeout (-> scrollManager.makeTwitterButtons(jQuery '#listOfNodes')), 0
    scrollManager.watch (jQuery '#listOfNodes')

  $scope.shared = {nodeFilter: null}
  $scope.$watch "shared.nodeFilter", ->
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
    if $scope.shared?.nodeFilter?.length > 0
      $scope.shared.nodeFilter = ""
      # scrollManager.scrollTo nodeAtTime(time), animate: false
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

  $scope.linkToNode = (n) -> `%{toJSON _approot}` + "ni/#{n.relId}"
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