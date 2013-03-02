app = angular.module 'playerMod'

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

return ($scope, $routeParams, $location, $http, scrollManager, MediaPlayer, PublishedState) ->
  window.sc = $scope
  window.sm = scrollManager

  unless window.firstLoad
    startAt = parseInt($location.search().t || "0")
    window.firstLoad = true
  else
    startAt = window.startAt # HACK
    window.startAt = 0

  $scope.mediaPlayer = new MediaPlayer('#videoContainerCell', $scope, {start: startAt})
  $scope.episode = {title: "loading...", number: $routeParams.episodeNumber}

  # ISSUE, HACK - work around chrome caching issue
  q = $http.get "/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}?chromesux=true"
  q.success (data) ->
    # HACK - should be integrated to model
    data.published = PublishedState.fromJSON(data.published)
    # END HACK
    $scope.episode = data
    $scope.loadMedia()

  $scope.loadMedia = ->
    data = $scope.episode
    window.document.title = "Podly - #{$scope.episode.podcast} ##{$scope.episode.number}"
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
  nodeAtTime = (t) ->
    result = _.chain($scope.episode.nodes).filter((n) -> n.time <= t).last().value()

    if (result != lastNode)
      lastNode = result
      $scope.scrollTo result
    result
  # TODO - rate-limit, caching ?
  $scope.currentNode = -> nodeAtTime $scope.time - $scope.mediaPlaybackOffset

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

  $scope.searchResults = []
  $scope.displaySearchResults = false
  $scope.doSearch = ->
    distinctBy = (f, xs) -> _.uniq(xs, false, f)
    p = (name) -> (x) -> x[name]
    # distinctBy p("title"), x.nodes
    take = (n, xs) -> _.take(xs, n)
    map = (f, xs) -> _.map(xs, f)

    q = $http.get "/search?q=#{$scope.episodeSearchValue}"
    q.success (data) ->
      f = (x) ->
        ns = take(5, distinctBy(p("title"), x.nodes))
        [x.episode, map(((n) -> n.episode = x.episode; n), ns)]
      $scope.searchResults = _.flatten map(f, data)
      # $scope.searchResults = _(_(data).map (x) -> [x.episode, _(x.nodes).take(5)]).flatten()
      $scope.displaySearchResults = true
      $scope.episodeSearchValue = null

  $scope.loadSearchResult = (x) ->
    return unless x
    ep = x.episode || x

    # ISSUE - Hack for podcast name
    url = "/podcasts/#{$routeParams.podcastName}/episodes/#{ep.number}"
    if x.relId
      if $routeParams.podcastName is ep.podcast and parseInt($routeParams.episodeNumber) is ep.number
        $scope.seek x
        $scope.displaySearchResults = false
        return
      else
        window.startAt = x.time
    $location.url(url)

  if $.jStorage.get("isSignedUp")
    $scope.signupMessage = ""
    $scope.signupBoxHidden = true
  else
    $scope.signupMessage = "Subscribe for updates"
  $scope.subscribeUser = (evt) ->
    evt.preventDefault() # HACK
    q = $http.post "%{rawJS cmdSignupEmail}", [$scope.signupEmail]
    q.success (data) ->
      console.log "data:", data
      if data?[0] is "OK"
        $scope.signupMessage = "Success! You're subscribed."
        $scope.signupEmail = null
        $scope.signupBoxHidden = true
        $.jStorage.set("isSignedUp", true)
      else
        $scope.signupBoxDisabled = false
        $scope.signupMessage = "Yo dawg, fix your email!"
    q.error ->
      $scope.signupBoxHidden = true
      $scope.signupBoxDisabled = false
      $scope.signupMessage = ""

    $scope.signupBoxDisabled = true