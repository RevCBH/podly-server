app = angular.module('admin')

app.service 'mediaService', () ->
  makeSource = (k, res) ->
    x = {resource: res, kind: {}, offset: 0}
    x.kind[k] = []
    x

  mediaRegExs =
    ".*vimeo.com\\/(video\\/)?(\\d+)": 'VideoVimeo'
    ".*youtube.com\\/watch\\?([^&]*&)*v=([^&]+)": 'VideoYouTube'
    ".*youtu\\.be\\/([^\\/?]+)": 'VideoYouTube'
    ".*traffic\.libsyn\.com.*\\.mp3":  'AudioMp3'

  this.parseMediaUrl = (url) ->
    s = _(mediaRegExs).map (kind, re) ->
      match = (new RegExp(re)).exec url
      if match
        makeSource kind, _(match).last()
      else
        undefined

    _(s).find (x) -> x

  this.embeddedPlayerFor = (source) ->
    if source?.kind?.VideoVimeo
      jQuery """<iframe src="http://player.vimeo.com/video/#{source.resource}?api=1&amp;player_id=vimeo-player" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen>"""
    else
      jQuery """<div>Invalid media source</div>"""

return ($scope, $routeParams, $http, mediaService) ->
  $scope.episode =
    _id: null
    podcast: $routeParams.podcastName
    airDate: null
    searchSlug: null
    duration: null
    published: false

  $scope.mediaSource = ""

  $scope.$watch 'mediaSource', (newValue) ->
    x = mediaService.parseMediaUrl(newValue)
    # $scope.episode.mediaSources = if x then [x] else []
    ($ '#player-preview').html mediaService.embeddedPlayerFor(x)

  $scope.submit = () ->
    # TODO - use form controller
    ($ 'input').attr 'disabled', 'disabled'
    ($ 'button[type=submit]').attr 'disabled', 'disabled'

    q = $http.post("%{cmdCreateEpisode}", $scope.episode)
    q.success (data) ->
      window.location.hash = "#/podcasts/#{$routeParams.podcastName}/episodes/#{data.number}"

    # $http.post("%{cmdSum}", [1, 2]).success (data) ->
      # console.log "+:", data
    # console.log "em:", data
    # $http

