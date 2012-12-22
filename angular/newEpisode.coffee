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

return ($scope, $routeParams, $http, mediaService, EpisodeDoc) ->
  $scope.episode = new EpisodeDoc(podcast: $routeParams.podcastName)
  window.sc = $scope

  $scope.mediaSource = ""

  $scope.$watch 'mediaSource', (newValue) ->
    x = mediaService.parseMediaUrl(newValue)
    # $scope.episode.mediaSources = if x then [x] else []
    ($ '#player-preview').html mediaService.embeddedPlayerFor(x)

  $scope.submit = () ->
    # TODO - use form controller
    ($ 'input').attr 'disabled', 'disabled'
    ($ 'button[type=submit]').attr 'disabled', 'disabled'

    console.log $scope.episode
    # $scope.episode.

    q = $http.post("%{cmdCreateEpisode}", $scope.episode.toJSON())
    q.success (data) ->
      console.log "submit/success:", data
      window.location.hash = "#/podcasts/#{$routeParams.podcastName}/episodes/#{data.number}"
    q.error ->
      console.log "submit/error"


    # $http.post("%{cmdSum}", [1, 2]).success (data) ->
      # console.log "+:", data
    # console.log "em:", data
    # $http

