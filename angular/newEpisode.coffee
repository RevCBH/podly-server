app = angular.module('admin')

app.service 'mediaService', () ->
  makeSource = (k, res) ->
    # TODO - use models MediaSource type
    x = {resource: res, kind: {}, offset: 0}
    x.kind[k] = []
    x

  mediaRegExs =
    ".*vimeo\\.com\\/(video\\/)?(\\d+)": 'VideoVimeo'
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
    else if source?.kind?.AudioMp3
      """ <audio controls>
            <source src="#{source.resource}" type="audio/mpeg">
          </audio>"""
    else
      """<div>Invalid media source</div>"""

return ($scope, $routeParams, $http, mediaService, EpisodeDoc) ->
  $scope.episode = new EpisodeDoc(podcast: $routeParams.podcastName)
  window.sc = $scope

  $scope.mediaSourceUrl = ""
  $scope.mediaSource = undefined

  $scope.$watch 'mediaSourceUrl', (newValue) ->
    $scope.mediaSource = mediaService.parseMediaUrl(newValue)
    $scope.episode.mediaSources = if $scope.mediaSource then [$scope.mediaSource] else []
    ($ '#player-preview').html mediaService.embeddedPlayerFor($scope.mediaSource)

  $scope.submit = () ->
    # TODO - use form controller
    ($ 'input').attr 'disabled', 'disabled'
    ($ 'button[type=submit]').attr 'disabled', 'disabled'

    console.log $scope.episode
    window.ep = $scope.episode

    # HACK - WTF? why is cmdCreateEpisode surrounded by backtics?
    cmd = '%{cmdCreateEpisode}'
    cmd = cmd.slice(1,-1) if cmd[0] is '`'
    q = $http.post(cmd, $scope.episode.toJSON())
    q.success (data) ->
      console.log "submit/success:", data
      window.location.hash = "#/podcasts/#{$routeParams.podcastName}/episodes/#{data.number}"
    q.error ->
      console.log "submit/error"

  $scope.isValid = ->
    $scope.mediaSource && true


    # $http.post("%{cmdSum}", [1, 2]).success (data) ->
      # console.log "+:", data
    # console.log "em:", data
    # $http

