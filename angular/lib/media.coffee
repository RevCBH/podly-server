app = angular.module('media', ['models'])

# class MediaPlayer
#   initialize: (@container, @scope) ->

#   loadVimeoPlayer: ->

#     """<iframe podly-vimeo id="vimeo-player" src="{{vimeoUrl()}}" width="640" height="360" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen>"""

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

  this.urlForSource = (source) ->
    if source?.kind?.VideoVimeo
      "http://player.vimeo.com/video/#{source.resource}?api=1&amp;player_id=vimeo-player"
    else
      throw "NYI"

  this.embeddedPlayerFor = (source) ->
    if source?.kind?.VideoVimeo
      """<iframe src="http://player.vimeo.com/video/#{source.resource}?api=1&amp;player_id=vimeo-player" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen>"""
    else if source?.kind?.AudioMp3
      """ <audio controls>
            <source src="#{source.resource}" type="audio/mpeg">
          </audio>"""
    else
      """<div>Invalid media source</div>"""

# app.directive 'podlyPlayer', () ->
#   initVimeoPlayer = (scope) ->
#     scope.player = $f 'vimeo-player'
#     scope.player.addEvent 'ready', (x) ->
#       scope.player.addEvent 'playProgress', (data, id) ->
#         scope.$apply "time = #{data.seconds}"

#       scope.player.addEvent 'play', -> scope.$apply "isPlaying = true"
#       setStopped = -> scope.$apply "isPlaying = false"
#       scope.player.addEvent 'pause', setStopped
#       scope.player.addEvent 'finish', setStopped

#   initAudioPlayer = ->
#   initYouTubePlayer = ->

#   (scope, element, attrs) ->
#     scope.time = 0
#     scope.isPlaying = false
#     scope.players = {}

#     exists = (sel) -> element.find(sel).length > 0
#     initVimeoPlayer() if exists '#vimeo-player'
#     initAudioPlayer() if exists '#audio-player'
#     initYouTubePlayer() if exists '#youtube-player'

