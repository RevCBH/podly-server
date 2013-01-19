app = angular.module('media', ['models'])

app.service 'mediaService', (MediaKind) ->
  makeSource = (k, res) ->
    {resource: res, kind: MediaKind[k], offset: 0}

  mediaRegExs =
    ".*vimeo\\.com\\/(video\\/)?(\\d+)": 'VideoVimeo'
    ".*youtube.com\\/watch\\?([^&]*&)*v=([^&#]+)": 'VideoYouTube'
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
    else if source?.kind?.VideoYouTube
      youtubeUrl = "http://www.youtube.com/embed/#{source.resource}"
      """<iframe type="text/html" src="#{youtubeUrl}" frameborder="0"></iframe>"""
    else
      """<div>Invalid media source</div>"""

class MediaPlayer
  @__plr_id = 0
  constructor: (@container, @scope, @autoplay=false, @width=640, @height=360) ->
    @player = null

  loadSource: (source) ->
    kind = null
    if source.kind.VideoVimeo
      kind = 'Vimeo'
      @loadVimeoPlayer(source.resource)
    else if source.kind.VideoYouTube
      kind = 'YouTube'
      @loadYoutubePlayer(source.resource)
    else if source.kind.AudioMp3
      kind = 'Mp3'
      @loadAudioPlayer(source.resource)
    mixpanel?.track 'Load Media', {resource: "#{kind}@#{source.resource}"}

  loadYoutubePlayer: (resource) ->
    unless window.YT
      window.onYouTubeIframeAPIReady = -> delete window.onYouTubeIframeAPIReady

    plrId = "youtube-player-#{@constructor.__plr_id++}"
    $(@container).html "<div id='#{plrId}'></div>"

    onPlayerReady = (event) =>
      @player = event.target
      if @autoplay
        @seekTo(@autoplay.start) if @autoplay.start
        @play()

    onPlayerStateChange = (evt) =>
      if evt.data is YT.PlayerState.PLAYING
        @scope.$apply "isPlaying = true"
        startPolling()
      else
        @scope.$apply "isPlaying = false"
        stopPolling()

    mkPlayer = =>
      # HACK - don't use polling here
      return setTimeout(mkPlayer, 10) if window.onYouTubeIframeAPIReady

      new YT.Player plrId,
        width: @width
        height: @height
        videoId: resource
        events:
          onReady: onPlayerReady
          onStateChange: onPlayerStateChange
        playerVars:
          autohide: 1
          disablekb: 1
          controls: 1
          modestbranding: 1
          showinfo: 0
    mkPlayer()

    pollTime = =>
        @scope.$apply "time = #{@player.getCurrentTime()}"
        if @__should_poll
          setTimeout pollTime, 300
    startPolling = =>
      return if @__should_poll
      @__should_poll = true
      pollTime()
    stopPolling = =>
      @__should_poll = false

    @play = => @player.playVideo()
    @pause = => @player.pauseVideo()
    @seekTo = (t) => @player.seekTo(t)
    @scope.player = this

  loadVimeoPlayer: (resource) ->
    plrId = "vimeo-player-#{@constructor.__plr_id++}"
    vimeoUrl = "http://player.vimeo.com/video/#{resource}?api=1&amp;player_id=#{plrId}"
    key = "vimeo-#{resource}"
    jqElem = jQuery """<iframe podly-vimeo id="#{plrId}" src="#{vimeoUrl}" width="#{@width}" height="#{@height}" frameborder="0" webkitAllowFullScreen mozallowfullscreen allowFullScreen>"""
    $(@container).html jqElem
    plr = $f(plrId)
    @player = plr

    plr.addEvent 'ready', =>
      plr.addEvent 'playProgress', (data, id) => @scope.$apply "time = #{data.seconds}"
      plr.addEvent 'play', => @scope.$apply "isPlaying = true"
      setStopped = => @scope.$apply "isPlaying = false"
      plr.addEvent 'pause', setStopped
      plr.addEvent 'finish', setStopped

      if @autoplay
        if @autoplay.start
          ensurePlayAt = (t) =>
            console.log "scope.time:", @scope.time
            unless @scope.time >= t
              plr.api 'seekTo', t
              setTimeout (-> ensurePlayAt t), 200

          setTimeout (=>
            plr.api 'play'
            ensurePlayAt @autoplay.start), 0
        else
          setTimeout (-> plr.api 'play'), 0

    @play = => @player.api 'play'
    @pause = => @player.api 'pause'
    @seekTo = (t) => @player.api 'seekTo', t
    @scope.player = this

  loadAudioPlayer: (resource) ->
    plrId = "mp3-player-#{@constructor.__plr_id++}"
    jqElem = jQuery """<audio id="#{plrId}" controls><source src="#{resource}" type="audio/mpeg"></audio>"""
    $(@container).html jqElem
    $(@container).height '30px'
    @player = jqElem[0]
    jQuery(window).resize()

    @player.addEventListener 'timeupdate', => @scope.$apply "time = #{@player.currentTime}"
    @player.addEventListener 'play', => @scope.$apply "isPlaying = true"
    setStopped = => @scope.$apply "isPlaying = false"
    for x in ['pause', 'ended']
      @player.addEventListener x, setStopped
    @player.addEventListener 'canplay', =>
      if @autoplay
        @seekTo(@autoplay.start) if @autoplay.start
        @play()

    @play = => @player.play()
    @pause = => @player.pause()
    @seekTo = (t) => @player.currentTime = t
    @scope.player = this

app.constant 'MediaPlayer', MediaPlayer
