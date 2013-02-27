app = angular.module('admin')

app.directive 'stickyBits', ->
  (scope, element, attrs) ->
    anchor = element.find('.sticky-anchor')
    divTop = anchor.offset().top
    sticky = element.find('.sticky')
    $(window).scroll ->
      windowTop = $(window).scrollTop() + 52;
      if windowTop > divTop
        sticky.addClass('stuck')
        anchor.addClass('active');
      else
        sticky.removeClass('stuck')
        anchor.removeClass('active')

app.directive 'episodeEditor', ($routeParams, $http, dataService, $compile)->
  (scope, element, attrs) ->
    # req.error -> TODO - something reasonable

    ($ 'table tbody[data-selector]').popover()

    scope.icons = dataService.icons

    scope.newNodeType = {}
    scope.originalNewNodeTitle = null
    scope.showNewNodeTypeModal = (ntTitle=null) ->
      scope.originalNewNodeTitle = ntTitle
      scope.newNodeType.title = ntTitle
      scope.newNodeType.icon = null
      ($ '#node-type-creator').modal()

    scope.hideNodeTypeModal = ->
      ($ '#node-type-creator').modal('hide')

    scope.saveNewNodeType = ->
      cleanup = ->
        scope.newNodeType = {}
        scope.originalNewNodeTitle = null
        scope.hideNodeTypeModal()

      q = $http.post("%{rawJS cmdCreateNodeType}", scope.newNodeType)
      q.success (data) ->
        dataService.nodeTypes.push(data)
        dataService.nodeTypes = _(dataService.nodeTypes.uniq false, (x) -> x._id)
        window.ds = dataService
        _(scope.nodeParseResults).each (x) ->
          if _.isString(x.node.nodeType) && (x.node.nodeType.toLowerCase() == scope.originalNewNodeTitle.toLowerCase())
            x.node.nodeType = data
            x.validate()
        # TODO - unify with nodeParseResults
        _(scope.nodeRows).each (x) ->
          if _.isString(x.nodeType) && (x.nodeType.toLowerCase() == scope.originalNewNodeTitle.toLowerCase())
            x.nodeType = data
            x.update()
        cleanup()
      q.error ->
        cleanup()

InPlaceEditor = (scope) ->
  originalValue = null
  scope.isEditing = false

  scope.beginEditing = =>
    originalValue = JSON.stringify @getter(scope)
    scope.isEditing = true
    @onBeginEditing() if @onBeginEditing

  @endEditing = (save=true) =>
    return unless scope.isEditing
    unless save
      @setter (JSON.parse originalValue)
    else
      scope.model = $.trim(scope.model)
      @onUpdate() if @onUpdate

    originalValue = null
    scope.isEditing = false

  return this

app.directive 'nodeTypeSelect', (dataService, $parse, $timeout) ->
  replace: true
  transclude: true
  scope:
    model: '='
  template: """
    <div class="in-place-wrapper" style="min-width: 86px;">
      <input type=text class="in-place-input" ng-show=isEditing ng-model=nodeTypeTitle style="width: 72px; margin-left: 0px;">
      <span class="in-place-display" ng-hide=isEditing>
        <a ng-click='beginEditing()' style="margin-left: 7px;">{{model.title || model || '&lt;none&gt;'}}</a>
      </span>
      <span ng-transclude></span>
    </div>
    """
  link: (scope, elem, attrs) ->
    inputElement = elem.find 'input.in-place-input'

    ipe = new InPlaceEditor(scope)
    ipe.getter = ->
      scope.model?.title || scope.model
    ipe.setter = (val) ->
      if _.isString(val)
        val = dataService.nodeTypeNamed(val) || val
      scope.model = val
      scope.nodeTypeTitle = ipe.getter()
      return null
    ipe.onUpdate = ->
      inputElement.val($.trim scope.nodeTypeTitle)
      @setter(scope.nodeTypeTitle)
      # ISSUE, HACK - why do we need to defer this?
      $timeout -> $parse(attrs.onUpdate)(scope.$parent)
    ipe.onBeginEditing = -> setTimeout (-> inputElement.focus()), 0

    scope.nodeTypeTitle = ipe.getter()
    scope.save = -> ipe.endEditing(true)
    scope.revert = -> ipe.endEditing(false)

    inputElement.blur ->
      scope.$apply -> scope.save()

    inputElement.keydown (event) ->
      endWithSave = (s) ->
        event.preventDefault()
        scope.$apply -> ipe.endEditing(s)
      endWithSave(true) if event.which is 13
      endWithSave(false) if event.keyCode is 27

app.directive 'inPlace', ($parse) ->
  return {
    transclude: true
    scope:
      model: '='
    compile: (tElement, tAttrs, transclude) ->
      inputTemplate = """<div class='in-place-input' ng-show=isEditing ng-transclude></div>"""
      previewTemplate = """
        <span class='in-place-display' ng-hide=isEditing>
          <a ng-click='beginEditing()'>{{model || '&lt;none&gt;'}}</a>
        </span>"""

      # tElement.html "<div class='in-place-wrapper'></div>"
      tElement.html inputTemplate
      tElement.append previewTemplate

      (scope, elem, attrs) ->
        originalValue = null
        inputElement = $(elem).find('.in-place-input input')
        onUpdate = $parse(attrs.onUpdate)

        scope.isEditing = false
        scope.beginEditing = ->
          originalValue = JSON.stringify scope.model
          scope.isEditing = true
          setTimeout (-> inputElement.focus()), 0

        scope.endEditing = (revert=false) ->
          return unless scope.isEditing
          if revert
            scope.model = JSON.parse originalValue
          else
            scope.model = $.trim(scope.model)
            inputElement.val(scope.model)
            res = onUpdate(scope.$parent)

          originalValue = null
          scope.isEditing = false

        inputElement.blur ->
          scope.$apply -> scope.endEditing()

        inputElement.keydown (event) ->
          endWithRevert = (r) ->
            event.preventDefault()
            scope.$apply -> scope.endEditing(r)

          endWithRevert false if event.which is 13
          endWithRevert true if event.which is 27
  }

app.service 'dataService', ($http) ->
  @nodeTypes = _([])
  # ISSUE - some more of that toJSON bullshit
  # @icons = JSON.parse("%{rawJS $ L8.unpack $ encode icons}")
  @icons = `%{rawJS $ L8.unpack $ encode icons}`

  req = $http.get("/nodetypes")
  req.success (data) => @nodeTypes = _(data)
  # req.error -> TODO - something reasonable

  @nodeTypeNamed = (name) =>
    name = name.toLowerCase()
    @nodeTypes.find (x) -> x.title?.toLowerCase() == name

  return this

app.service 'nodeCsvParser', (dataService) ->
  translateHeader = (x) ->
    x = x.trim().toLowerCase()
    return "time" if _(['time', 'offset']).contains x
    return "nodeType" if _(['nodetype', 'node type', 'node_type', 'category', 'catagory']).contains x
    return "title" if _(['name', 'title']).contains x
    return "url" if _(['url', 'link', 'uri']).contains x
    return x

  # TODO - try parse time
  parseTime = (x) ->
    _(x.split ':').reduce ((acc, x) -> 60*acc + parseInt(x)), 0

  @separatorCharacter = "\t"

  @parse = (newValue) ->
    return null unless newValue

    # ISSUE, HACK - workaround bad quotes in copypasta from google docs
    esc = (x) ->
      x.replace(/&/g, '&amp;').replace(/"/g, '&quot;')
    uesc = (x) ->
      x.replace(/&quot;/g, '"').replace(/&amp;/g, '&')
    newValue = esc newValue
    #####

    lines = $.csv.parsers.splitLines(newValue, {separator: @separatorCharacter, delimiter: '"', state: {}})
    head = lines.shift().split(@separatorCharacter)
    # ISSUE, HACK - workaround bad quotes in copypasta from google docs
    head = _(head).map uesc
    #####
    lines.unshift _(head).map(translateHeader).join(@separatorCharacter)

    xs = $.csv.toObjects lines.join("\n"),
      separator: @separatorCharacter

    # ISSUE, HACK - workaround bad quotes in copypasta from google docs
    xs = _(xs).map (x) ->
      res = {}
      for own k, v of x
        res[k] = uesc (v || '')
      return res
    #####

    _(xs).each (x) =>
      nt = dataService.nodeTypes.find (n) -> n.title.toLowerCase() == x.nodeType?.toLowerCase()
      x.nodeType = nt || x.nodeType
      x.time = parseTime(x.time)
      x.relId = null

    return xs
    # $scope.episode.nodes = xs

escapeHtml = (str) ->
  entityMap =
    "&": "&amp;"
    "<": "&lt;"
    ">": "&gt;"
    '"': '&quot;'
    "'": '&#39;'
    "/": '&#x2F;'
  String(str).replace /[&<>"'\/]/g, (s) -> entityMap[s]

NodeParseResult = (n) ->
  @node = n
  @validate = =>
    # console.log "validate:", @node
    @errors = []
    @errors.push {title: "title is required"} unless @node.title?.length > 0
    @errors.push {time: "time is required"} unless @node.time
    # errors.push "Invalid time #{x.time}" if _(x.time).isString()
    @errors.push {nodeType: "nodeType is required"} unless @node.nodeType
    @errors.push {nodeType: "Invalid nodeType: '#{escapeHtml(@node.nodeType)}'"} if _(@node.nodeType).isString()
    @errors.push {url: "Missing URL"} unless @node.url

    if @errors.length
      @isValid = false
      @cssRowClass = "error"
      @rowIcon = "icon-info-sign"
    else
      @isValid = true
      @cssRowClass = "success"
      @rowIcon = "icon-ok"
    return @errors

  @isValid = false
  @cssRowClass = "error"
  @rowIcon = "icon-info-sign"
  @popoverContent = ->
    mid = _(@errors).map((x) -> "<li>#{_(x).values()[0]}</li>").join("")
    "<ul>" + mid + "</ul>"
  @invalidNodeType = ->
    x = _(@errors).find (x) -> x.nodeType?.indexOf("Invalid nodeType") == 0
    return @node.nodeType if x

  return this

class ModelWrapper
  @attr = (a...) -> @attrs = (@attrs || []).concat(a)

  constructor: (@model) -> @loadAttrs()

  getAttrs: => @__proto__.constructor.attrs

  defaultNull = (x) ->
    if _.isNull(x) or _.isUndefined(x) then null else x

  loadAttrs: -> @[a] = defaultNull(@model[a]) for a in @getAttrs()
  storeAttrs: -> @model[a] = defaultNull(@[a]) for a in @getAttrs() # @constructor.attrs
  isValid: (k) ->
    xs = @errors || []
    xs = @errorsFor(k) if k
    not (xs.length > 0)

  errorsFor: (k) -> _(@errors || []).filter (x) -> x[k]
  warningsFor: (k) -> _(@warnings || []).filter (x) -> x[k]

  toJSON: ->
    res = {}
    res[a] = @[a] for a in @getAttrs()
    return res

class NodeRowWrapper extends ModelWrapper
  @attr 'title', 'time', 'url', 'nodeType'
  @attr 'relId'

  constructor: (@model, @episode) ->
    super(@model)

  validate: =>
    @errors = []
    @warnings = []

    @errors.push {title: "title is required"} unless @title?.length > 0
    @errors.push {time: "time is required"} unless _.isNumber(@time)
    @warnings.push {nodeType: "No category specified"} unless @nodeType
    @errors.push {nodeType: "Category doesn't exist"} if _.isString(@nodeType) and @nodeType.length
    # @errors.push {url: "URL is required"} unless @url
    @warnings.push {url: "Invalid URL"} unless /^http(s)?\:\/\/[^.]+\.[^.]+/.test(@url)
    @isValid()

  invalidNodeType: ->
    x = _(@errors || []).find (x) -> _.isString(x?.nodeType)
    return @nodeType if x

  statusFor: (k) =>
    return 'status error' if @errorsFor(k).length > 0
    return 'status warning' if @warningsFor(k).length > 0
    return ''

  # TODO - this is getting evaluated extremely often, investigate
  messagesFor: (k, list=false) =>
    es = _(@errorsFor(k)).map (x) -> "Error: #{x[k]}"
    ws = _(@warningsFor(k)).map (x) -> "Warn: #{x[k]}"
    xs = es.concat ws
    if list
      xs = "<ul><li>" + xs.join("</li><li>") + "</li></ul>"
    return xs

  update: =>
    @validate()
    @storeAttrs()
    # HACK - clear stuck popups on update...
    $('td div.popover-thunk + .popover').prev().popover('destroy')
    window.x = this
    if @isValid()
      q = @$http.post("%{rawJS cmdSetNodeInstance}", [@episode._id, @toJSON()])
      # TODO - result handling
      q.success (data) =>
        for a in @getAttrs()
          @model[a] = data[a]
        @loadAttrs()
      q.error => console.log "update/error"

  revert: @loadAttrs

  isNew: => @model._id
  manualMode: false

return ($scope, $routeParams, $location, $http, nodeCsvParser, $compile, PublishedState, MediaPlayer, Permission, Privilege, $filter, mediaService) ->
  window.sc = $scope
  $scope.csvData = null

  window.http = NodeRowWrapper::$http = $http # HACK
  window.ps = PublishedState
  $scope.PublishedState = PublishedState

  $scope.mediaPlayer = new MediaPlayer($('#videoContainerCell'), $scope)

  $scope.episode = {
    _id: null
    podcast: $routeParams.podcastName
    number: $routeParams.episodeNumber
    title: "Loading..."
  }
  $scope.rights = {}

  # ISSUE, HACK - work around chrome caching issue
  req = $http.get("/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}?chromesux=true")
  req.success (data) ->
    # HACK - should be integrated to model
    data.published = PublishedState.fromJSON(data.published)
    # END HACK
    $scope.episode = data
    $scope.mediaPlayer.loadSource(data.mediaSources[0])
    # $scope.mediaPlayer.loadVimeoPlayer(data.mediaSources[0].resource, $('#videoContainerCell'))

    q = $http.post("%{rawJS cmdGrantsForEpisode}", [$scope.episode._id])
    q.success (data) ->
      res = {}
      for k, v of data
        v.grants = _(v.grants).map (x) -> Permission.fromJSON x
        v.id = k
      $scope.rights = data

  $scope.nodeParseResults = null

  $scope.$watch "episode.nodes", (newValue) ->
        $scope.nodeRows = _(newValue).map (x) ->
          x = new NodeRowWrapper(x, $scope.episode)
          x.validate()
          x

  $scope.vimeoUrl = ->
    resource = 51488504
    "http://player.vimeo.com/video/#{resource}?api=1&amp;player_id=vimeo-player"

  $scope.deleteNode = (node) ->
    original = $scope.episode.nodes.slice(0)
    $scope.episode.nodes = _(original).without(node)
    q = $http.post("%{rawJS cmdDeleteNode}", [node.relId])
    # q.success (data) ->
    q.error -> $scope.episode.nodes = original

  $scope.newNodeTitle = ""
  $scope.$watch "newNodeTitle", (newValue, oldValue) ->
    if newValue.length > 0 && oldValue.length == 0
      $scope.pause()
    # if newValue.length == 0 && oldValue.length > 0
    #   $scope.play()

  $scope.createNode = ->
    return unless $scope.newNodeTitle?.length > 0
    n = {
      title: $scope.newNodeTitle
      time: $scope.time
    }

    $scope.episode.nodes.push(n)
    n = new NodeRowWrapper(n, $scope.episode)
    # n.validate()
    n.update()

    $scope.lastCreatedNode = n
    $scope.nodeRows.push(n)
    $scope.newNodeTitle = ""
    $scope.seek($scope.time - 5)

  $scope.syncTime = (n, evt) ->
    # console.log "syncTime - this:", @, "n:", n, "evt:", evt
    if evt?.shiftKey
      n.manualMode = true
      window.fi = $filter
      n.newTimeBuffer = $filter('formatOffset')(n.time)
      setTimeout (-> $(evt.target).parent().find('input').focus()), 0
    else
      n.time = $scope.time
      n.update()

  $scope.updateTimeManual = (n, save) ->
    n.manualMode = false
    if save and n.newTimeBuffer
      if n.newTimeBuffer.indexOf(':') >= 0
        parts = n.newTimeBuffer.split ':'
      else
        parts = [n.newTimeBuffer.slice(0,2), n.newTimeBuffer.slice(2,4), n.newTimeBuffer.slice(4,6)]
      n.time = _(parts).reduce ((acc, x) -> 60*acc + parseInt(x)), 0
      n.update()

  $scope.curTab = "nodes"

  $scope.$watch 'csvData', (newValue) ->
    $scope.parseErrors = null
    $scope.nodeParseResults = null

    try
      $scope.nodeParseResults = _(nodeCsvParser.parse(newValue)).map (x) ->
        res = new NodeParseResult(x)
        res.validate()
        res
    catch error
      throw error
      $scope.parseErrors = [error.message]

  $scope.nodeRowClass = (node) ->
    if node.nodeType
      "success"
    else
      "error"

  $scope.successNodes = -> _($scope.nodeParseResults).filter (x) -> x.isValid
  $scope.errorNodes = -> _($scope.nodeParseResults).reject (x) -> x.isValid
  $scope.appendBtnClass = ->
    if $scope.errorNodes().length is 0
      "btn-success"
    else
      "btn-warning"

  propertyUpdaters =
    title: "%{rawJS cmdSetEpisodeTitle}"
    number: "%{rawJS cmdSetEpisodeNumber}"
  $scope.saveProperty = (named) ->
    q = $http.post(propertyUpdaters[named], [$scope.episode._id, $scope.episode[named]])
    q.error -> window.location.reload(true)
    q.success (data) ->
      # HACK - update url if number changes...
      if named is 'number'
        $location.url("/admin/podcasts/#{$routeParams.podcastName}/episodes/#{data.number}")
      else
        $scope.episode[named] = data[named]

  $scope.saveCSV = (opts={overwrite: false}) ->
    originalResults = $scope.nodeParseResults.slice 0
    originalEp = JSON.stringify $scope.episode

    nodes = {}
    unless opts.overwrite
      _($scope.episode.nodes).each (x) -> nodes[x.time] = x
    _($scope.successNodes()).each (x) -> nodes[x.node.time] = x.node

    $scope.episode.nodes = _(nodes).values()
    $scope.nodeParseResults = $scope.errorNodes()

    ep = JSON.parse JSON.stringify($scope.episode)
    # TODO - proper node toJSON somewhere
    _(ep.nodes).each (x) ->
      ks = _(x).keys()
      for k in ks
        delete x[k] if k.indexOf('$') == 0 || k == 'notes'

    # console.log "\tsending ep:", JSON.parse JSON.stringify(ep)
    window.ep = ep

    q = $http.post("%{rawJS cmdReplaceNodes}", ep)
    q.error ->
      $scope.episode = JSON.parse originalEp
      $scope.nodeParseResults = originalResults

  $scope.userRights = _(`%{rawJS rightsDoc}`).map (x) -> Permission.fromJSON(x)
  user = `%{rawJS userDoc}`

  $scope.submitEpisode = ->
    $scope.episode.published = PublishedState.StateSubmitted
    q = $http.post("%{rawJS cmdSubmitForReview}", [$scope.episode._id])
    q.success (data) ->
      # HACK - should be integrated to model
      data.published = PublishedState.fromJSON(data.published)
      # END HACK
      $scope.episode = data
    q.error (err) -> console.log "error:", err

  hasEpGrant = (x, grant) ->
    x = x.HasEpisodeGrant
    x?.permission?[grant] if (x?.episodeId is $scope.episode._id)
  hasEpRight = (x, right) -> x.HasRole?.permission?[right] or hasEpGrant(x, right)

  hasEditRight = -> _($scope.userRights).find (x) -> hasEpRight(x, 'AsEditor')
  hasPublishRight = -> _($scope.userRights).find (x) -> hasEpRight(x, 'AsPublisher')
  hasManageRight = -> _($scope.userRights).find (x) -> hasEpRight(x, 'AsManager')
  hasAdminRight = -> _($scope.userRights).find (x) -> hasEpRight(x, 'AsAdmin')

  $scope.canPublish = ->
    hasPublishRight() and _([PublishedState.StateSubmitted, PublishedState.StatePending]).contains $scope.episode.published
  $scope.canUnpublish = ->
    hasPublishRight() and (PublishedState.StatePublished is $scope.episode.published)
  $scope.canSubmit = ->
    hasEditRight() and ($scope.episode.published is PublishedState.StateDraft)
  $scope.canManage = ->
    hasAdminRight() or hasManageRight()
  $scope.isAdmin = hasAdminRight

  $scope.hasPermisson = (userId, perm) ->
    userId = parseInt userId if _.isString(userId)
    _($scope.rights[userId].grants).find (x) -> hasEpGrant(x, perm)

  $scope.togglePermission = (userId, perm) ->
    userId = parseInt userId if _.isString(userId)
    grant = $scope.hasPermisson(userId, perm)
    if grant
      q = $http.post("%{rawJS cmdRevoke}", [userId, grant])
      q.success ->
        vals = _($scope.rights[userId].grants).without grant
        $scope.rights[userId].grants = vals
        $scope.userRights = vals if userId is user._id
    else
      grant = Permission.HasEpisodeGrant(Privilege[perm], $scope.episode._id)
      q = $http.post("%{rawJS cmdGrant}", [userId, grant])
      q.success ->
        $scope.rights[userId].grants.push ||= []
        $scope.rights[userId].grants.push grant
        $scope.userRights.push grant if userId is user._id

  $scope.addGrantUser = ->
    q = $http.post("%{rawJS cmdGetUserForEmail}", [$scope.grantUserEmail])

    q.success (data) ->
      $scope.grantUserEmail = ""
      return if $scope.rights[data._id]
      $scope.rights[data._id] = {id: data._id, ident: data.identity, grants:[]}
    q.error ->

  updatePublishedTo = (state, action) ->
    original = $scope.episode.published
    $scope.episode.published = state
    q = $http.post(action, [$scope.episode._id])
    q.error -> $scope.episode.published = original

  $scope.publishEpisode = -> updatePublishedTo PublishedState.StatePublished, "%{rawJS cmdPublishEpisode}"
  $scope.unpublishEpisode = -> updatePublishedTo PublishedState.StatePending, "%{rawJS cmdUnpublishEpisode}"

  guardPlayer = (f) -> f($scope.player) if $scope.player

  $scope.seek = (time) -> guardPlayer (p) ->
    p.seekTo(time)
    p.play()

  $scope.play = -> guardPlayer (p) -> p.play()

  $scope.pause = -> guardPlayer (p) -> p.pause()

  $scope.togglePlay = (evt)->
    evt.preventDefault() if evt?.preventDefault
    if $scope.isPlaying
      $scope.pause()
    else
      $scope.play()

  $scope.$watch 'newMediaSourceUrl', (newValue) ->
    $scope.newMediaSource = mediaService.parseMediaUrl(newValue)
    console.log $scope.newMediaSource
    ($ '#newMediaPreview').html mediaService.embeddedPlayerFor($scope.newMediaSource)

  $scope.removeMediaSource = (x) ->
    originalValue = JSON.stringify $scope.episode.mediaSources
    $scope.episode.mediaSources = _($scope.episode.mediaSources).without(x)
    q = $http.post("%{rawJS cmdRemoveMediaSource}", [$scope.episode._id, x.kind])
    q.error ->
      $scope.episode.mediaSources = JSON.parse originalValue

  $scope.addMediaSource = ->
    return unless $scope.newMediaSource
    originalValue = JSON.stringify $scope.episode.mediaSources
    $scope.episode.mediaSources = _($scope.episode.mediaSources).push($scope.newMediaSource)
    q = $http.post("%{rawJS cmdAddMediaSource}", [$scope.episode._id, $scope.newMediaSource])
    q.success ->
      $scope.newMediaSource = undefined
      $scope.newMediaSourceUrl = ""
    q.error ->
      $scope.episode.mediaSources = JSON.parse originalValue
