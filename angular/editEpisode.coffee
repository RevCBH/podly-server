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

app.directive 'podlyVimeo', ->
  (scope, element, attrs) ->
    scope.time = 0
    scope.isPlaying = false
    scope.player = $f 'vimeo-player'
    scope.player.addEvent 'ready', (x) ->
      scope.player.addEvent 'playProgress', (data, id) ->
        scope.$apply "time = #{data.seconds}"

      scope.player.addEvent 'play', -> scope.$apply "isPlaying = true"
      setStopped = -> scope.$apply "isPlaying = false"
      scope.player.addEvent 'pause', setStopped
      scope.player.addEvent 'finish', setStopped

app.directive 'episodeEditor', ($routeParams, $http, dataService)->
  (scope, element, attrs) ->
    req = $http.get("/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}")
    req.success (data) -> scope.episode = data
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

      q = $http.post("%{cmdCreateNodeType}", scope.newNodeType)
      q.success (data) ->
        dataService.nodeTypes.push(data)
        dataService.nodeTypes = _(dataService.nodeTypes.uniq false, (x) -> x._id)
        window.ds = dataService
        _(scope.nodeParseResults).each (x) ->
          if _.isString(x.node.nodeType) && (x.node.nodeType.toLowerCase() == scope.originalNewNodeTitle.toLowerCase())
            x.node.nodeType = data
          x.validate()
        cleanup()
      q.error ->
        cleanup()

InPlaceEditor = (scope) ->
  originalValue = null
  scope.isEditing = false

  scope.beginEditing = =>
    originalValue = JSON.stringify @getter(scope)
    scope.isEditing = true

  @endEditing = (save=true) =>
    unless save
      @setter (JSON.parse originalValue)
    else
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
      <span class="in-place-display" ng-hide=isEditing ng-click='beginEditing()'>
        <a style="margin-left: 7px;">{{model.title || model || '&lt;none&gt;'}}</a>
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
      @setter(scope.nodeTypeTitle)
      # ISSUE, HACK - why do we need to defer this?
      $timeout -> $parse(attrs.onUpdate)(scope.$parent)

    scope.nodeTypeTitle = ipe.getter()
    scope.save = -> ipe.endEditing(true)
    scope.revert = -> ipe.endEditing(false)

    inputElement.blur = -> scope.$apply -> scope.save()

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
        <span class='in-place-display' ng-hide=isEditing ng-click='beginEditing()'>
          <a>{{model || '&lt;none&gt;'}}</a>
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
          if revert
            scope.model = JSON.parse originalValue
          else
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

# class NodeType
#   constructor: (@obj) ->
#   toString: -> @obj.title
#   toJSON: -> @obj.toJSON()
#   title: @obj.title
#   @icon: @obj.icon

app.service 'dataService', ($http) ->
  @nodeTypes = _([])
  @icons = %{L8.unpack $ encode icons}

  req = $http.get("/nodetypes")
  req.success (data) => @nodeTypes = _(data)
  # req.error -> TODO - something reasonable

  @nodeTypeNamed = (name) =>
    name = name.toLowerCase()
    @nodeTypes.find (x) -> x.title?.toLowerCase() == name

  # @icons = =>
  #   @nodeTypes.map (x) -> x.icon

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
      x._id = null
      x.relId = null
      x.linkTitle = ""

    # rejects = _(xs).reject (x) -> x.nodeType
    # xs = _(xs).filter (x) -> x.nodeType
      # x.nodeType = {} unless x.nodeType

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
  loadAttrs: -> @[a] = @model[a] for a in @constructor.attrs
  storeAttrs: -> @model[a] = @[a] for a in @constructor.attrs
  isValid: (k) ->
    xs = @errors || []
    xs = @errorsFor(k) if k
    not (xs.length > 0)

  errorsFor: (k) -> _(@errors || []).filter (x) -> x[k]
  warningsFor: (k) -> _(@warnings || []).filter (x) -> x[k]

class NodeRowWrapper extends ModelWrapper
  @attr 'title', 'time', 'url', 'nodeType'
  @attr 'relId' # TODO - obscure, should be handled by episode entity

  validate: =>
    @errors = []
    @warnings = []

    @errors.push {title: "title is required"} unless @title?.length > 0
    @errors.push {time: "time is required"} unless @time
    @errors.push {nodeType: "nodeType is required"} unless @nodeType
    @errors.push {nodeType: "Invalid nodeType"} if _.isString(@nodeType)
    @errors.push {url: "URL is required"} unless @url
    @warnings.push {url: "Invalid URL"} unless /^http(s)?\:\/\/[^.]+\.[^.]+/.test(@url)
    @isValid()

  invalidNodeType: ->
    x = _(@errors || []).find (x) -> x?.nodeType?.indexOf("Invalid nodeType") == 0
    return @nodeType if x

  statusFor: (k) =>
    return 'status error' if @errorsFor(k).length > 0
    return 'status warning' if @warningsFor(k).length > 0
    return ''

  messagesFor: (k, list=false) =>
    es = _(@errorsFor(k)).map (x) ->
          console.log "Error:", x
          "Error: #{x[k]}"
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
    # if @isValid()
    #   TODO - update backend

  revert: @loadAttrs

  isNew: => @model._id

return ($scope, $routeParams, $http, nodeCsvParser) ->
  window.sc = $scope
  $scope.csvData = null

  $scope.episode = {
    _id: null
    podcast: $routeParams.podcastName
    number: $routeParams.episodeNumber
    title: "Loading..."
  }

  $scope.nodeParseResults = null

  $scope.$watch "episode.nodes", (newValue) ->
        $scope.nodeRows = _(newValue).map (x) ->
          x = new NodeRowWrapper(x)
          x.validate()
          x

  $scope.deleteNode = (node) ->
    original = $scope.episode.nodes.slice(0)
    $scope.episode.nodes = _(original).without(node)
    q = $http.post("%{cmdDeleteNode}", [node.relId])
    # q.success (data) ->
    q.error () ->
      $scope.episode.nodes = original

  $scope.newNodeTitle = ""
  $scope.createNode = ->
    return unless $scope.newNodeTitle?.length > 0
    n = {
      title: $scope.newNodeTitle
      time: $scope.time
    }

    $scope.episode.nodes.push(n)
    n = new NodeRowWrapper(n)
    n.validate()
    $scope.nodeRows.push(n)
    $scope.newNodeTitle = ""

  $scope.syncTime = (n) ->
    n.time = $scope.time
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
    title: "%{cmdSetEpisodeTitle}"
    number: "%{cmdSetEpisodeNumber}"
  $scope.saveProperty = (named) ->
    q = $http.post(propertyUpdaters[named], [$scope.episode._id, $scope.episode[named]])
    q.error -> window.location.reload(true)
    q.success (data) ->
      # HACK - update url if number changes...
      if named is 'number'
        window.location.hash = "#/podcasts/#{$routeParams.podcastName}/episodes/#{data[0]}"
      else
        $scope.episode[named] = data[0]

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

    console.log "\tsending ep:", JSON.parse JSON.stringify(ep)
    window.ep = ep

    q = $http.post("%{cmdReplaceNodes}", ep)
    q.error ->
      $scope.episode = JSON.parse originalEp
      $scope.nodeParseResults = originalResults

  guardPlayer = (f) -> f($scope.player) if $scope.player

  $scope.seek = (time) -> guardPlayer (p) ->
    p.api 'seekTo', time
    p.api 'play'

  $scope.play = -> guardPlayer (p) ->
    p.api 'play'

  $scope.pause = -> guardPlayer (p) ->
    p.api 'pause'

  $scope.togglePlay = ->
    if $scope.isPlaying
      $scope.pause()
    else
      $scope.play()
