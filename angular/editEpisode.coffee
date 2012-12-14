app = angular.module('admin')

# TODO - move to common module
app.filter 'formatOffset', -> (input) ->
  n = parseInt input
  c = [
    (n / 3600) % 60, #hh
    (n / 60) % 60, #mm
    n % 60 #ss
  ]

  c = c.map (x) ->
    x = Math.floor x
    x = "0#{x}" if x < 10
    return x

  c.join ':'


app.directive 'episodeEditor', ($routeParams, $http, dataService)->
  (scope, element, attrs) ->
    haveErrored = false
    ealert = (msg) ->
      alert msg unless haveErrored
      haveErrored = true

    req = $http.get("/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}")
    req.success (data) -> scope.episode = data
    req.error -> ealert "Unable to load episode data! Please refresh the page."

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
      q = $http.post("%{cmdCreateNodeType}", scope.newNodeType)
      q.success (data) ->
        dataService.nodeTypes.push(data)
        dataService.nodeTypes = _(dataService.nodeTypes.uniq false, (x) -> x._id)
        _(scope.nodeParseResults).each (x) ->
          if x.node.nodeType is scope.originalNewNodeTitle
            x.node.nodeType = scope.newNodeType
          # if _.isString(x.node.nodeType)
          #   x.node.nodeType = dataService.nodeTypeNamed(x.node.nodeType) || x.node.nodeType
          x.validate()
        scope.hideNodeTypeModal()
      q.error ->
        scope.hideNodeTypeModal()

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
    <div class="in-place-wrapper">
      <input type=text class="in-place-input" ng-show=isEditing ng-model=nodeTypeTitle>
      <span ng-hide=isEditing ng-click='beginEditing()'>
        <a>{{model.title || model || '&lt;none&gt;'}}</a>
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
        <span ng-hide=isEditing ng-click='beginEditing()'>
          <a>{{model || '&lt;none&gt;'}}</a>
        </span>"""

      tElement.html "<div class='in-place-wrapper'></div>"
      tElement.append inputTemplate
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
  console.log "icons:", @icons

  req = $http.get("/nodetypes")
  req.success (data) => @nodeTypes = _(data)
  req.error -> ealert "Unable to load node type data! Please refresh the page."

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
  parseTime = (x) -> _(x.split ':').reduce ((acc, x) -> 60*acc + parseInt(x)), 0

  @separatorCharacter = "\t"

  @parse = (newValue) ->
    return null unless newValue

    lines = $.csv.parsers.splitLines(newValue, {separator: @separatorCharacter, delimiter: '"', state: {}})
    head = lines.shift().split(@separatorCharacter)
    lines.unshift _(head).map(translateHeader).join(@separatorCharacter)

    xs = $.csv.toObjects lines.join("\n"),
      separator: @separatorCharacter

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

return ($scope, $routeParams, $http, nodeCsvParser) ->
  $scope.csvData = null

  $scope.episode = {
    _id: null
    podcast: $routeParams.podcastName
    number: $routeParams.episodeNumber
    title: "Loading..."
  }

  $scope.nodeParseResults = null

  $scope.deleteNode = (node) ->
    console.log "tryna deleteit:", node
    original = $scope.episode.nodes.slice(0)
    $scope.episode.nodes = _(original).without(node)
    q = $http.post("%{cmdDeleteNode}", [node.relId])
    # q.success (data) ->
    q.error () ->
      $scope.episode.nodes = original

  $scope.$watch 'csvData', (newValue) ->
    $scope.parseErrors = null
    $scope.nodeParseResults = null

    try
      $scope.nodeParseResults = _(nodeCsvParser.parse(newValue)).map (x) ->
        res = new NodeParseResult(x)
        res.validate()
        res
    catch error
      $scope.parseErrors = [error.message]

    # xs = nodeCsvParser.parse newValue
    # original = $scope.episode.nodes.slice 0
    # $scope.episode.nodes = xs
    # # q = $http.post("/po")

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

  $scope.saveCSV = (opts={overwrite: false}) ->
    originalResults = $scope.nodeParseResults.slice 0
    originalEp = JSON.stringify $scope.episode

    nodes = {}
    unless opts.overwrite
      _($scope.episode.nodes).each (x) -> nodes[x.time] = x
    _($scope.successNodes()).each (x) -> nodes[x.node.time] = x.node

    $scope.episode.nodes = _(nodes).values()
    $scope.nodeParseResults = $scope.errorNodes()

    q = $http.post("%{cmdReplaceNodes}", $scope.episode)
    q.error ->
      $scope.episode = JSON.parse originalEp
      $scope.nodeParseResults = originalResults
