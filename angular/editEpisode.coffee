app = angular.module('admin')

app.directive 'episodeEditor', ($routeParams, $http)->
  (scope, element, attrs) ->
    haveErrored = false
    ealert = (msg) ->
      alert msg unless haveErrored
      haveErrored = true

    req = $http.get("/podcasts/#{$routeParams.podcastName}/episodes/#{$routeParams.episodeNumber}")
    req.success (data) -> scope.episode = data
    req.error -> ealert "Unable to load episode data! Please refresh the page."

    ($ 'table tbody[data-selector]').popover()

app.directive 'inPlace', ->
  nxtId = 0
  return {
    transclude: true
    scope: {model: '='}
    compile: (tElement, tAttrs, transclude) ->
      inputElement = "<input id='xxx-#{nxtId++}
      ' class='in-place-input' type=text ng-show=isEditing ng-model=model>"
      previewElement = "<span ng-hide=isEditing ng-click='beginEditing()' ng-transclude></span>"

      tElement.html "<div class='in-place-wrapper'></div>"
      tElement.append inputElement
      tElement.append previewElement

      (scope, elem, attrs) ->
        originalValue = null
        inputElement = $(elem).find('input.in-place-input')

        scope.isEditing = false
        scope.beginEditing = ->
          originalValue = JSON.stringify scope.model
          scope.isEditing = true
          setTimeout (->
            inputElement.focus()), 1000

        scope.endEditing = (revert=false) ->
          scope.model = JSON.parse originalValue if revert
          originalValue = null
          scope.isEditing = false

        inputElement.blur ->
          scope.$apply -> scope.endEditing()

        inputElement.keydown (event) ->
          endWithRevert = (r) ->
            event.preventDefault()
            scope.$apply -> scope.endEditing(r)

          endWithRevert false if event.which is 13
          endWithRevert true if event.keyCode is 27
  }

app.service 'nodeCsvParser', ($http) ->
  @nodeTypes = []

  req = $http.get("/nodetypes")
  req.success (data) => @nodeTypes = _(data)
  req.error -> ealert "Unable to load node type data! Please refresh the page."

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
      nt = @nodeTypes.find (n) -> n.title.toLowerCase() == x.nodeType?.toLowerCase()
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

window.NPR = NodeParseResult = (n) ->
  @node = n
  @validate = ->
    @errors = []
    @errors.push "title is required" unless @node.title?.length > 0
    @errors.push "time is required" unless @node.time
    # errors.push "Invalid time #{x.time}" if _(x.time).isString()
    @errors.push "nodeType is required" unless @node.nodeType
    @errors.push "Invalid nodeType: '#{escapeHtml(@node.nodeType)}'" if _(@node.nodeType).isString()
    @errors.push "Missing URL" unless @node.url

    unless @errors.length
      @isValid = true
      @cssRowClass = "success"
      @rowIcon = "icon-ok"
    return @errors

  @isValid = false
  @cssRowClass = "error"
  @rowIcon = "icon-info-sign"
  @popoverContent = ->
    mid = _(@errors).map((x) -> "<li>#{x}</li>").join("")
    "<ul>" + mid + "</ul>"

  return this

return ($scope, $routeParams, $http, nodeCsvParser) ->
  window.sc = $scope
  window.cparse = nodeCsvParser

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
