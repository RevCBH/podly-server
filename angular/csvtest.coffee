($scope) ->
  nodeTypes = _(`%{rawJS nodeTypesJson}`)

  $scope.csvData = ""
  $scope.data = []
  $scope.headers = []

  $scope.mediaSource = ""

  $scope.episode =
    mediaSources: []
    nodes: []
    searchSlug: ""

  $scope.episodeJson = -> JSON.stringify($scope.episode)

  makeSource = (k, res) ->
    x = {resource: res, kind: {}}
    x.kind[k] = []
    x

  mediaRegExs =
    ".*vimeo.com\\/(video\\/)?(\\d+)": 'VideoVimeo'
    ".*youtube.com\\/watch\\?([^&]*&)*v=([^&]+)": 'VideoYouTube'
    ".*youtu\\.be\\/([^\\/?]+)": 'VideoYouTube'
    ".*traffic\.libsyn\.com.*\\.mp3":  'AudioMp3'

  $scope.parseMediaUrl = (url) ->
    s = _(mediaRegExs).map (kind, re) ->
      match = (new RegExp(re)).exec url
      if match
        makeSource kind, _(match).last()
      else
        undefined

    _(s).find (x) -> x

  $scope.$watch 'mediaSource', (newValue) ->
    x = $scope.parseMediaUrl(newValue)
    $scope.episode.mediaSources = if x then [x] else []

  $scope.translateHeader = (x) ->
    x = x.trim().toLowerCase()
    return "time" if _(['time', 'offset']).contains x
    return "nodeType" if _(['nodetype', 'node type', 'node_type', 'category', 'catagory']).contains x
    return "title" if _(['name', 'title']).contains x
    return "url" if _(['url', 'link', 'uri']).contains x
    return x

  sepChar = "\t"
  $scope.$watch 'csvData', (newValue) ->
    return null unless newValue

    lines = $.csv.parsers.splitLines(newValue, {separator: sepChar, delimiter: '"', state: {}})
    head = lines.shift().split(sepChar)
    lines.unshift _(head).map($scope.translateHeader).join(sepChar)

    xs = $.csv.toObjects lines.join("\n"),
      separator: sepChar

    _(xs).each (x) ->
      x.nodeType = nodeTypes.find (n) -> n.title.toLowerCase() == x.nodeType?.toLowerCase()
      x.nodeType = {} unless x.nodeType

    $scope.episode.nodes = xs