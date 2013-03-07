app = angular.module('models', [])

defaultNull = (x) -> if _.isNull(x) or _.isUndefined(x) then null else x

class ModelBase
  @attr = (a...) -> @attrs = (@attrs || []).concat(a)

  constructor: (@model) -> @loadAttrs()
  getAttrs: => @__proto__.constructor.attrs
  loadAttrs: -> @[a] = defaultNull(@model[a]) for a in @getAttrs()
  storeAttrs: -> @model[a] = defaultNull(@[a]) for a in @getAttrs()
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
app.constant 'ModelBase', ModelBase

class Algebraic
  constructor: (@$case) -> @[@$case] = []

  @enum = (xs...) -> @case x for x in xs
  @case = (kase) ->
    @$cases = @$cases || {}
    # TODO - error if dup

    if _(kase).isString()
      @$cases[kase] = []
      @[kase] = new @(kase)
      return null

    for cname, cdef of kase
      @$cases[cname] = _(cdef).pairs()
      @[cname] = (args...) =>
        ins = {}
        res = new @(cname)
        res[cname] = ins
        for [[pname, pdef], a] in _(cdef).chain().pairs().zip(args).value()
          unless a.constructor.name is pdef.name
            throw "invalid argument: #{a} for #{cname}.#{pname}. Expected #{pdef.name}"
          ins[pname] = a
        return res
    return null

  @deriving = (classes...) ->
    for c in classes
      if c.toLowerCase() == 'show'
        cname = @name
        @::toString = -> "#{cname}.#{@$case}"

  @fromJSON = (data) ->
    k = null
    elem = _(data)
    elem = _(JSON.parse(data)) if _.isString(data)
    if elem.isObject()
      [k, vals] = elem.pairs()[0]
      cons = @[k]
      schema = @$cases[k]

      if _.isFunction(cons)
        args = for [prop, type] in schema
          if typeof type?.fromJSON is 'function'
            type.fromJSON vals[prop]
          else
            vals[prop]
        return cons args...
      else
        return cons
    return @[k] if @[k]
    # TODO - raise type error

  toJSON: =>
    res = {}
    for k, v of @
      res[k] = v if @hasOwnProperty(k) and not (k in ['$case'])
    return res

class MediaKind extends Algebraic
  @enum 'AudioMp3', 'VideoVimeo', 'VideoYouTube'
  @deriving 'Show'
app.constant 'MediaKind', MediaKind

class Privilege extends Algebraic
  @enum 'AsEditor', 'AsManager', 'AsPublisher', 'AsAdmin'
  @deriving 'Show'
app.constant 'Privilege', Privilege

class PublishedState extends Algebraic
  @enum 'StateDraft', 'StateSubmitted', 'StatePending', 'StatePublished'
  @deriving 'Show'
app.constant 'PublishedState', PublishedState

class Permission extends Algebraic
  @case HasRole:
    permission: Privilege
  @case HasEpisodeGrant:
    permission: Privilege
    episodeId: Number
  @deriving 'Show'
app.constant 'Permission', Permission

# class User extends ModelBase
#   @attr 'email'
# app.constant 'User', User

class EpisodeDoc extends ModelBase
  @attr 'podcast', 'podcastShortName', 'title', 'number', 'searchSlug', 'airDate', 'published', 'duration'
  @attr 'mediaSources', 'nodes', '_id', 'lastModified'

  constructor: (@model, defPodcastName=null) ->
    @model._id ||= null
    @model.podcast ||= defPodcastName
    @model.podcastShortName ||= defPodcastName
    @model.airDate ||= null
    @model.searchSlug ||= null
    @model.duration ||= null
    @model.lastModified ||= null
    console.log "published:", @model.published
    if @model.published
      @model.published = PublishedState.fromJSON @model.published
    else
      @model.published = PublishedState.StateDraft
    @model.nodes ||= []
    super(@model)
app.constant 'EpisodeDoc', EpisodeDoc

class MediaSourceDoc extends ModelBase
  @attr 'kind', 'offset', 'resource'
  constructor: (@model) ->
    @model.offset ||= 0
app.constant 'MediaSourceDoc', MediaSourceDoc

class NodeRowWrapper extends ModelBase
  @attr 'title', 'time', 'url', 'nodeType', 'relId'

  constructor: (@model, @episode) ->
    super(@model)

  validate: =>
    @errors = []
    @warnings = []

    @errors.push {title: "title is required"} unless @title?.length > 0
    @errors.push {time: "time is required"} unless _.isNumber(@time)
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

  # TODO - this is getting evaluated extremely often, investigate
  messagesFor: (k, list=false) =>
    es = _(@errorsFor(k)).map (x) -> "Error: #{x[k]}"
    ws = _(@warningsFor(k)).map (x) -> "Warn: #{x[k]}"
    xs = es.concat ws
    if list
      xs = "<ul><li>" + xs.join("</li><li>") + "</li></ul>"
    return xs

  update: =>
    console.log "update"
    @validate()
    @storeAttrs()
    # HACK - clear stuck popups on update...
    $('td div.popover-thunk + .popover').prev().popover('destroy')
    window.x = this
    if @isValid()
      console.log "update->cmdSetNodeInstance"
      # TODO - move cmdSetNodeInstance to separate concern
      q = @$http.post("%{rawJS cmdSetNodeInstance}", [@episode._id, @toJSON()])
      # TODO - result handling
      q.success (data) =>
        for a in @getAttrs()
          console.log "\t#{a}: #{data[a]}"
          @model[a] = data[a]
        @loadAttrs()
        console.log "update/success:", data
      q.error => console.log "update/error"

  revert: @loadAttrs

  isNew: => @model._id