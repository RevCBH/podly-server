app = angular.module('scroll', [])

app.service 'scrollManager', () ->
  isAutoScrolling = false
  finishedAutoScrolling = false
  this.shouldAutoScroll = true
  this.disable = => this.shouldAutoScroll = false
  this.enable = => this.shouldAutoScroll = true

  this.makeTwitterButtons = (container) ->
    # TODO - this is a specific function that should be moved elsewhere
    return unless container?.length > 0
    height = container.height()
    inner = $(container.children()[0])
    top = container.offset().top
    bottom = top + container.height()
    elems = inner.find('.nodeElements').parent()
    for x in elems
      x = $(x)
      xtop = x.offset().top
      xbot = xtop + x.height()
      if (bottom >= xtop >= top) or (bottom >= xbot >= top)
        x.addClass('visible-node')
      else
        x.removeClass('visible-node')
    # END TODO

  this.watch = (container) =>
    container.on 'mouseover', '.visible-node', {}, (evt) ->
      Socialite.load(evt.target)

    @makeTwitterButtons(container)
    container.scroll () =>
      @makeTwitterButtons(container)
      if finishedAutoScrolling
        finishedAutoScrolling = false
        tmpShould = this.shouldAutoScroll
        setTimeout (=>
            isAutoScrolling = false
            this.shouldAutoScroll = tmpShould
          ), 100
        return

      @disable() unless isAutoScrolling

  this.getShouldAutoScroll = => this.shouldAutoScroll

  beginAutoScroll = (container) ->
    # container.css 'overflow', 'hidden'
    isAutoScrolling = true
    finishedAutoScrolling = false

  endAutoScroll = (container) ->
    # container.css 'overflow', 'auto'
    finishedAutoScrolling = true

  this.scrollTo = (opts) ->
    return if (not this.shouldAutoScroll) or isAutoScrolling

    container = opts.container
    elem = opts.target
    top = elem.offset().top - container.offset().top + container.scrollTop()

    if opts.animate
      beginAutoScroll(container)
      container.stop().animate {scrollTop: top},
        duration: 1000
        complete: ->
          endAutoScroll(container)
    else
      container.stop().scrollTop(top)
      endAutoScroll(container)

  return this