jQuery(document).ready ->
  setNodeListSize = ->
    # ISSUE, HACK - don't rely on JS for this, don't hardcode heights if you have to
    x = jQuery('#listOfNodes')
    before = jQuery('#topContent')
    t = before.offset().top + before.height()
    h = Math.max(55*3, jQuery(window).height() - t - 3)
    x.height(h)
  jQuery(window).resize(setNodeListSize)
  setNodeListSize()

tag = document.createElement('script')
tag.src = "//www.youtube.com/iframe_api"
firstScriptTag = document.getElementsByTagName('script')[0]
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag)