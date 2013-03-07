app = angular.module('admin')

return ($scope, $location, $routeParams, $http, mediaService, EpisodeDoc) ->
  $scope.episode = new EpisodeDoc(podcast: $routeParams.podcastName)
  # window.sc = $scope

  $scope.mediaSourceUrl = ""
  $scope.mediaSource = undefined

  $scope.$watch 'mediaSourceUrl', (newValue) ->
    $scope.mediaSource = mediaService.parseMediaUrl(newValue)
    $scope.episode.mediaSources = if $scope.mediaSource then [$scope.mediaSource] else []
    ($ '#player-preview').html mediaService.embeddedPlayerFor($scope.mediaSource)

  $scope.submit = () ->
    # TODO - use form controller
    ($ 'input').attr 'disabled', 'disabled'
    ($ 'button[type=submit]').attr 'disabled', 'disabled'

    # HACK - WTF? why is cmdCreateEpisode surrounded by backtics?
    cmd = '%{rawJS cmdCreateEpisode}'
    cmd = cmd.slice(1,-1) if cmd[0] is '`'
    # END HACK
    q = $http.post(cmd, $scope.episode)
    q.success (data) ->
      $scope.episode = data
      # HACK ISSUE - Need to reload page for video to show up...
      $location.url "/admin/podcasts/#{$routeParams.podcastName}/episodes/#{data.number}"
      # window.location.reload(true)
    q.error ->
      console.log "submit/error"

  $scope.isValid = ->
    $scope.mediaSource && true

    # $http.post("%{rawJS cmdSum}", [1, 2]).success (data) ->
      # console.log "+:", data
    # console.log "em:", data
    # $http

