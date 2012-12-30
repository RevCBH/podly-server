app = angular.module('admin')

return ($scope, $routeParams, $http, mediaService, EpisodeDoc) ->
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

    console.log $scope.episode
    # window.ep = $scope.episode

    # HACK - WTF? why is cmdCreateEpisode surrounded by backtics?
    cmd = '%{cmdCreateEpisode}'
    cmd = cmd.slice(1,-1) if cmd[0] is '`'
    # END HACK
    q = $http.post(cmd, $scope.episode.toJSON())
    q.success (data) ->
      window.location.hash = "#/podcasts/#{$routeParams.podcastName}/episodes/#{data.number}"
    q.error ->
      console.log "submit/error"

  $scope.isValid = ->
    $scope.mediaSource && true


    # $http.post("%{cmdSum}", [1, 2]).success (data) ->
      # console.log "+:", data
    # console.log "em:", data
    # $http

