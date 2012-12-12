app = angular.module('admin')

app.directive 'podcastList', () ->
  # window.rex = $resource
  # window.pcx = Podcasts = $resource '/podcasts'
  console.log "episodeList directive activated"
  (scope, element, attrs) ->
    jQuery.getJSON '/podcasts', (data) ->
      scope.$apply -> scope.podcasts = data
    # data = Podcasts.get -> alertService.doAlert "Got data"

return ($scope) ->
  $scope.podcasts = []