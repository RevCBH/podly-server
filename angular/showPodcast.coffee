app = angular.module('admin')

app.directive 'podcastDetail', ($routeParams) ->
  (scope, element, attrs) ->
    jQuery.getJSON "/podcasts/#{$routeParams.podcastName}", (data) ->
      scope.$apply -> scope.podcast = data

app.directive 'episodeList', ($routeParams) ->
  (scope, element, attrs) ->
    jQuery.getJSON "/podcasts/#{$routeParams.podcastName}/episodes", (data) ->
      scope.$apply -> scope.episodes = data

return ($scope, $routeParams, $http, Permission) ->
  $scope.podcast =
    name: $routeParams.podcastName

  $scope.episodes = []
  $scope.rights = _(`%{rawJS rightsDoc}`).map (x) -> Permission.fromJSON(x)
  window.sc = $scope