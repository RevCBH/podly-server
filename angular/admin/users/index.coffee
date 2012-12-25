app = angular.module('admin')

app.directive 'userList', ($http) ->
  (scope, element, attrs) ->
    q = $http.get '/users'
    q.success (data) -> scope.users = data

return ($scope) ->
  $scope.users = []