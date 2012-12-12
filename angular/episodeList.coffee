app = angular.module('playerMod')

app.service "alertService", () ->
  this.doAlert = () -> alert("service alert")

return ($scope, alertService) ->
  alertService.doAlert()
  $scope.episodes = [
    {number: 1, title: "Boners"}
    {number: 2, title: "Magic"}
    {number: 20, title: "Penny"}
  ]