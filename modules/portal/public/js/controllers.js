
function SearchCtrl($scope, $rootScope, $search, $location, $http) {
  console.log("Loading searchctrl")

  $scope.doSearch = function() {
    $http({url: getSearchUrl(), method: "GET", headers:{Accept: "application/json"}}).success(function(data) {
      $scope.results = data;
      console.log("Got data: ", data);
    });
  };

  $scope.updateUrl = function() {

  }

  $scope.searchParams = {
    q: $location.search().q || "",
    excludes: [],
    facets: []
  };

  var getSearchUrl = function() {
    var url = jsRoutes.controllers.portal.Application.search().url;
    if (!$scope.searchParams.q.trim() == "") {
      url += "?q=" + $scope.searchParams.q.trim();
    }
    return url;
  }

  $scope.results = {};

  $scope.doSearch();
}

function ProfileCtrl($scope, $rootScope, $http) {

}

function ItemCtrl($scope, $rootScope, $routeParams, $http) {

  var url = null;
  switch ($routeParams.itemType) {
    case EntityTypes.documentaryUnit:
      url = jsRoutes.controllers.archdesc.DocumentaryUnits.get($routeParams.itemId).url;
      break;
    case EntityTypes.historicalAgent:
      url = jsRoutes.controllers.authorities.HistoricalAgents.get($routeParams.itemId).url;
      break;
    case EntityTypes.repository:
      url = jsRoutes.controllers.archdesc.Repositories.get($routeParams.itemId).url;
      break;
    case EntityTypes.cvocConcept:
      url = jsRoutes.controllers.vocabs.Concepts.get($routeParams.itemId).url;
      break;
    default: url = "none";
  }
  //$http.get(url).success(function(data) {
  //$http({method: "GET", url: url, headers: {Accept: "application/json"}}).success(function(data) {
  $http.get(jsRoutes.controllers.core.Application.getGeneric($routeParams.itemId).url).success(function(data) {
    console.log(data)
    $scope.itemData = data;
  });
}