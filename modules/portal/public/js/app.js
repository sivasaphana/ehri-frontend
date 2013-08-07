var Portal = angular.module('Portal', []).service("$search", [SearchService]);

Portal.config(['$routeProvider', function($routeProvider) {
  $routeProvider
      .when("/portal/profile/", {
        templateUrl: ANGULAR_PARTIALS + "/profile.tpl.html",
        controller: ProfileCtrl
      })
      .when("/portal/search/", {
        templateUrl: ANGULAR_PARTIALS + "/search.tpl.html",
        controller: SearchCtrl
      })
      .when("/portal/item/:itemType/:itemId", {
        templateUrl: ANGULAR_PARTIALS + "/item.tpl.html",
        controller: ItemCtrl
      })
      .otherwise({redirectTo: "/portal/search/"});
}]).config(['$locationProvider', function($locationProvider) {
  $locationProvider.html5Mode(true);
  $locationProvider.hashPrefix = "/portal";
}]);


function AppCtrl ($scope, $rootScope, $http) {

  $rootScope.EntityTypes = EntityTypes;

  $http.get(jsRoutes.controllers.portal.Application.account().url).success(function(data) {
    $rootScope.account = data;
  });

  $http.get(jsRoutes.controllers.portal.Application.profile().url).success(function(data) {
    $rootScope.profile = data;
  });
}
