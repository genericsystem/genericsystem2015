angular.module('CrudApp', ['ngRoute']).config(['$routeProvider', function ($routeProvider) {
	$routeProvider.
	when('/', {templateUrl: '/tpl/home.html', controller: IndexCtrl}).
	when('/list', {templateUrl: '/tpl/lists.html', controller: ListCtrl}).
	when('/add-inst', {templateUrl: '/tpl/add-new.html', controller: AddCtrl}).
	when('/edit/:id', {templateUrl: '/tpl/edit.html', controller: EditCtrl}).
	otherwise({redirectTo: '/'});
}]);

function IndexCtrl($scope, $http, $location){
	$http.get('/api/types').then(function(response) {
		$scope.choices = response.data;		
		$scope.select = function(choice){
			path = choice.tableName;
			columns = choice.columns;			
			$scope.activePath = $location.path('/list');			
		};		
	});
}

function ListCtrl($scope, $http, $route) {
	$scope.type = path;
	$scope.names = columns;
	$http.get('/api/'+path).then(function(response) {
		$scope.instances = response.data;		
	});	

	$scope.commit = function(){
		$http.put('/api/'+path+'/commit/');
	};
	$scope.shift = function () {
			$http.post('/api/'+path+'/shift/');
			$route.reload();	
	}; 
	$scope.clear = function (instance) {
			$http.delete('/api/'+path+'/clear/');			
			$route.reload();						
	};	
}

function AddCtrl($scope, $http, $location) {
	$scope.master = {};
	$scope.activePath = null;
	$scope.type = path;
	$scope.names = columns;
	$scope.add_new = function (instance, AddNewForm) {
		$http.post('/api/'+path+'/', instance).then(function(response) {
			$scope.reset();
			$scope.activePath = $location.path('/list');
		});
		$scope.reset = function () {
			$scope.instance = angular.copy($scope.master);
		};
		$scope.reset();
	};
}

function EditCtrl($scope, $http, $location, $routeParams) {
	var id = $routeParams.id; 
	$scope.activePath = null;
	$scope.type = path;	
	$scope.names = columns; 
	$http.get('/api/'+path+'/' + id).then(function(response) {
		$scope.instance = response.data;
	});

	$scope.update = function (instance) {
		$http.put('/api/'+path+'/' + id, instance).then(function(response) {
			console.log("UPDAAAAAAAATEEEE");
			$scope.instance = response.data;
			$scope.activePath = $location.path('/list');
			$scope.apply();
			})
	};
	$scope.delete = function (instance) {
			$http.delete('/api/'+path+'/' + instance.id).then(function(response) {
			$scope.activePath = $location.path('/list');
			$scope.apply();
			})
		};
}
