angular.module('CrudApp', ['ngRoute']).config(['$routeProvider', function ($routeProvider) {
	$routeProvider.
	when('/', {templateUrl: '/tpl/home.html', controller: IndexCtrl}).
	when('/list', {templateUrl: '/tpl/lists.html', controller: ListCtrl}).
	when('/add-inst', {templateUrl: '/tpl/add-new.html', controller: AddCtrl}).
	when('/edit/:id', {templateUrl: '/tpl/edit.html', controller: EditCtrl}).
	otherwise({redirectTo: '/'});

}]);

function IndexCtrl($scope, $http, $location){
	$http.get('/api/types').success(function(data){	
		$scope.choices = data;		
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
	$http.get('/api/'+path).success(function (data) {	
		$scope.instances = data;		
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
		$http.post('/api/'+path+'/', instance).success(function () {
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
	$http.get('/api/'+path+'/' + id).success(function (data) {
		$scope.instance = data;
	});

	$scope.update = function (instance) {
		$http.put('/api/'+path+'/' + id, instance).success(function (data) {
			$scope.instance = data;
			$scope.activePath = $location.path('/list');
		});
	};

	$scope.delete = function (instance) {
			$http.delete('/api/'+path+'/' + instance.id).success(function (data){  
			$scope.activePath = $location.path('/list')});
		};
}
