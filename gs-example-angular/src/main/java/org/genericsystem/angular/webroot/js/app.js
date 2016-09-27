angular.module('CrudApp', []).config(['$routeProvider', function ($routeProvider) {
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

	<button class="btn btn-success" ng-click="commit(instance)">
	Save <span class="glyphicon glyphicon-floppy-disk" />
	</button>
 
	<button class="btn btn-warning" ng-click="shift(instance)">
	Shift <span class="glyphicon glyphicon-fast-forward" />
	</button>
 
	<button class="btn btn-danger" ng-click="clear(instance)">
	Cancel <span class="glyphicon glyphicon-trash" />
	</button>
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
		var deleteInst = confirm('Are you absolutely sure you want to delete?');
		if (deleteInst) {
			$http.delete('/api/'+path+'/' + instance.id);           
			$scope.activePath = $location.path('/list');
		}
	};
}
