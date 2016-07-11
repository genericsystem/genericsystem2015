package org.genericsystem.demo;

import org.genericsystem.carcolor.AppHtml;
import org.genericsystem.carcolor.model.Car;
import org.genericsystem.carcolor.model.CarColor;
import org.genericsystem.carcolor.model.Color;
import org.genericsystem.carcolor.model.Power;
import org.genericsystem.common.Statics;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.ApplicationsDeploymentConfig;
import org.genericsystem.reactor.model.EngineModel;
import org.genericsystem.todomvc.TodoApp;
import org.genericsystem.todomvc.TodoList;
import org.genericsystem.todomvc.Todos;

/**
 * Hello world!
 *
 */
public class App {
	public static void main(String[] args) {
		ApplicationsDeploymentConfig appsConfig = new ApplicationsDeploymentConfig(Statics.DEFAULT_HOST,
				args.length == 0 ? Statics.DEFAULT_PORT : Integer.parseInt(args[0]));
		appsConfig.addApplication("/todomvc", TodoApp.class, TodoList.class, Engine.class, System.getenv("HOME") + "/genericsystem/todo/", Todos.class);
		appsConfig.addApplication("/apphtml", AppHtml.class, EngineModel.class, Engine.class, System.getenv("HOME") + "/genericsystem/cars/", Car.class,
				Power.class, Color.class, CarColor.class);
		new ApplicationServer(appsConfig).start();

	}
}
