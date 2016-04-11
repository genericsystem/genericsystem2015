package org.genericsystem.gsadmin;

import org.genericsystem.admin.model.Car;
import org.genericsystem.admin.model.CarColor;
import org.genericsystem.admin.model.Color;
import org.genericsystem.admin.model.Power;
import org.genericsystem.distributed.EnginesDeploymentConfig.DefaultPathSingleEngineDeployment;
import org.genericsystem.distributed.cacheonclient.EngineServer;

public class Server {
	public static void main(String[] args) {
		EngineServer server = new EngineServer(new DefaultPathSingleEngineDeployment("/home/middleware/test/", Car.class, Power.class, CarColor.class, Color.class));
		server.start();
	}
}
