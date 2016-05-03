package org.genericsystem.distributed.cacheonclient;

import org.genericsystem.distributed.EnginesDeploymentConfig;
import org.testng.annotations.Test;

@Test
public class MultipleDeploymentsTest extends AbstractTest {

	@Override
	public EnginesDeploymentConfig getDeploymentOptions() {
		return new EnginesDeploymentConfig().addEngine("/FirstEngine", directoryPath + "1").addEngine("/SecondEngine", directoryPath);
	}

	public void test001_Engine_name() {
		ClientEngine engine1 = new ClientEngine("/FirstEngine");
		ClientEngine engine2 = new ClientEngine("/SecondEngine");
	}
}
