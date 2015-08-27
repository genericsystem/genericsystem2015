package org.genericsystem.cache;

import org.genericsystem.kernel.Statics;

public abstract class AbstractMultipleBasesTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addEngine("SecondEngine", null).addEngine("FirstEngine", null);
	}
}
