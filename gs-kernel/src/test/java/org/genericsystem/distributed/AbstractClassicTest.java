package org.genericsystem.distributed;

import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.kernel.Statics;

public abstract class AbstractClassicTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath);
	}
}
