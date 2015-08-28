package org.genericsystem.cache;

import org.genericsystem.cache.AnnotationTest.Vehicle;
import org.genericsystem.kernel.Statics;

public abstract class AbstractPersistanceTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addClasses(Vehicle.class);
	}

}
