package org.genericsystem.cache;

import org.genericsystem.cache.FileSystem.FileType;
import org.genericsystem.kernel.Statics;

public abstract class AbstractFileSystemTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addClasses(FileSystem.class, FileType.class);
	}
}
