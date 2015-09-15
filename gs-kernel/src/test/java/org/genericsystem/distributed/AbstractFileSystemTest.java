package org.genericsystem.distributed;

import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.distributed.FileSystem.FileType;
import org.genericsystem.kernel.Statics;

public abstract class AbstractFileSystemTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addClasses(FileSystem.class, FileType.class);
	}
}
