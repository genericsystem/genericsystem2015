package org.genericsystem.examplejsf.bean;

import javax.enterprise.inject.Specializes;

import org.genericsystem.cdi.PersistentDirectoryProvider;
import org.genericsystem.kernel.Statics;

@Specializes
public class MyPersistentDirectoryProvider extends PersistentDirectoryProvider {
	@Override
	public String getEngineValue() {
		return Statics.ENGINE_VALUE;
	}

	@Override
	public String getDirectoryPath() {
		return DIRECTORY_PATH;
	}
}