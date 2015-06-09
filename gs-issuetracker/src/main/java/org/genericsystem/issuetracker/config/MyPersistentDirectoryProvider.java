package org.genericsystem.issuetracker.config;

import javax.enterprise.inject.Specializes;

import org.genericsystem.cdi.PersistentDirectoryProvider;
import org.genericsystem.kernel.Statics;

@Specializes
public class MyPersistentDirectoryProvider extends PersistentDirectoryProvider {
	@Override
	public String getEngineValue() {
		return Statics.ENGINE_VALUE; // Engine name
	}

	@Override
	public String getDirectoryPath() {
		return System.getenv("HOME") + "/issuetracker"; // your directory path
	}
}
