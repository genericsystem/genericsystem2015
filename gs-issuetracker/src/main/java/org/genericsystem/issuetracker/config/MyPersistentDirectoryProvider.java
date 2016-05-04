package org.genericsystem.issuetracker.config;

import javax.enterprise.inject.Specializes;
import org.genericsystem.cdi.PersistentDirectoryProvider;

@Specializes
public class MyPersistentDirectoryProvider extends PersistentDirectoryProvider {

	@Override
	public String getDirectoryPath() {
		return System.getenv("HOME") + "/issuetracker"; // your directory path
	}
}
