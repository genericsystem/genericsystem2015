package org.genericsystem.example;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.inject.Alternative;
import javax.enterprise.inject.Specializes;

import org.genericsystem.cdi.PersistentDirectoryProvider;

@Specializes
@Alternative
@ApplicationScoped
public class MyPersistentDirectoryProvider extends PersistentDirectoryProvider {
	@Override
	public String getEngineValue() {
		return "myDataBase";
	}

	@Override
	public String getDirectoryPath() {
		return System.getenv("HOME") + "/my_directory_path";
	}
}
