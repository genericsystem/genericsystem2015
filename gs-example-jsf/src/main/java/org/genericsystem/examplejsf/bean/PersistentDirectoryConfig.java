package org.genericsystem.examplejsf.bean;

import javax.enterprise.inject.Specializes;

import org.genericsystem.cdi.PersistentDirectoryProvider;

@Specializes
public class PersistentDirectoryConfig extends PersistentDirectoryProvider {

	@Override
	public String getDirectoryPath() {
		return DEFAULT_DIRECTORY_PATH;
	}
}