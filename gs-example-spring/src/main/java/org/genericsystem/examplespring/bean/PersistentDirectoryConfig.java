package org.genericsystem.examplespring.bean;

import org.genericsystem.spring.PersistentDirectoryProvider;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

@Primary
@Component
public class PersistentDirectoryConfig extends PersistentDirectoryProvider {

	@Override
	public String getDirectoryPath() {
		return DEFAULT_DIRECTORY_PATH;
	}
}