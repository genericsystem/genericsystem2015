package org.genericsystem.spring;

import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

/**
 * Persistence is not activated by default if you want to persist, you have to set a specialized mock persistentDirectoryProvider in your project :
 *
 * @Specializes public class PersistentDirectoryConfig extends PersitentDirectoryProvider { @Override String getDirectoryPath() { return DIRECTORY_PATH; } }
 *
 * @author Nicolas Feybesse
 *
 */
// @Specializes
// @Alternative
// @ApplicationScoped
@Lazy
@Component
@Primary
public class PersistentDirectoryConfig extends PersistentDirectoryProvider {

	@Override
	public String getDirectoryPath() {
		return DEFAULT_DIRECTORY_PATH;
	}
}
