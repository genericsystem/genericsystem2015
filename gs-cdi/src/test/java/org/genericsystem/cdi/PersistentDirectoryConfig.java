package org.genericsystem.cdi;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.inject.Alternative;
import javax.enterprise.inject.Specializes;

/**
 * Persistence is not activated by default if you want to persist, you have to set a specialized mock persistentDirectoryProvider in your project :
 *
 * @Specializes public class PersistentDirectoryConfig extends PersitentDirectoryProvider { @Override String getDirectoryPath() { return DIRECTORY_PATH; } }
 *
 * @author Nicolas Feybesse
 *
 */

@Specializes
@Alternative
@ApplicationScoped
public class PersistentDirectoryConfig extends PersistentDirectoryProvider {
	@Override
	public String getDirectoryPath() {
		return DEFAULT_DIRECTORY_PATH;
	}
}
