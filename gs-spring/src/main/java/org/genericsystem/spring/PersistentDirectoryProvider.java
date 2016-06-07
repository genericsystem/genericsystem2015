package org.genericsystem.spring;

import org.springframework.stereotype.Component;

/**
 * Persistence is not activated by default. If you want to persist, you have to set a specialized mock persistentDirectoryProvider in your project :
 * 
 * @Specializes <pre>
 * public class MockPersistentDirectoryProvider extends PersistentDirectoryProvider {
 * 
 * 	&#064;Override
 * 	public String getDirectoryPath() {
 * 		return DIRECTORY_PATH;
 * 	}
 * }
 * </pre>
 * 
 * @author Nicolas Feybesse
 * 
 */
// @ApplicationScoped
@Component
public class PersistentDirectoryProvider {
	public static final String DEFAULT_DIRECTORY_PATH = System.getenv("HOME") + "/test/genericsystem";

	public String getDirectoryPath() {
		return null;// no persistence by default
	}
}
