package org.genericsystem.cdi;

import javax.enterprise.context.ApplicationScoped;

import org.genericsystem.kernel.Statics;

/**
 * Persistence is not activated by default. If you want to persist, you have to set a specialized mock persistentDirectoryProvider in your project :
 * 
 * @Specializes <pre>
 * public class MockPersistentDirectoryProvider extends PersistentDirectoryProvider {
 * 	public String getEngineValue() {
 * 		return Statics.ENGINE_VALUE;
 * 	}
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
@ApplicationScoped
public class PersistentDirectoryProvider {
	public static final String DIRECTORY_PATH = System.getenv("HOME") + "/test/genericsystem";

	public String getEngineValue() {
		return Statics.ENGINE_VALUE;
	}

	public String getDirectoryPath() {
		return null;// no persistence by default
	}
}
