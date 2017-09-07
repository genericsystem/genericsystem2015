package org.genericsystem.cv.utils;

import java.lang.invoke.MethodHandles;

import org.opencv.core.Core;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class to wrap opencv native library loading.
 * 
 * @author Pierrik Lassalas
 */
public class NativeLibraryLoader {
	private static boolean loaded = false;
	private static final String libraryPath = "/usr/local/share/OpenCV/java/libopencv_java330.so";
	private static final String fallbackPath = System.getenv("HOME") + "/opencv/build/lib/libopencv_java330.so";
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	// Make sure the library is only loaded once?

	public static void load() {
		load(libraryPath);
	}

	public static void load(String nativeLibraryPath) {
		if (!loaded) {
			try {
				logger.debug("java.library.path: {}", System.getProperty("java.library.path"));
				System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
			} catch (UnsatisfiedLinkError e) {
				logger.info("Unable to load from library path! Using absolute path instead ({})", nativeLibraryPath);
				try {
					System.load(nativeLibraryPath);
				} catch (UnsatisfiedLinkError e1) {
					logger.info("Unable to load then library from {}. Using fallback path ({})", nativeLibraryPath, fallbackPath);
					try {
						System.load(nativeLibraryPath);
					} catch (Exception e2) {
						throw new RuntimeException("Unable to load OpenCV native library", e2);
					}
				}
			} finally {
				loaded = true;
			}

		} else {
			logger.debug("Native library already loaded.");
		}
	}
}
