package org.genericsystem.cv.utils;

import java.lang.invoke.MethodHandles;

import org.opencv.core.Core;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Helper class to wrap OpenCV native library loading.
 * 
 * @author Pierrik Lassalas
 */
public class NativeLibraryLoader {

	/**
	 * Boolean value set to true as soon as the library is loaded.
	 */
	private static boolean loaded = false;
	/**
	 * Default library path. Can be overridden using {@link #load(String)} and providing the new path.
	 */
	private static final String libraryPath = "/usr/local/share/OpenCV/java/libopencv_java330.so";
	/**
	 * Path that is used when all other attempts have failed. Defaults to the folder where opencv was built.
	 */
	private static final String fallbackPath = System.getenv("HOME") + "/opencv/build/lib/libopencv_java330.so";

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	/**
	 * Load OpenCV native library with default arguments.
	 */
	public static void load() {
		load(libraryPath);
	}

	/**
	 * Load OpenCV native library from a specific location.
	 * 
	 * @param nativeLibraryPath - the location of OpenCV native library
	 */
	public static void load(String nativeLibraryPath) {
		if (!loaded) {
			loaded = loadFromLibrarypath(nativeLibraryPath);
			// loaded = true;
		} else {
			logger.debug("Native library already loaded.");
		}
	}

	private static boolean loadFromLibrarypath(String nativeLibraryPath) {
		try {
			logger.debug("java.library.path: {}", System.getProperty("java.library.path"));
			System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
			return true;
		} catch (UnsatisfiedLinkError e) {
			logger.info("Unable to load the library from java.library.path! Using absolute path instead ({})", nativeLibraryPath);
			return loadFromAbsolutePath(nativeLibraryPath);
		}
	}

	private static boolean loadFromAbsolutePath(String nativeLibraryPath) {
		try {
			System.load(nativeLibraryPath);
			return true;
		} catch (UnsatisfiedLinkError e) {
			logger.info("Unable to load the library from {}. Using fallback path ({})", nativeLibraryPath, fallbackPath);
			return loadFromAlternatePath();
		}
	}

	private static boolean loadFromAlternatePath() throws RuntimeException {
		try {
			System.load(fallbackPath);
			return true;
		} catch (UnsatisfiedLinkError e) {
			throw new RuntimeException("Unable to load OpenCV native library", e);
		}
	}
}
