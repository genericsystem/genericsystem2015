//package org.genericsystem.spring;
//
//
//import org.genericsystem.common.AbstractCache;
//import org.genericsystem.common.Generic;
//import org.testng.annotations.Test;
//
//@Test
//public class CacheTest extends AbstractTest {
//
//	public void test001_flush() {
//		Generic vehicle = engine.addInstance("Vehicle");
//		assert vehicle.isAlive();
//		engine.getCurrentCache().flush();
//		assert vehicle.isAlive();
//		vehicle.remove();
//		engine.getCurrentCache().flush();
//		assert !vehicle.isAlive();
//	}
//
//	public void test001_clear() {
//		engine.addInstance("Vehicle");
//		engine.getCurrentCache().clear();
//		assert engine.getInstance("Vehicle") == null;
//	}
//
//	// public void test001_newCache_nostarted() {
//	// Cache currentCache = engine.getCurrentCache();
//	// engine.newCache().start();
//	// catchAndCheckCause(() -> currentCache.flush(), CacheNoStartedException.class);
//	// }
//
//	public void test001_mountNewCache() {
//		AbstractCache currentCache = engine.getCurrentCache();
//		currentCache.mount();
//		assert engine.getCurrentCache() == currentCache;
//		engine.addInstance("Vehicle");
//		currentCache.flush();
//		currentCache.unmount();
//		currentCache.clear();
//		assert engine.getInstance("Vehicle") == null;
//	}
//
//	public void test002_mountNewCache() {
//		AbstractCache currentCache = engine.getCurrentCache();
//		assert currentCache.getCacheLevel() == 0;
//		currentCache.mount();
//		Generic vehicle = engine.addInstance("Vehicle");
//		assert currentCache.getCacheLevel() == 1;
//		assert vehicle.isAlive();
//		currentCache.unmount();
//		assert !vehicle.isAlive();
//	}
// }
