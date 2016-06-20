package org.genericsystem.spring;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CacheTest extends AbstractTest {

	protected static Logger log = LoggerFactory.getLogger(CacheTest.class);

	@Test
	public void test001_flush() {
		Generic vehicle = engine.addInstance("Vehicle");
		assertTrue(vehicle.isAlive());
		engine.getCurrentCache().flush();
		assertTrue(vehicle.isAlive());
		vehicle.remove();
		engine.getCurrentCache().flush();
		assertFalse(vehicle.isAlive());
	}

	@Test
	public void test001_clear() {
		engine.addInstance("Vehicle");
		engine.getCurrentCache().clear();
		assertNull(engine.getInstance("Vehicle"));
	}

	// public void test001_newCache_nostarted() {
	// Cache currentCache = engine.getCurrentCache();
	// engine.newCache().start();
	// catchAndCheckCause(() -> currentCache.flush(), CacheNoStartedException.class);
	// }
	@Test
	public void test001_mountNewCache() {
		AbstractCache currentCache = engine.getCurrentCache();
		currentCache.mount();
		assertEquals(engine.getCurrentCache(), currentCache);
		engine.addInstance("Vehicle");
		currentCache.flush();
		currentCache.unmount();
		currentCache.clear();
		assertNull(engine.getInstance("Vehicle"));
	}

	@Test
	public void test002_mountNewCache() {
		AbstractCache currentCache = engine.getCurrentCache();
		assertEquals(currentCache.getCacheLevel(), 0);
		currentCache.mount();
		Generic vehicle = engine.addInstance("Vehicle");
		assertEquals(currentCache.getCacheLevel(), 1);
		assertTrue(vehicle.isAlive());
		currentCache.unmount();
		assertFalse(vehicle.isAlive());
	}

}
