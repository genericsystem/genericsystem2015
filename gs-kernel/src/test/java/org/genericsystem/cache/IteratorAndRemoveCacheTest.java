package org.genericsystem.cache;

import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class IteratorAndRemoveCacheTest extends AbstractTest {

	public void test002_IterateAndRemove() {
		Engine engine = new Engine();
		Cache cache1 = engine.getCurrentCache();
		Cache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");

		cache2.flush();
		int cpt = 0;
		for (Generic g : car.getInstances()) {
			if (cpt % 2 == 0) {
				cache1.start();
				cache1.shiftTs();
				g.remove();
				cache1.flush();
			} else {
				cache2.start();
				cache2.shiftTs();
				g.remove();
				cache2.flush();
			}
			cpt++;
		}
		assert car.getInstances().size() == 0;
	}

	public void test001_() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		Cache cache1 = engine.getCurrentCache();
		cache1.flush();
		myCar1.remove();
		cache1.flush();
		Cache cache2 = engine.newCache().start();
		catchAndCheckCause(() -> myCar1.remove(), AliveConstraintViolationException.class);
		cache2.flush();
	}

	public void test002_() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Cache cache = engine.getCurrentCache();
		cache.flush();

		Cache cache2 = engine.newCache().start();
		myCar.remove();

		cache.start();
		cache.shiftTs();
		myCar.remove();
		cache.flush();
		cache2.start();
		catchAndCheckCause(() -> cache2.tryFlush(), OptimisticLockConstraintViolationException.class);
	}

	public void test003_IterateAndRemove() {
		Engine engine = new Engine();
		Cache cache1 = engine.getCurrentCache();
		Cache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		cache2.flush();
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");

		int cpt = 0;
		for (Generic g : car.getInstances()) {
			if (g.equals(myCar3))
				cache2.flush();
			if (cpt % 2 == 0) {
				cache1.start();
				cache1.shiftTs();
				g.remove();
				cache1.flush();
			} else {
				cache2.start();
				cache2.shiftTs();
				g.remove();
			}
			cpt++;
		}
		cache2.flush();
		assert car.getInstances().size() == 0;
		cache1.start();
		cache1.shiftTs();
		assert car.getInstances().size() == 0;
	}

	// public void test005_IterateAndRemove() {
	// Engine engine = new Engine();
	// Cache cache1 = engine.getCurrentCache();
	//
	// Generic car = engine.addInstance("Car");
	// Generic myCar1 = car.addInstance("myCar1");
	// Generic myCar2 = car.addInstance("myCar2");
	// Generic myCar3 = car.addInstance("myCar3");
	// cache1.flush();
	// Cache cache2 = engine.newCache().start();
	// cache1.start();
	// Generic myCar4 = car.addInstance("myCar4");
	//
	// Snapshot<Generic> myCars = car.getInstances();
	// Iterator<Generic> iterator = myCars.iterator();
	// int cpt = 0;
	// while (iterator.hasNext()) {
	// Generic g = iterator.next();
	// if (g.equals(myCar2)) {
	// cache2.start();
	// Generic myCar4Bis = car.addInstance("myCar4");
	// cache2.flush();
	// cache1.start();
	// }
	// cpt++;
	// System.out.println(g.detailedInfo());
	// }
	//
	// assert cpt == 4 : cpt;
	//
	// }

	public void test009_IterateAndAdd() {
		Engine engine = new Engine();
		Cache cache1 = engine.getCurrentCache();

		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");

		cache1.flush();

		Snapshot<Generic> myCars = car.getInstances();

		Iterator<Generic> iterator = myCars.iterator();
		Generic myCar5 = car.addInstance("myCar5");
		Generic myCar6 = car.addInstance("myCar6");

		int cpt = 0;
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}

		assert cpt == 6 : cpt;
	}

	// public void test010_IterateAndAdd() {
	// Stream<Integer> stream = StreamSupport.stream(Spliterators.spliteratorUnknownSize(new Iterator<Integer>() {
	//
	// @Override
	// public boolean hasNext() {
	// throw new IllegalStateException("call hasnext");
	// // return false;
	// }
	//
	// @Override
	// public Integer next() {
	// throw new IllegalStateException("call next");
	// // return null;
	// }
	// }, 0), false);
	// Stream.concat(Stream.empty(), stream).iterator();
	// // stream.iterator();
	// }

}
