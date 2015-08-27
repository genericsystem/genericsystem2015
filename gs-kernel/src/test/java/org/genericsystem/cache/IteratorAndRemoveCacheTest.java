package org.genericsystem.cache;

import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.testng.annotations.Test;

@Test
public class IteratorAndRemoveCacheTest extends AbstractClassicTest {

	public void test002_IterateAndRemove() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache1 = engine.getCurrentCache();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		ClientGeneric myCar2 = car.addInstance("myCar2");
		ClientGeneric myCar3 = car.addInstance("myCar3");
		ClientGeneric myCar4 = car.addInstance("myCar4");

		cache2.flush();
		int cpt = 0;
		for (ClientGeneric g : car.getInstances()) {
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
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		ClientCache cache1 = engine.getCurrentCache();
		cache1.flush();
		myCar1.remove();
		cache1.flush();
		ClientCache cache2 = engine.newCache().start();
		catchAndCheckCause(() -> myCar1.remove(), AliveConstraintViolationException.class);
		cache2.flush();
	}

	public void test002_() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar = car.addInstance("myCar");
		ClientCache cache = engine.getCurrentCache();
		cache.flush();

		ClientCache cache2 = engine.newCache().start();
		myCar.remove();

		cache.start();
		cache.shiftTs();
		myCar.remove();
		cache.flush();
		cache2.start();

		try {
			cache2.tryFlush();
		} catch (ConcurrencyControlException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public void test003_IterateAndRemove() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache1 = engine.getCurrentCache();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		ClientGeneric myCar2 = car.addInstance("myCar2");
		cache2.flush();
		ClientGeneric myCar3 = car.addInstance("myCar3");
		ClientGeneric myCar4 = car.addInstance("myCar4");

		int cpt = 0;
		for (ClientGeneric g : car.getInstances()) {
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
		ClientEngine engine = new ClientEngine();
		ClientCache cache1 = engine.getCurrentCache();

		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		ClientGeneric myCar2 = car.addInstance("myCar2");
		ClientGeneric myCar3 = car.addInstance("myCar3");
		ClientGeneric myCar4 = car.addInstance("myCar4");

		cache1.flush();

		Snapshot<ClientGeneric> myCars = car.getInstances();

		Iterator<ClientGeneric> iterator = myCars.iterator();
		ClientGeneric myCar5 = car.addInstance("myCar5");
		ClientGeneric myCar6 = car.addInstance("myCar6");

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
