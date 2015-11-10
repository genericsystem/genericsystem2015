package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.List;

import javafx.collections.ObservableList;

import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class ObservableListTest extends AbstractTest {

	@Test(invocationCount = 5)
	public void test001_ObservableList() throws InterruptedException {
		HeavyClientEngine engine = new HeavyClientEngine();
		assert engine == engine.adjustMeta();
		ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
		if (dependenciesObservableList.isEmpty())
			Thread.sleep(100);
		assert !dependenciesObservableList.isEmpty();
	}

	// should it work or not?
	public void test002_ConcurrentTryFlush() throws InterruptedException, ConcurrencyControlException {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		HeavyClientEngine engine2 = new HeavyClientEngine();
		engine1.addInstance("car");
		engine1.getCurrentCache().getDependenciesObservableList(engine1);
		engine1.getCurrentCache().tryFlush();
	}

	public void test003_ConcurrentShiftTs() throws InterruptedException, ConcurrencyControlException {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		HeavyClientEngine engine2 = new HeavyClientEngine();
		engine1.addInstance("car");
		ObservableList<Generic> dependenciesObservableList1 = engine1.getCurrentCache().getDependenciesObservableList(engine1);
		engine1.getCurrentCache().tryFlush();
		assert engine2.getCurrentCache().shiftTs() >= engine1.getCurrentCache().getTs();
		ObservableList<Generic> dependenciesObservableList2 = engine2.getCurrentCache().getDependenciesObservableList(engine2);
		Thread.sleep(1000);
		assert dependenciesObservableList1.size() == dependenciesObservableList2.size() : dependenciesObservableList1 + "\t:\t" + dependenciesObservableList2;
		for (int i = 0; i < dependenciesObservableList1.size(); i++) {
			assert dependenciesObservableList1.get(i).genericEquals(dependenciesObservableList2.get(i));
		}
	}

	public void test003_ConcurrentShiftTsOnInstances() throws InterruptedException, ConcurrencyControlException {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		HeavyClientEngine engine2 = new HeavyClientEngine();
		Generic car = engine1.addInstance("car");
		List<Generic> genericCars = new ArrayList<>();
		Generic myCar1 = car.addInstance("myCar1");
		genericCars.add(myCar1);
		Generic myCar2 = car.addInstance("myCar2");
		genericCars.add(myCar2);
		Generic myCar3 = car.addInstance("myCar3");
		genericCars.add(myCar3);
		Generic myCar4 = car.addInstance("myCar4");
		genericCars.add(myCar4);
		Generic myCar5 = car.addInstance("myCar5");
		genericCars.add(myCar5);
		engine1.getCurrentCache().flush();
		engine2.getCurrentCache().shiftTs();
		ObservableList<Generic> dependencies = engine2.getCurrentCache().getDependenciesObservableList(car);
		if (dependencies.isEmpty())
			Thread.sleep(100);
		assert dependencies.containsAll(genericCars);
		assert genericCars.containsAll(dependencies);

	}

	public void test003_Iterate() throws InterruptedException {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("car");
		List<Generic> genericCars = new ArrayList<>();
		Generic myCar1 = car.addInstance("myCar1");
		genericCars.add(myCar1);
		Generic myCar2 = car.addInstance("myCar2");
		genericCars.add(myCar2);
		Generic myCar3 = car.addInstance("myCar3");
		genericCars.add(myCar3);
		Generic myCar4 = car.addInstance("myCar4");
		genericCars.add(myCar4);
		Generic myCar5 = car.addInstance("myCar5");
		genericCars.add(myCar5);
		ObservableList<Generic> dependenciesObservableList1 = engine.getCurrentCache().getDependenciesObservableList(car);
		engine.getCurrentCache().flush();

		assert dependenciesObservableList1.size() == 5;
		assert genericCars.containsAll(dependenciesObservableList1);
		assert dependenciesObservableList1.containsAll(genericCars);

	}
}
