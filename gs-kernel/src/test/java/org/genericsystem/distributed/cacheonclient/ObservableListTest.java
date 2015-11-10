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
		CocClientEngine engine = new CocClientEngine();
		assert engine == engine.adjustMeta();
		ObservableList<Generic> dependenciesObservableList = new AbstractWrappable.WrappableImpl<Generic>(engine.getCurrentCache().getWrappableDependencies(engine));

		if (dependenciesObservableList.isEmpty())
			Thread.sleep(100);
		assert !dependenciesObservableList.isEmpty();
		System.out.println(dependenciesObservableList);
	}

	public void test002_ConcurrentTryFlush() throws InterruptedException, ConcurrencyControlException {
		CocClientEngine engine1 = new CocClientEngine();
		CocClientEngine engine2 = new CocClientEngine();
		engine2.addInstance("elephant");
		engine2.getCurrentCache().getWrappableDependencies(engine1);
		engine2.getCurrentCache().tryFlush();
	}

	// public void test003_ConcurrentShiftTs() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine1 = new CocClientEngine();
	// CocClientEngine engine2 = new CocClientEngine();
	// engine2.setInstance("car");
	// ObservableList<Generic> dependenciesObservableList2 = engine2.getCurrentCache().getDependenciesObservableList(engine2);
	// engine2.getCurrentCache().tryFlush();
	// assert engine1.getCurrentCache().shiftTs() >= engine2.getCurrentCache().getTs();
	// ObservableList<Generic> dependenciesObservableList1 = engine1.getCurrentCache().getDependenciesObservableList(engine1);
	// if (dependenciesObservableList1.isEmpty())
	// Thread.sleep(100);
	// assert dependenciesObservableList1.size() == dependenciesObservableList2.size() : dependenciesObservableList1 + "\t:\t" + dependenciesObservableList2;
	// compareGraph(engine1, engine2);
	// }

	// public void test002_ShiftTs() throws ConcurrencyControlException {
	// CocClientEngine engine = new CocClientEngine();
	// Generic elephant = engine.addInstance("elephant");
	// engine.getCurrentCache().shiftTs();
	// assert !engine.getCurrentCache().getDependenciesObservableList(engine).contains(elephant);
	// }
	//
	// // naturally empty, engines aren't linked to one another
	// public void test003_ConcurrentShiftTsOnInstances() throws InterruptedException, ConcurrencyControlException {
	// CocClientEngine engine1 = new CocClientEngine();
	// CocClientEngine engine2 = new CocClientEngine();
	// Generic car = engine1.addInstance("car");
	// List<Generic> genericCars = new ArrayList<>();
	// Generic myCar1 = car.addInstance("myCar1");
	// genericCars.add(myCar1);
	// Generic myCar2 = car.addInstance("myCar2");
	// genericCars.add(myCar2);
	// Generic myCar3 = car.addInstance("myCar3");
	// genericCars.add(myCar3);
	// Generic myCar4 = car.addInstance("myCar4");
	// genericCars.add(myCar4);
	// Generic myCar5 = car.addInstance("myCar5");
	// genericCars.add(myCar5);
	// engine1.getCurrentCache().flush();
	// engine2.getCurrentCache().shiftTs();
	// ObservableList<Generic> dependencies = engine2.getCurrentCache().getDependenciesObservableList(car);
	// if (dependencies.isEmpty())
	// Thread.sleep(100);
	// assert dependencies.containsAll(genericCars);
	// assert genericCars.containsAll(dependencies);
	//
	// }

	@Test(invocationCount = 5)
	public void test003_Contains() throws InterruptedException {
		CocClientEngine engine = new CocClientEngine();
		Generic car = engine.setInstance("car");
		List<Generic> genericCars = new ArrayList<>();
		Generic myCar1 = car.setInstance("myCar1");
		genericCars.add(myCar1);
		Generic myCar2 = car.setInstance("myCar2");
		genericCars.add(myCar2);
		Generic myCar3 = car.setInstance("myCar3");
		genericCars.add(myCar3);
		Generic myCar4 = car.setInstance("myCar4");
		genericCars.add(myCar4);
		Generic myCar5 = car.setInstance("myCar5");
		genericCars.add(myCar5);
		ObservableList<Generic> dependenciesObservableList1 = new AbstractWrappable.WrappableImpl<Generic>(engine.getCurrentCache().getWrappableDependencies(car));
		engine.getCurrentCache().flush();
		if (dependenciesObservableList1.isEmpty())
			Thread.sleep(100);
		assert dependenciesObservableList1.size() == 5 : dependenciesObservableList1.size();
		for (int i = 0; i < dependenciesObservableList1.size(); i++)
			assert genericCars.get(i).equals(dependenciesObservableList1.get(i));
	}
}
