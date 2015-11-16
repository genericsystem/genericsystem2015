package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
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
		ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getWrappableDependencies(engine);
		if (dependenciesObservableList.size() == 0) {
			Thread.sleep(100);
		}
		assert dependenciesObservableList.size() != 0;
	}

	@Test(invocationCount = 5)
	public void test002_ConcurrentTryFlush() throws InterruptedException, ConcurrencyControlException {
		CocClientEngine engine1 = new CocClientEngine();
		CocClientEngine engine2 = new CocClientEngine();
		engine2.addInstance("elephant");
		Wrappable<Generic> wrap = engine2.getCurrentCache().getWrappableDependencies(engine1);
		engine2.getCurrentCache().tryFlush();
		if (wrap.size() == 0)
			Thread.sleep(100);
		assert wrap.size() != 0;
		for (int i = 0; i < wrap.size(); i++)
			wrap.get(i);
	}

	@Test(invocationCount = 5)
	public void test003_ListenerOnAddInstance() throws InterruptedException {
		CocClientEngine engine = new CocClientEngine();
		Generic car = engine.setInstance("car");

		engine.getCurrentCache().asyncMount();

		Wrappable<Generic> test = engine.getCurrentCache().getWrappableDependencies(car);
		ObservableList<Generic> binding = FXCollections.observableArrayList();
		Bindings.bindContent(binding, test);

		Generic myBMW = car.addInstance("myBMW");
		Generic myVolksWagen = car.addInstance("myVolksWagen");
		if (test.size() == 0)
			Thread.sleep(100);
		assert test.size() != 0;
		assert binding.contains(myBMW);
		assert binding.contains(myVolksWagen);
		assert binding.equals(test);

	}

	@Test(invocationCount = 5)
	public void test004_ListenerOnRemoveInstance() throws InterruptedException {
		CocClientEngine engine = new CocClientEngine();
		Generic car = engine.setInstance("car");

		engine.getCurrentCache().asyncMount();

		Wrappable<Generic> test = engine.getCurrentCache().getWrappableDependencies(car);
		ObservableList<Generic> binding = FXCollections.observableArrayList();
		Bindings.bindContent(binding, test);

		Generic myBMW = car.addInstance("myBMW");
		Generic myVolksWagen = car.addInstance("myVolksWagen");
		if (test.size() <= 1)
			Thread.sleep(100);
		assert test.size() != 0;
		assert binding.contains(myBMW);
		assert binding.contains(myVolksWagen);
		assert binding.equals(test);

		engine.getCurrentCache().remove(myBMW);
		engine.getCurrentCache().remove(myVolksWagen);
		if (test.size() != 0)
			Thread.sleep(100);
		assert test.size() == 0;
		assert !binding.contains(myBMW);
		assert !binding.contains(myVolksWagen);
		assert binding.equals(test);
		//
		// engine.getCurrentCache().clear();
		// if (test.size() != 0)
		// Thread.sleep(100);
		// assert test.size() == 0;
		// assert binding.isEmpty();
	}

	@Test(invocationCount = 5)
	public void test005_ListenerFlushAndRemove() throws InterruptedException {
		// TODO flush with the mount, test will contain [mybmw, myvolkswagen, mybmw, myvolkswagen].
		// flush without the mount, test will only contain [mybmw, myvolkswagen].
		// don't mount, and binding will be empty
		CocClientEngine engine = new CocClientEngine();
		Generic car = engine.setInstance("car");

		engine.getCurrentCache().asyncMount();

		Wrappable<Generic> test = engine.getCurrentCache().getWrappableDependencies(car);
		ObservableList<Generic> binding = FXCollections.observableArrayList();
		Bindings.bindContent(binding, test);

		Generic myBMW = car.addInstance("myBMW");
		Generic myVolksWagen = car.addInstance("myVolksWagen");

		System.out.println("flush");
		engine.getCurrentCache().flush();
		Thread.sleep(100);

		assert test.size() == 2 : "binding::" + binding + "\ttest::" + test;
		assert binding.equals(test) : "binding::" + binding + "\ttest::" + test;

		myBMW.remove();
		myVolksWagen.remove();
		if (test.size() != 0)
			Thread.sleep(100);

		assert test.size() == 0 : "binding::" + binding + "\ttest::" + test;
		assert binding.equals(test) : "binding::" + binding + "\ttest::" + test;
		//
		// engine.getCurrentCache().clear();
		// if (test.size() != 0)
		// Thread.sleep(100);
		// assert test.size() == 0;
		// assert binding.isEmpty();
	}

	@Test(invocationCount = 5)
	public void test006_Contains() throws InterruptedException {
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
		ObservableList<Generic> dependenciesObservableList1 = engine.getCurrentCache().getWrappableDependencies(car);
		engine.getCurrentCache().flush();
		if (dependenciesObservableList1.isEmpty())
			Thread.sleep(100);
		assert dependenciesObservableList1.size() == 5 : dependenciesObservableList1.size();
		for (int i = 0; i < dependenciesObservableList1.size(); i++)
			assert genericCars.get(i).equals(dependenciesObservableList1.get(i));
	}
}
