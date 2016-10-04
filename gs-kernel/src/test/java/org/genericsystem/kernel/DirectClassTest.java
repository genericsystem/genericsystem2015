package org.genericsystem.kernel;

import org.genericsystem.api.core.annotations.DirectClass;
import org.genericsystem.api.core.annotations.InstanceClass;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class DirectClassTest extends AbstractTest {
	public void test001() {
		Engine engine = new Engine(Vehicle1.class);
		assert engine.find(Vehicle1.class) instanceof Vehicle1;
	}

	public void test002() {
		Engine engine = new Engine(Vehicle2.class);
		assert engine.find(Vehicle2.class) instanceof Vehicle2;
	}

	public void test003() {
		Engine engine = new Engine(Vehicle3.class);
		assert engine.find(Vehicle3.class) instanceof Vehicle3;
	}

	public void test004() {
		Engine engine = new Engine(Vehicle4.class);
		assert engine.find(Vehicle4.class) instanceof Vehicle4;
	}

	public void test005() {
		Engine engine = new Engine(Vehicle5.class);
		assert !(engine.find(Vehicle5.class) instanceof Vehicle5);
	}

	public void test006() {
		Engine engine = new Engine(Vehicle6.class);
		assert engine.find(Vehicle6.class) instanceof Vehicle6;
		assert engine.find(Vehicle6.class) instanceof Generic;
	}

	public void test007() {
		Engine engine = new Engine(Car.class, CarStandard.class, MyAudi.class, MyBmw.class);
		assert engine.find(MyBmw.class) instanceof CarStandard;
		assert engine.find(MyAudi.class) instanceof MyAudi;
	}

	public void test008() {
		Engine engine = new Engine(CarStandard.class);
		System.out.println(engine.find(CarStandard.class).getClass().getName());
		CarStandard cs = engine.find(CarStandard.class);
	}

	public void test009() {
		catchAndCheckCause(() -> new Engine(MyBmw2.class), IllegalStateException.class);
		catchAndCheckCause(() -> new Engine(MyBmw3.class), IllegalStateException.class);
	}

	@SystemGeneric
	public static interface Vehicle1 extends Generic {

	}

	@DirectClass
	@SystemGeneric
	public static interface Vehicle2 {

	}

	@SystemGeneric
	public static class Vehicle3 implements Generic {

	}

	@DirectClass
	@SystemGeneric
	public static class Vehicle4 {
	}

	@SystemGeneric
	public static class Vehicle5 {

	}

	@DirectClass
	@SystemGeneric
	public static class Vehicle6 implements Generic {

	}

	@SystemGeneric
	@Meta(Car.class)
	public static class Bmw implements Generic {

	}

	@InstanceClass(CarStandard.class)
	@SystemGeneric
	public static interface Car {

	}

	@SystemGeneric
	@Meta(Car.class)
	public static class CarStandard {

	}

	@SystemGeneric
	@Meta(Car.class)
	public static class MyBmw extends CarStandard {
	}

	@SystemGeneric
	@Meta(Car.class)
	public static class MyBmw2 {
	}

	@SystemGeneric
	@Meta(Car.class)
	public static class MyBmw3 extends Bmw {
	}

	@DirectClass
	@SystemGeneric
	@Meta(Car.class)
	public static class MyAudi extends CarStandard {

	}

}
