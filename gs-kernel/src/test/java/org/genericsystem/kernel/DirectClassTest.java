package org.genericsystem.kernel;

import org.genericsystem.api.core.annotations.DirectClass;
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
}
