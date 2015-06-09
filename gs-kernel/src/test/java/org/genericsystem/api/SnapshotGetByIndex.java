package org.genericsystem.api;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.kernel.AbstractTest;
import org.genericsystem.kernel.Generic;
import org.genericsystem.kernel.Root;
import org.testng.annotations.Test;

@Test
public class SnapshotGetByIndex extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myMercedes = car.addInstance("myMercedes");
		Snapshot<Generic> snapshot = car.getSubInstances();
		assert snapshot.getByIndex(0).equals(myBmw);
		assert snapshot.getByIndex(1).equals(myAudi);
		assert snapshot.getByIndex(2).equals(myMercedes);
		assert snapshot.getByIndex(12) == null;
	}

}
