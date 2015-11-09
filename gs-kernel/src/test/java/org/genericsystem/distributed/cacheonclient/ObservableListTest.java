package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class ObservableListTest extends AbstractTest {

	@Test(invocationCount = 10)
	public void test001_ObservableList() throws InterruptedException {
		HeavyClientEngine engine = new HeavyClientEngine();
		assert engine == engine.adjustMeta();
		ObservableList<Generic> dependenciesObservableList = engine.getCurrentCache().getDependenciesActualObservableList(engine);
		if (dependenciesObservableList.isEmpty())
			Thread.sleep(100);
		assert !dependenciesObservableList.isEmpty();
	}

}
