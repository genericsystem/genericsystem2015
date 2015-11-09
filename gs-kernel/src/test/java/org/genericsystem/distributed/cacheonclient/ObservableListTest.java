package org.genericsystem.distributed.cacheonclient;

import java.util.List;

import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class ObservableListTest extends AbstractTest {

	@Test(invocationCount = 10)
	public void test001_ObservableList() throws InterruptedException {
		HeavyClientEngine engine = new HeavyClientEngine();
		assert engine == engine.adjustMeta();
		ObservableValue<List<Generic>> dependenciesObservableList = engine.getCurrentCache().getDependenciesObservableList(engine);
		if (dependenciesObservableList.getValue().isEmpty()) {
			Thread.sleep(100);
			if (dependenciesObservableList.getValue().isEmpty()) {
				Thread.sleep(100);
				assert !dependenciesObservableList.getValue().isEmpty();
			}
		}
	}

}
