package org.genericsystem.spring;

import java.io.Serializable;

import org.genericsystem.common.Generic;
import org.genericsystem.models.Count;
import org.junit.Test;

public class PersistenceTest extends AbstractTest {
	@Test
	public void testCount() {
		Generic count = engine.find(Count.class);
		if (count.getInstances().isEmpty()) {
			count.addInstance(0);
			engine.getCurrentCache().flush();
		}
		Generic actualValue = count.getInstances().first();
		Serializable previousValue = actualValue.getValue();
		actualValue.updateValue((int) actualValue.getValue() + 1);
		log.info("previousValue {}, actualValue {}.", previousValue, actualValue.getValue());
		engine.getCurrentCache().flush();
		engine.close();
	}

}
