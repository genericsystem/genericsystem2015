package org.genericsystem.distributed.cacheonclient;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	public void test000() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		root.close();
	}

}
