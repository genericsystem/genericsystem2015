package org.genericsystem.distributed.cacheonserver;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.LightClientEngine;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	public void test000() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		root.close();
	}

}
