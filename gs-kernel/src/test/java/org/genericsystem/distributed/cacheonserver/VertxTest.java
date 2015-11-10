package org.genericsystem.distributed.cacheonserver;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.CosClientEngine;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	public void test000() {
		CosClientEngine root = new CosClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		root.close();
	}

}
