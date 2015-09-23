package org.genericsystem.distributed;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	public void test000() {
		HeavyClientEngine root = new HeavyClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		root.close();
	}

}
