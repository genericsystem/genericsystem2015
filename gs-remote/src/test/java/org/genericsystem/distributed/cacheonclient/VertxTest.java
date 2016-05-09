package org.genericsystem.distributed.cacheonclient;

import org.genericsystem.common.Generic;
import org.genericsystem.remote.ClientEngine;
import org.testng.annotations.Test;

@Test
public class VertxTest extends AbstractTest {

	public void test000() {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		root.close();
	}

}
