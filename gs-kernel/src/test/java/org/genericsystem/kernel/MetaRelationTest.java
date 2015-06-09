package org.genericsystem.kernel;

import org.testng.annotations.Test;

@Test
public class MetaRelationTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic metaRelation = root.getMetaRelation();
		assert metaRelation != null;
		assert root.getLevel() == 0;
		assert metaRelation.getLevel() == 0;
		assert metaRelation.isMeta();
		assert metaRelation.getMeta() == metaRelation;
		assert metaRelation.inheritsFrom(root);
		assert metaRelation.isInstanceOf(root);
		assert metaRelation.isInstanceOf(metaRelation);
		assert !root.getInstances().contains(metaRelation);
		assert !root.getSubInstances().contains(metaRelation);
	}

	public void test002() {
		Root root = new Root();
		Generic metaRelation = root.getMetaRelation();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = root.addInstance("carColor", new Generic[] { car, color });
		assert carColor.getMeta() == metaRelation;
		assert carColor.isInstanceOf(metaRelation);
	}
}
