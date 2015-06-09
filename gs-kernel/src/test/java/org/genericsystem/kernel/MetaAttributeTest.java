package org.genericsystem.kernel;

import org.testng.annotations.Test;

@Test
public class MetaAttributeTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic metaAttribute = root.getMetaAttribute();
		assert metaAttribute != null;
		assert root.getLevel() == 0;
		assert metaAttribute == root.setInstance(root.getValue(), root);
		assert metaAttribute.getLevel() == 0;
		assert metaAttribute.isMeta();
		assert metaAttribute.getMeta() == metaAttribute;
		assert metaAttribute.inheritsFrom(root) : metaAttribute.info();
		assert metaAttribute.isInstanceOf(root);
		assert metaAttribute.isInstanceOf(metaAttribute);
		assert !root.getInstances().contains(metaAttribute);
		assert !root.getSubInstances().contains(metaAttribute);
		assert metaAttribute.getBaseComponent().equals(root);
	}

	public void test002() {
		Root root = new Root();
		Generic metaAttribute = root.getMetaAttribute();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		assert power.getMeta() == metaAttribute;
		assert power.isInstanceOf(metaAttribute) : power.info();
	}

	public void test003() {
		Root root = new Root();
		Generic metaAttribute = root.getMetaAttribute();
		Generic vehicle = root.addInstance("Vehicle");
		Generic option = root.addInstance("Option", vehicle);
		Generic power = root.addInstance(option, "Power", vehicle);

		assert power.getMeta() == metaAttribute;
		assert power.isInstanceOf(metaAttribute) : power.info();
	}

	public void test004() {
		Root root = new Root();
		Generic metaAttribute = root.getMetaAttribute();
		Generic vehicle = root.addInstance("Vehicle");
		Generic options = root.addInstance("Options", vehicle);
		Generic music = root.addInstance(options, "Music", vehicle);

		assert music.getMeta() == metaAttribute;
		assert music.isInstanceOf(metaAttribute) : music.info();
		assert music.getSupers().stream().anyMatch(superVertex -> superVertex.equals(options)) : music.info();
	}
}
