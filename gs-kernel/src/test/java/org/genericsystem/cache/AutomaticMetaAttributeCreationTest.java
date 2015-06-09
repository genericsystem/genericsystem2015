package org.genericsystem.cache;

import org.testng.annotations.Test;

@Test
public class AutomaticMetaAttributeCreationTest extends AbstractTest {

	public void test001_addInstance_metaAttribute() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		assert metaAttribute != null;
		assert engine.getLevel() == 0;
		assert metaAttribute.getLevel() == 0;
		assert metaAttribute.inheritsFrom(engine) : metaAttribute.info();
	}

	public void test002_addInstance_metaAttribute() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		assert power.getMeta() == metaAttribute;
		assert power.isInstanceOf(metaAttribute) : power.info();
	}

	public void test003_addInstance_metaAttribute_override() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic option = engine.addInstance("Option", vehicle);
		Generic power = engine.addInstance(option, "Power", vehicle);

		assert power.getMeta() == metaAttribute;
		assert power.isInstanceOf(metaAttribute) : power.info();
	}

	public void test004_addInstance_metaAttribute_override() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic power2 = engine.addInstance(power, "Power2", vehicle);

		assert power2.getMeta() == metaAttribute;
		assert power2.isInstanceOf(metaAttribute) : power2.info();
		assert power2.getSupers().stream().anyMatch(superGeneric -> superGeneric.equals(power)) : power2.info();
	}

	public void test005_setInstance_metaAttribute_engine() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		assert engine.getLevel() == 0;
		assert metaAttribute.getLevel() == 0;
		assert metaAttribute.inheritsFrom(engine) : metaAttribute.info();
	}

	public void test006_setInstance_metaAttribute_attribute() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		assert power.getMeta() == metaAttribute;
		assert power.isInstanceOf(metaAttribute) : power.info();
	}

	public void test007_setInstance_metaAttribute_override() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic option = engine.addInstance("Option", vehicle);
		Generic power = engine.addInstance(option, "Power", vehicle);

		assert power.getMeta() == metaAttribute;
		assert power.isInstanceOf(metaAttribute) : power.info();
	}

	public void test008_setInstance_metaAttribute_override() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic power2 = engine.addInstance(power, "Power2", vehicle);

		assert power2.getMeta() == metaAttribute;
		assert power2.isInstanceOf(metaAttribute) : power2.info();
		assert power2.getSupers().stream().anyMatch(superGeneric -> superGeneric.equals(power)) : power2.info();
	}
}
