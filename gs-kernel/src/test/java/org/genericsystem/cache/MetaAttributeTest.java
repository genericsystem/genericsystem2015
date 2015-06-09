//package org.genericsystem.kernel;
//
//import org.testng.annotations.Test;
//
//@Test
//public class MetaAttributeTest extends AbstractTest {
//
//	public void test001_addInstance_metaAttribute() {
//		Root engine = new Root();
//		// Vertex metaAttribute = engine.setInstance(engine.getValue(), engine);
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		assert engine.getLevel() == 0;
//		assert metaAttribute.getLevel() == 0;
//		assert metaAttribute.inheritsFrom(engine) : metaAttribute.info();
//	}
//
//	public void test002_addInstance_metaAttribute() {
//		Root engine = new Root();
//		// Vertex metaAttribute = engine.setInstance(engine.getValue(), engine);
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		Vertex vehicle = engine.addInstance("Vehicle");
//		Vertex power = engine.addInstance("Power", vehicle);
//		assert power.getMeta() == metaAttribute;
//		assert power.isInstanceOf(metaAttribute) : power.info();
//	}
//
//	public void test003_addInstance_metaAttribute_override() {
//		Root engine = new Root();
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		Vertex vehicle = engine.addInstance("Vehicle");
//		Vertex option = engine.addInstance("Option", vehicle);
//		Vertex power = engine.addInstance(option, "Power", vehicle);
//
//		assert power.getMeta() == metaAttribute;
//		assert power.isInstanceOf(metaAttribute) : power.info();
//	}
//
//	public void test004_addInstance_metaAttribute_override() {
//		Root engine = new Root();
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		Vertex vehicle = engine.addInstance("Vehicle");
//		Vertex power = engine.addInstance("Power", vehicle);
//		Vertex power2 = engine.addInstance(power, "Power2", vehicle);
//
//		assert power2.getMeta() == metaAttribute;
//		assert power2.isInstanceOf(metaAttribute) : power2.info();
//		assert power2.getSupersStream().anyMatch(superVertex -> superVertex.equals(power)) : power2.info();
//	}
//
//	public void test005_setInstance_metaAttribute_engine() {
//		Root engine = new Root();
//		Vertex metaAttribute = engine.setInstance(engine.getValue(), engine);
//		assert engine.getLevel() == 0;
//		assert metaAttribute.getLevel() == 0;
//		assert metaAttribute.inheritsFrom(engine) : metaAttribute.info();
//	}
//
//	public void test006_setInstance_metaAttribute_attribute() {
//		Root engine = new Root();
//		// Vertex metaAttribute = engine.setInstance(engine.getValue(), engine);
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		Vertex vehicle = engine.addInstance("Vehicle");
//		Vertex power = engine.addInstance("Power", vehicle);
//		assert power.getMeta() == metaAttribute;
//		assert power.isInstanceOf(metaAttribute) : power.info();
//	}
//
//	public void test007_setInstance_metaAttribute_override() {
//		Root engine = new Root();
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		Vertex vehicle = engine.addInstance("Vehicle");
//		Vertex option = engine.addInstance("Option", vehicle);
//		Vertex power = engine.addInstance(option, "Power", vehicle);
//
//		assert power.getMeta() == metaAttribute;
//		assert power.isInstanceOf(metaAttribute) : power.info();
//	}
//
//	public void test008_setInstance_metaAttribute_override() {
//		Root engine = new Root();
//		Vertex metaAttribute = engine.addInstance(engine.getValue(), engine);
//		Vertex vehicle = engine.addInstance("Vehicle");
//		Vertex power = engine.addInstance("Power", vehicle);
//		Vertex power2 = engine.addInstance(power, "Power2", vehicle);
//
//		assert power2.getMeta() == metaAttribute;
//		assert power2.isInstanceOf(metaAttribute) : power2.info();
//		assert power2.getSupersStream().anyMatch(superVertex -> superVertex.equals(power)) : power2.info();
//	}
// }
