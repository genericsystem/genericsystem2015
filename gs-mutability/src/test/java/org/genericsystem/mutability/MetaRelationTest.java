package org.genericsystem.mutability;
//package org.genericsystem.kernel;
//
//import java.util.Collections;
//
//import org.testng.annotations.Test;
//
//@Test
//public class MetaRelationTest extends AbstractTest {
//
//	public void test001_setMetaAttribute_engineEngine() {
//
//		Root engine = new Root();
//		Vertex metaAttribute = engine.setMetaAttribute();
//		Vertex metaRelation = engine.setMetaAttribute(Collections.singletonList(engine));
//		assert metaRelation.getMeta() == metaAttribute;
//		assert metaRelation.inheritsFrom(metaAttribute);
//	}
//
//	public void test002_setMetaAttribute_relation() {
//
//		Root engine = new Root();
//		Vertex metaAttribute = engine.setMetaAttribute();
//		Vertex metaRelation = engine.setMetaAttribute(Collections.singletonList(engine));
//		Vertex car = engine.addInstance("Car");
//		Vertex power = engine.addInstance("Power", car);
//		Vertex color = engine.addInstance("Color");
//		Vertex carColor = engine.addInstance("carColor", new Vertex[] { car, color });
//		assert carColor.isInstanceOf(metaRelation);
//		assert power.isInstanceOf(metaAttribute);
//	}
// }
