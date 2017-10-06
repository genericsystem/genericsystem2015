package org.genericsystem.cv.classifier.newmodel;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.RollbackException;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ConsolidatedType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ConsolidatedType.ConsolidatedInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocClassType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocClassType.DocClassInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocType.DocInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgDocRel;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgDocRel.ImgDocLink;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgPathType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgPathType.ImgPathInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgTimestampType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgTimestampType.ImgTimestampInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.LayoutType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.LayoutType.LayoutInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNumType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNumType.ZoneNumInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneType;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneType.ZoneInstance;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Rect;
import org.testng.annotations.AfterClass;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

/**
 * Simple test class to test the {@link SimpleModel} definitions.
 * 
 * @author Pierrik Lassalas
 */
public class SimpleModelTest {

	private Engine engine;
	private static final String docClass1 = "id-fr-front";
	private static final String docClass2 = "id-fr-back";
	private static final String lay1 = "layout1";
	private static final String lay2 = "layout2";
	private static final String lay3 = "layout3";
	private static final String lay4 = "layout4";
	private static final String lay5 = "layout5";
	private static final String link1 = "link1";
	private static final String filename1 = "document1.png";
	private static final String filename2 = "document2.png";
	private static final String docPath1 = "/path/to/file";
	private static final String docPath2 = "/alternate/path/to/file";
	private static final Long timestamp1 = 123L;
	private static final Long timestamp2 = 321L;

	@BeforeClass
	public void init() {
		engine = new Engine(DocClassType.class, DocClassInstance.class, LayoutType.class, LayoutInstance.class, ImgDocRel.class, ImgDocLink.class, DocType.class, DocInstance.class, ImgType.class, ImgInstance.class, ZoneType.class, ZoneInstance.class,
				ZoneNumType.class, ZoneNumInstance.class, ConsolidatedType.class, ConsolidatedInstance.class, ImgPathType.class, ImgPathInstance.class, ImgTimestampType.class, ImgTimestampInstance.class);
	}

	@AfterClass
	public void stop() {
		engine.close();
	}

	@AfterMethod
	public void afterMethod() {
		engine.getCurrentCache().clear();
	}

	@Test
	public void testDocClass() {
		DocClassType docClassType = engine.find(DocClassType.class);
		DocClassInstance instance1 = docClassType.addDocClass(docClass1);
		DocClassInstance instance2 = docClassType.getDocClass(docClass1);
		DocClassInstance instance3 = docClassType.addDocClass(docClass2);
		Snapshot<DocClassInstance> instances = docClassType.getAllDocClasses();

		assertEquals(instance1, instance2); // Getter OK
		assertEquals(instance1.getValue(), docClass1); // Instance value OK
		assertTrue(instances.containsAll(Arrays.asList(instance1, instance3))); // Snapshot OK

		try {
			docClassType.addDocClass(docClass1);
		} catch (Exception e) {
			assertTrue(e instanceof RollbackException);
		}
	}

	@Test
	public void testDocClassDoc() {
		DocClassType docClassType = engine.find(DocClassType.class);
		DocClassInstance docClass = docClassType.addDocClass(docClass1);
		DocInstance doc1 = docClass.addDocInstance(filename1);
		DocInstance doc3 = docClass.getDocInstance(filename1);
		DocInstance doc4 = docClass.addDocInstance(filename2);
		Snapshot<DocInstance> docs = docClass.getAllDocInstances();

		assertEquals(doc1, doc3); // Getter OK
		assertEquals(doc1.getValue(), filename1); // Instance value OK
		assertTrue(docs.containsAll(Arrays.asList(doc1, doc4))); // Snapshot OK

		try {
			docClass.addDocInstance(filename1);
		} catch (Exception e) {
			assertTrue(e instanceof RollbackException);
		}
	}

	@Test
	public void testDocClassLayout() {
		DocClassType docClassType = engine.find(DocClassType.class);
		DocClassInstance docClass = docClassType.addDocClass(docClass1);
		LayoutInstance layout1 = docClass.addLayout(lay1);
		LayoutInstance layout2 = docClass.getLayout(lay1);
		LayoutInstance layout3 = docClass.addLayout(lay2);
		Snapshot<LayoutInstance> layouts = docClass.getAllLayouts();

		assertEquals(layout1, layout2); // Getter OK
		assertEquals(layout1.getValue(), lay1); // Instance value OK
		assertTrue(layouts.containsAll(Arrays.asList(layout1, layout3))); // Snapshot OK

		try {
			docClass.addLayout(lay1);
		} catch (Exception e) {
			assertTrue(e instanceof RollbackException);
		}
	}

	@Test
	public void testDocClassLayoutInheritance() {
		DocClassType docClassType = engine.find(DocClassType.class);
		DocClassInstance docClass = docClassType.addDocClass(docClass1);
		LayoutInstance root = docClass.addLayout(lay1);
		LayoutInstance parent1 = docClass.addLayout(lay2, root);
		LayoutInstance parent2 = docClass.addLayout(lay3, root);
		LayoutInstance child1 = docClass.addLayout(lay4, parent1);
		LayoutInstance child2 = docClass.addLayout(lay5, parent1);
		Snapshot<LayoutInstance> all = docClass.getAllLayouts();
		Snapshot<LayoutInstance> leaves = docClass.getAllLayoutLeaves();
		LayoutInstance layoutRoot = docClass.getLayoutRoot();

		assertTrue(all.containsAll(Arrays.asList(root, parent1, parent2, child1, child2)));
		assertTrue(leaves.containsAll(Arrays.asList(parent2, child1, child2)));
		assertFalse(leaves.contains(root));
		assertFalse(leaves.contains(parent1));
		assertEquals(layoutRoot, root);
	}

	@Test
	public void testLayout() {
		DocClassType docClassType = engine.find(DocClassType.class);
		DocClassInstance docClass = docClassType.addDocClass(docClass1);
		LayoutInstance root = docClass.addLayout(lay1);

		assertEquals(root.getDocClassInstance(), docClass); // Composition OK
	}

	@Test
	public void testDoc() {
		DocClassType docClassType = engine.find(DocClassType.class);
		DocClassInstance docClass = docClassType.addDocClass(docClass1);
		DocInstance doc = docClass.addDocInstance(filename1);

		assertEquals(doc.getDocClassInstance(), docClass); // Composition OK
	}

	@Test
	public void testImgDocRel() {
		DocClassType docClassType = engine.find(DocClassType.class);
		ImgType imgType = engine.find(ImgType.class);
		ImgDocRel imgDocRel = engine.find(ImgDocRel.class);
		DocClassInstance docClass = docClassType.addDocClass(docClass1);
		DocInstance doc = docClass.addDocInstance(filename1);
		ImgInstance img = imgType.addImg(filename1);
		ImgDocLink link = (ImgDocLink) imgDocRel.setInstance(link1, img, doc);

		// XXX finish
		// assertEquals(doc.getDocClassInstance(), docClass); // Composition OK
	}

	@Test
	public void testImg() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.addImg(filename1);
		ImgInstance doc2 = imgType.getImg(filename1);
		ImgInstance doc3 = imgType.addImg(filename2);
		Snapshot<ImgInstance> docs = imgType.getImgInstances();

		assertEquals(doc1, doc2); // Getter OK
		assertEquals(doc1.getValue(), filename1); // Instance value OK
		assertTrue(docs.containsAll(Arrays.asList(doc1, doc3))); // Snapshot OK

		try {
			imgType.addImg(filename1);
		} catch (Exception e) {
			assertTrue(e instanceof RollbackException);
		}
	}

	@Test
	public void testZone() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.addImg(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		Rect rect2 = new Rect(20, 20, 50, 150);
		ZoneInstance zone1 = doc1.addZone(rect1);
		ZoneInstance zone2 = doc1.addZone(rect2);
		ZoneInstance zone3 = doc1.getZone(rect1);
		zone1.setConsolidated(filename1);
		Snapshot<ZoneInstance> zones = doc1.getZoneInstances();
		Snapshot<ZoneInstance> emptyZones = doc1.getEmptyZoneInstances();

		assertEquals(zone1.getImgInstance(), doc1); // Composition OK
		assertEquals(zone3, zone1); // Getter ok
		assertTrue(zones.containsAll(Arrays.asList(zone1, zone2))); // Snapshot OK
		assertTrue(emptyZones.contains(zone2));
		assertFalse(emptyZones.contains(zone1));
		assertEquals(zone2.getZoneRect(), rect2); // Instance value ok

		try {
			doc1.addZone(rect1);
		} catch (Exception e) {
			assertTrue(e instanceof RollbackException);
		}
	}

	@Test
	public void testConsolidated() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.addImg(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		ZoneInstance zone1 = doc1.addZone(rect1);
		ConsolidatedInstance string1 = zone1.setConsolidated(filename1);

		assertEquals(string1.getZoneInstance(), zone1); // Composition OK
		assertEquals(zone1.getConsolidated(), string1); // Getter OK

		ConsolidatedInstance string2 = zone1.setConsolidated(filename2);

		assertNotEquals(zone1.getConsolidated(), string1);
		assertEquals(zone1.getConsolidated(), string2); // PropertyConstraint
	}

	@Test
	public void testZoneNum() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.addImg(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		ZoneInstance zone1 = doc1.addZone(rect1);
		ZoneNumInstance num1 = zone1.setZoneNum(1);

		assertEquals(num1.getZoneInstance(), zone1); // Composition OK
		assertEquals(zone1.getZoneNum(), num1); // Getter OK

		ZoneNumInstance num2 = zone1.setZoneNum(2);

		assertNotEquals(zone1.getZoneNum(), num1);
		assertEquals(zone1.getZoneNum(), num2); // PropertyConstraint
	}

	@Test
	public void testDocPath() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.addImg(filename1);
		ImgPathInstance dpi1 = doc1.setImgPath(docPath1);

		assertEquals(dpi1.getImgInstance(), doc1); // Composition OK
		assertEquals(doc1.getImgPath(), dpi1); // Getter OK

		ImgPathInstance dpi2 = doc1.setImgPath(docPath2);

		assertNotEquals(doc1.getImgPath(), dpi1);
		assertEquals(doc1.getImgPath(), dpi2); // PropertyConstraint
	}

	@Test
	public void testDocTimestamp() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.addImg(filename1);
		ImgTimestampInstance dti1 = doc1.setImgTimestamp(timestamp1);

		assertEquals(dti1.getImgInstance(), doc1); // Composition OK
		assertEquals(doc1.getImgTimestamp(), dti1); // Getter OK

		ImgTimestampInstance dti2 = doc1.setImgTimestamp(timestamp2);

		assertNotEquals(doc1.getImgTimestamp(), dti1);
		assertEquals(doc1.getImgTimestamp(), dti2); // PropertyConstraint
	}

}
