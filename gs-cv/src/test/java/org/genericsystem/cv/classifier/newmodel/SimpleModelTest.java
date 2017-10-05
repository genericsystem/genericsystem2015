package org.genericsystem.cv.classifier.newmodel;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertNotEquals;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
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
	public void testDoc() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.setImg(filename1);
		ImgInstance doc2 = imgType.setImg(filename1);
		ImgInstance doc3 = imgType.getImg(filename1);
		ImgInstance doc4 = imgType.setImg(filename2);
		Snapshot<ImgInstance> docs = imgType.getImgInstances();

		assertEquals(doc1, doc2); // No duplicates
		assertEquals(doc1, doc3); // Getter OK
		assertEquals(doc1.getValue(), filename1); // Instance value OK
		assertTrue(docs.containsAll(Arrays.asList(doc1, doc4))); // Snapshot OK
	}

	@Test
	public void testZone() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.setImg(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		Rect rect2 = new Rect(20, 20, 50, 150);
		ZoneInstance zone1 = doc1.setZone(rect1);
		ZoneInstance zone2 = doc1.setZone(rect1);
		ZoneInstance zone3 = doc1.setZone(rect2);
		ZoneInstance zone4 = doc1.getZone(rect1);
		Snapshot<ZoneInstance> zones = doc1.getZoneInstances();

		assertEquals(zone1.getImgInstance(), doc1); // Composition OK
		assertEquals(zone1, zone2); // No duplicates
		assertEquals(zone4, zone1); // Getter ok
		assertTrue(zones.containsAll(Arrays.asList(zone1, zone3))); // Snapshot OK
		assertEquals(zone3.getZoneRect(), rect2); // Instance value ok
	}

	@Test
	public void testConsolidated() {
		ImgType imgType = engine.find(ImgType.class);
		ImgInstance doc1 = imgType.setImg(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		ZoneInstance zone1 = doc1.setZone(rect1);
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
		ImgInstance doc1 = imgType.setImg(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		ZoneInstance zone1 = doc1.setZone(rect1);
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
		ImgInstance doc1 = imgType.setImg(filename1);
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
		ImgInstance doc1 = imgType.setImg(filename1);
		ImgTimestampInstance dti1 = doc1.setImgTimestamp(timestamp1);

		assertEquals(dti1.getImgInstance(), doc1); // Composition OK
		assertEquals(doc1.getImgTimestamp(), dti1); // Getter OK

		ImgTimestampInstance dti2 = doc1.setImgTimestamp(timestamp2);

		assertNotEquals(doc1.getImgTimestamp(), dti1);
		assertEquals(doc1.getImgTimestamp(), dti2); // PropertyConstraint
	}

}
