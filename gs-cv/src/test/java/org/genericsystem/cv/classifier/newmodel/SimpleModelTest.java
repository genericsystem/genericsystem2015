package org.genericsystem.cv.classifier.newmodel;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Consolidated;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Consolidated.ConsolidatedInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Doc;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Doc.DocInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocPath;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocPath.DocPathInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocTimestamp;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.DocTimestamp.DocTimestampInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgFilter;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone.ZoneInstance;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNum;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.ZoneNum.ZoneNumInstance;
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
		engine = new Engine(ImgFilter.class, ImgFilterInstance.class, Doc.class, DocInstance.class, Zone.class, ZoneInstance.class, ZoneNum.class, ZoneNumInstance.class, Consolidated.class, ConsolidatedInstance.class, DocPath.class, DocPathInstance.class,
				DocTimestamp.class, DocTimestampInstance.class);
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
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		DocInstance doc2 = doc.setDoc(filename1);
		DocInstance doc3 = doc.getDoc(filename1);
		DocInstance doc4 = doc.setDoc(filename2);
		Snapshot<DocInstance> docs = doc.getDocInstances();

		assertEquals(doc1, doc2); // No duplicates
		assertEquals(doc1, doc3); // Getter OK
		assertEquals(doc1.getValue(), filename1); // Instance value OK
		assertTrue(docs.containsAll(Arrays.asList(doc1, doc4))); // Snapshot OK
	}

	@Test
	public void testZone() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		Rect rect2 = new Rect(20, 20, 50, 150);
		ZoneInstance zone1 = doc1.setZone(rect1);
		ZoneInstance zone2 = doc1.setZone(rect1);
		ZoneInstance zone3 = doc1.setZone(rect2);
		ZoneInstance zone4 = doc1.getZone(rect1);
		Snapshot<ZoneInstance> zones = doc1.getZoneInstances();

		assertEquals(zone1.getDocInstance(), doc1); // Composition OK
		assertEquals(zone1, zone2); // No duplicates
		assertEquals(zone4, zone1); // Getter ok
		assertTrue(zones.containsAll(Arrays.asList(zone1, zone3))); // Snapshot OK
		assertEquals(zone3.getZoneRect(), rect2); // Instance value ok
	}

	@Test
	public void testConsolidated() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		ZoneInstance zone1 = doc1.setZone(rect1);
		ConsolidatedInstance string1 = zone1.setConsolidated(filename1);

		assertEquals(string1.getZoneInstance(), zone1); // Composition OK
		assertEquals(zone1.getConsolidated(), string1); // Getter OK

		ConsolidatedInstance string2 = zone1.setConsolidated(filename2);

		assertEquals(zone1.getConsolidated(), string2); // PropertyConstraint
	}

	@Test
	public void testZoneNum() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		Rect rect1 = new Rect(0, 0, 200, 100);
		ZoneInstance zone1 = doc1.setZone(rect1);
		ZoneNumInstance num1 = zone1.setZoneNum(1);

		assertEquals(num1.getZoneInstance(), zone1); // Composition OK
		assertEquals(zone1.getZoneNum(), num1); // Getter OK

		ZoneNumInstance num2 = zone1.setZoneNum(2);

		assertEquals(zone1.getZoneNum(), num2); // PropertyConstraint
	}

	@Test
	public void testDocPath() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		DocPathInstance dpi1 = doc1.setDocPath(docPath1);

		assertEquals(dpi1.getDocInstance(), doc1); // Composition OK
		assertEquals(doc1.getDocPath(), dpi1); // Getter OK

		DocPathInstance dpi2 = doc1.setDocPath(docPath2);

		// assertNotEquals(doc1.getDocPath(), dpi1);
		assertEquals(doc1.getDocPath(), dpi2); // PropertyConstraint
	}

	@Test
	public void testDocTimestamp() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		DocTimestampInstance dti1 = doc1.setDocTimestamp(timestamp1);

		assertEquals(dti1.getDocInstance(), doc1); // Composition OK
		assertEquals(doc1.getDocTimestamp(), dti1); // Getter OK

		DocTimestampInstance dti2 = doc1.setDocTimestamp(timestamp1);

		// assertNotEquals(doc1.getDocTimestamp(), dti1); // Why does this test fail?
		assertEquals(doc1.getDocTimestamp(), dti2); // PropertyConstraint
	}

	@Test
	public void testImgFilter() {
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ImgFilterInstance ifi1 = imgFilter.setImgFilter(filename1);
		ImgFilterInstance ifi2 = imgFilter.setImgFilter(filename1);
		ImgFilterInstance ifi3 = imgFilter.getImgFilter(filename1);
		ImgFilterInstance ifi4 = imgFilter.setImgFilter(filename2);
		Snapshot<ImgFilterInstance> filters = imgFilter.getImgFilters();

		assertEquals(ifi1, ifi2); // No duplicates
		assertEquals(ifi1, ifi3); // Getter OK
		assertEquals(ifi1.getValue(), filename1); // Instance value OK
		assertTrue(filters.containsAll(Arrays.asList(ifi1, ifi4))); // Snapshot OK
	}
}
