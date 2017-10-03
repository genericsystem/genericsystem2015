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
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone;
import org.genericsystem.cv.classifier.newmodel.SimpleModel.Zone.ZoneInstance;
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
	private static final Long timestamp = 123L;

	@BeforeClass
	public void init() {
		engine = new Engine(Doc.class, DocInstance.class, Zone.class, ZoneInstance.class, Consolidated.class, ConsolidatedInstance.class, DocPath.class, DocPathInstance.class, DocTimestamp.class, DocTimestampInstance.class);
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
		ZoneInstance zone2 = doc1.setZone(rect2);
		ZoneInstance zone3 = doc1.getZone(rect1);
		Snapshot<ZoneInstance> zones = doc1.getZoneInstances();

		assertEquals(zone1.getDocInstance(), doc1); // Composition OK
		assertEquals(zone3, zone1); // Getter ok
		assertTrue(zones.containsAll(Arrays.asList(zone1, zone2))); // Snapshot OK
		assertEquals(zone2.getZoneRect(), rect2); // Instance value ok
	}

	@Test
	public void testDocPath() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		DocPathInstance dpi = doc1.setDocPath(docPath1);

		assertEquals(dpi.getDocInstance(), doc1); // Composition OK
		assertEquals(doc1.getDocPath(), dpi); // Getter OK
	}

	@Test
	public void testDocTimestamp() {
		Doc doc = engine.find(Doc.class);
		DocInstance doc1 = doc.setDoc(filename1);
		DocTimestampInstance dti = doc1.setDocTimestamp(timestamp);

		assertEquals(dti.getDocInstance(), doc1); // Composition OK
		assertEquals(doc1.getDocTimestamp(), dti); // Getter OK
	}
}
