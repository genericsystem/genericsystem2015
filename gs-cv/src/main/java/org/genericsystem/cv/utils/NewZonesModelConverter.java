package org.genericsystem.cv.utils;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.comparator.FillModelWithData;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.opencv.core.Rect;

public class NewZonesModelConverter {
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model/";

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static void main(String[] args) {
		Root root = FillModelWithData.getEngine(gsPath);

		DocClass docClass = root.find(DocClass.class);

		Snapshot<DocClassInstance> docClasses = (Snapshot) docClass.getInstances();

		docClasses.forEach(currentDocClass -> {
			System.out.println("--- Current docClass: " + currentDocClass);

			Snapshot<ZoneInstance> zoneInstances = currentDocClass.getZones();
			zoneInstances.forEach(currentZone -> {
				System.out.println("current zone: " + currentZone);
				int temp = (int) currentZone.getValue();
				Generic zoneNum = currentZone.setZoneNum(temp);

				Rect rect = currentZone.getRect();
				System.out.println("rect: " + rect);

				long start = System.nanoTime();
				currentZone.updateValue(ModelTools.generateZoneUID(rect));
				long stop = System.nanoTime();
				System.out.println("--------- update: " + (stop - start) / 1_000_000 + "ms");
			});
			root.getCurrentCache().flush();
		});

		docClasses.forEach(currentDocClass -> {
			System.out.println("--- Current docClass: " + currentDocClass);

			Snapshot<ZoneInstance> zoneInstances = currentDocClass.getZones();
			zoneInstances.forEach(currentZone -> {
				System.out.println("current zone: " + currentZone);
				System.out.println("value: " + currentZone.getValue());
			});
		});

		root.close();
	}
}
