package org.genericsystem.ir.reinforcer;

import java.lang.invoke.MethodHandles;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Root;
import org.genericsystem.cv.classifier.FillNewModelWithData;
import org.genericsystem.cv.newmodel.SimpleModel.ConsolidatedType.ConsolidatedInstance;
import org.genericsystem.cv.newmodel.SimpleModel.ImgType;
import org.genericsystem.cv.newmodel.SimpleModel.ImgType.ImgInstance;
import org.genericsystem.cv.newmodel.SimpleModel.ZoneType.ZoneInstance;
import org.opencv.core.Rect;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LabelsProvider {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv-newmodel";
	private static final Root root = FillNewModelWithData.getEngine(gsPath);

	public Labels getLabels(String imgName) {
		Stream<Label> stream = getImgZones(imgName);
		Labels labels = new Labels();
		stream.forEach(label -> labels.addLabel(label));
		return labels;
	}

	public Stream<Label> getImgZones(String name) {
		ImgType imgType = root.find(ImgType.class);
		ImgInstance imgInstance = imgType.getImg(name);
		if (imgInstance == null)
			return Stream.empty();
		Snapshot<ZoneInstance> zones = imgInstance.getConsolidatedZoneInstances();
		if (zones == null)
			return Stream.empty();
		return zones.stream().map(zi -> getLabelFromZone(zi));
	}

	private Label getLabelFromZone(ZoneInstance zoneInstance) {
		Rect rect = zoneInstance.getZoneRect();
		ConsolidatedInstance ci = zoneInstance.getConsolidated();
		String consolidated = ci == null ? "" : String.valueOf(ci.getValue());
		Label label = new Label(rect.tl().x, rect.tl().y, rect.br().x, rect.br().y, consolidated);
		return label;
	}

}
