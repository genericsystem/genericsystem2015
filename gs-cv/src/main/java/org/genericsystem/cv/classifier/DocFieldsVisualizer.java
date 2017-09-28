package org.genericsystem.cv.classifier;

import java.nio.file.Paths;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.opencv.core.Scalar;

import javafx.scene.layout.GridPane;

public class DocFieldsVisualizer extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int row = 0;
		int col = 0;
		final String filename = System.getenv("HOME") + "/genericsystem/gs-ir-files/converted-png/image-1.png";

		Img original = new Img(filename);
		Img deskewed = Deskewer.deskew(Paths.get(filename));
		Img annotated = Deskewer.getRotatedRectanglesDrawn(original, new Scalar(0, 0, 255), 4);

		Img fieldsDrawn = ClassifierUsingFields.getFieldsDrawn(deskewed, new Scalar(0, 0, 255), 4);

		DocClassFields docClass = new DocClassFields();
		docClass.merge(deskewed);
		docClass.consolidateOcr(deskewed);
		Img test = docClass.drawOcr(deskewed, new Scalar(0, 0, 255), 2);

		// mainGrid.add(annotated.getImageView(), col++, row);
		// mainGrid.add(deskewed.getImageView(), col++, row);
		mainGrid.add(fieldsDrawn.getImageView(), col++, row);
		mainGrid.add(test.getImageView(), col++, row);

		original.close();
		annotated.close();
		deskewed.close();
		fieldsDrawn.close();
		test.close();
	}

}
