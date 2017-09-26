package org.genericsystem.cv.classifier;

import java.nio.file.Paths;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.opencv.core.Scalar;

import javafx.scene.layout.GridPane;

public class DeskewVisualizer extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		int row = 0;
		int col = 0;
		final String filename = System.getenv("HOME") + "/genericsystem/gs-ir-files/converted-png/image-1.png";

		Img original = new Img(filename);
		// mainGrid.add(original.getImageView(), col++, row);

		Img deskewed = Deskewer.deskew(Paths.get(filename));
		// mainGrid.add(deskewed.getImageView(), col++, row);

		// col = 0;
		// row++;

		Img annotated = Deskewer.getRotatedRectanglesDrawn(original, new Scalar(0, 0, 255), 4);
		mainGrid.add(annotated.getImageView(), col++, row);

		Img annotated2 = Deskewer.getRotatedRectanglesDrawn(deskewed, new Scalar(0, 0, 255), 4);
		mainGrid.add(annotated2.getImageView(), col++, row);

		original.close();
		annotated.close();
		deskewed.close();
		annotated2.close();
	}

}
