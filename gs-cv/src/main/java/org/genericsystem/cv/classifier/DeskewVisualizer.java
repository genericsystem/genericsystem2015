package org.genericsystem.cv.classifier;

import java.nio.file.Paths;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.Deskewer;
import org.genericsystem.cv.utils.Deskewer.METHOD;
import org.opencv.core.Scalar;

import javafx.scene.layout.GridPane;

public class DeskewVisualizer extends AbstractApp {

	private int row = 0;
	private int col = 0;
	private final String filename = System.getenv("HOME") + "/genericsystem/gs-ir-files/converted-png/image-test2-0.png";
	private final Img original = new Img(filename);

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		showWithRectangles(mainGrid);
		col = 0;
		row++;
		showWithLines(mainGrid);
		original.close();
	}

	private void showWithRectangles(GridPane mainGrid) {
		Img deskewed = Deskewer.deskew(Paths.get(filename), METHOD.ROTADED_RECTANGLES);
		Img annotated = Deskewer.getRotatedRectanglesDrawn(original, new Scalar(0, 0, 255), 4);
		Img annotated2 = Deskewer.getRotatedRectanglesDrawn(deskewed, new Scalar(0, 0, 255), 4);

		mainGrid.add(annotated.getImageView(), col++, row);
		mainGrid.add(annotated2.getImageView(), col++, row);

		annotated.close();
		deskewed.close();
		annotated2.close();
	}

	private void showWithLines(GridPane mainGrid) {
		Img deskewed = Deskewer.deskew(Paths.get(filename), METHOD.HOUGH_LINES);
		Img annotated = Deskewer.getLinesDrawn(original, new Scalar(0, 0, 255), 4);
		Img annotated2 = Deskewer.getLinesDrawn(deskewed, new Scalar(0, 0, 255), 4);

		mainGrid.add(annotated.getImageView(), col++, row);
		mainGrid.add(annotated2.getImageView(), col++, row);

		annotated.close();
		deskewed.close();
		annotated2.close();
	}

}
