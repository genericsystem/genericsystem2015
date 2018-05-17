package org.genericsystem.cv.application;

import org.genericsystem.cv.AbstractApp;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Scalar;
import org.opencv.imgcodecs.Imgcodecs;

import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;

public class MeshgridRadonTest extends AbstractApp {

	public static void main(String[] args) {
		launch(args);
	}

	static {
		NativeLibraryLoader.load();
	}
	private Img image = new Img(Imgcodecs.imread("1_in.png"), false);
	private int cols = 3;
	private int rows = 2;

	@Override
	protected void fillGrid(GridPane mainGrid) {
		Image[] images = new Image[6];
		images[0] = image.toJfxImage();

		MeshGridRadon meshGrid = new MeshGridRadon(20, 40, 40, image.getSrc());
		meshGrid.build(-5000, -45, 45);
		try (Img vertDirs = new Img(meshGrid.drawVerticalDirs(new Scalar(255, 0, 255)), false);
				Img baseLines = new Img(meshGrid.drawBaselines(new Scalar(255, 0, 0)), false);
				Img grid = new Img(meshGrid.drawOnCopy(new Scalar(0, 0, 255)), false);
				Img surface = new Img(meshGrid.draw3Dsurface(new Scalar(0, 255, 0), new Scalar(0, 0, 255)), false);
				Img dewarped = new Img(meshGrid.dewarp(), false)) {
			images[1] = baseLines.toJfxImage();
			images[2] = vertDirs.toJfxImage();
			images[3] = grid.toJfxImage();
			images[4] = surface.toJfxImage();
			images[5] = dewarped.toJfxImage();
			for (int col = 0; col < cols; col++)
				for (int row = 0; row < rows; row++) {
					int i = row * cols + col;
					if (i < images.length) {
						ImageView imageView = new ImageView(images[i]);
						mainGrid.add(imageView, col, row);
						imageView.setPreserveRatio(true);
						imageView.setFitWidth(image.width());
						imageView.setFitHeight(image.height());
					}
				}
		}
	}
}
