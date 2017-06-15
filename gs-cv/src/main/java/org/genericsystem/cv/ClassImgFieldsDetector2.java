package org.genericsystem.cv;

import org.opencv.core.Core;

import javafx.scene.layout.GridPane;

public class ClassImgFieldsDetector2 extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "classes/passport-fr";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {
		ImgClass2 imgClass = ImgClass2.fromDirectory(null, classImgRepertory);
		mainGrid.add(new AwareImageView(imgClass.getObservableMean()), 0, 0);
		mainGrid.add(new AwareImageView(imgClass.getObservableVariance()), 1, 0);
		mainGrid.add(new ClassImgBoard(imgClass), 0, 1, 2, 1);
	}

}