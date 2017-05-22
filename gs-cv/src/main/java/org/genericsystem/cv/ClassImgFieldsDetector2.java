package org.genericsystem.cv;

import org.opencv.core.Core;

import javafx.beans.value.ObservableValue;
import javafx.scene.layout.GridPane;

public class ClassImgFieldsDetector2 extends AbstractApp {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "classes/id-fr-front";

	public static void main(String[] args) {
		launch(args);
	}

	@Override
	protected void fillGrid(GridPane mainGrid) {

		ImgClass2 imgClass = ImgClass2.fromDirectory(null, classImgRepertory);

		ObservableValue<Img> observableMean = imgClass.getObservableMean();
		Img model = observableMean.getValue();
		ObservableValue<Img> observableVariance = imgClass.getObservableVariance();

		mainGrid.add(new AwareImageView(observableMean), 0, 0);
		mainGrid.add(new AwareImageView(observableVariance), 1, 0);

		mainGrid.add(new ClassImgsBoard(imgClass, model), 0, 1, 2, 1);

	}

}