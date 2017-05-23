package org.genericsystem.cv;

import javafx.application.Platform;
import javafx.beans.value.ObservableValue;
import javafx.scene.image.ImageView;

public class AwareImageView extends ImageView {

	public AwareImageView(ObservableValue<Img> observableImg) {
		observableImg.addListener((o, ov, nv) -> {
			Platform.runLater(new Runnable() {
				@Override
				public void run() {
					setImage(observableImg.getValue().getImageView().getImage());
					System.gc();
					System.runFinalization();
				}
			});
		});
		if (observableImg.getValue() != null)
			setImage(observableImg.getValue().getImageView().getImage());
		else
			System.out.println("no image to display");
	}

}