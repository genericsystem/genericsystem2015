package org.genericsystem.cv.nn;

import java.io.File;
import java.util.List;

import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.util.ndarray.RecordConverter;
import org.datavec.api.writable.Writable;
import org.datavec.image.recordreader.BaseImageRecordReader;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.factory.Nd4j;
import org.opencv.core.Mat;
import org.opencv.core.Size;

public class ImageFeaturesRecordReader extends BaseImageRecordReader {
	static {
		NativeLibraryLoader.load();
	}

	private static final long serialVersionUID = 4031311017700609257L;

	public ImageFeaturesRecordReader(int height, int width, int channels, ParentPathLabelGenerator labelMaker, Mat vocabulary) {
		super(height, width, channels, labelMaker);
	}

	@Override
	public boolean batchesSupported() {
		return false;
	}

	@Override
	public List<Writable> next() {
		if (iter != null) {
			List<Writable> ret;
			File image = iter.next();
			currentFile = image;

			if (image.isDirectory())
				return next();
			try {
				invokeListeners(image);
				try (Img img0 = new Img(image.toString());
						Img img = img0.resize(new Size(width, height))) {
					INDArray row = Nd4j.create(img.getHogDescriptor().toArray());
					ret = RecordConverter.toRecord(row);
				}
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
			return ret;
		} else if (record != null) {
			hitImage = true;
			invokeListeners(record);
			return record;
		}
		throw new IllegalStateException("No more elements");
	}
}
