package org.genericsystem.cv.nn;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.writable.IntWritable;
import org.datavec.api.writable.Writable;
import org.datavec.image.recordreader.BaseImageRecordReader;

public class ImageClassRecordReader extends BaseImageRecordReader {

	private static final long serialVersionUID = -1990731291095750954L;

	public ImageClassRecordReader(int height, int width, int channels, ParentPathLabelGenerator labelMaker) {
		super(height, width, channels, labelMaker);
	}

	@Override
	public boolean batchesSupported() {
		return false;
	}

	@Override
	public List<Writable> next() {
		if (iter != null) {
			List<Writable> ret = new ArrayList<>();
			File image = iter.next();
			currentFile = image;

			if (image.isDirectory())
				return next();
			try {
				invokeListeners(image);
				ret.add(new IntWritable(labels.indexOf(getLabel(image.getPath()))));
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
