package org.genericsystem.cv.nn;

import java.io.File;
import java.io.IOException;

import org.nd4j.linalg.dataset.MultiDataSet;
import org.nd4j.linalg.dataset.api.MultiDataSetPreProcessor;
import org.nd4j.linalg.dataset.api.iterator.MultiDataSetIterator;

// Adaptation of ExistingMiniBatchDataSetIterator for MultiDataSets.
public class ExistingMiniBatchMultiDataSetIterator implements MultiDataSetIterator {
	public static final String DEFAULT_PATTERN = "dataset-%d.bin";

	private int currIdx;
	private File rootDir;
	private int totalBatches = -1;
	private MultiDataSetPreProcessor multiDataSetPreProcessor;
	private final String pattern;

	/**
	 * Create with the given root directory, using the default filename pattern {@link #DEFAULT_PATTERN}
	 * 
	 * @param rootDir
	 *            the root directory to use
	 */
	public ExistingMiniBatchMultiDataSetIterator(File rootDir) {
		this(rootDir, DEFAULT_PATTERN);
	}

	/**
	 *
	 * @param rootDir
	 *            The root directory to use
	 * @param pattern
	 *            The filename pattern to use. Used with {@code String.format(pattern,idx)}, where idx is an integer, starting
	 *            at 0.
	 */
	public ExistingMiniBatchMultiDataSetIterator(File rootDir, String pattern) {
		this.rootDir = rootDir;
		rootDir.mkdirs();
		totalBatches = rootDir.list().length;
		this.pattern = pattern;
	}

	@Override
	public MultiDataSet next(int num) {
		throw new UnsupportedOperationException("Unable to load custom number of examples");
	}

	@Override
	public boolean resetSupported() {
		return true;
	}

	@Override
	public boolean asyncSupported() {
		return true;
	}

	@Override
	public void reset() {
		currIdx = 0;
	}

	@Override
	public void setPreProcessor(MultiDataSetPreProcessor preProcessor) {
		this.multiDataSetPreProcessor = preProcessor;
	}

	@Override
	public MultiDataSetPreProcessor getPreProcessor() {
		return multiDataSetPreProcessor;
	}

	@Override
	public boolean hasNext() {
		return currIdx < totalBatches;
	}

	@Override
	public void remove() {
		// no opt;
	}

	@Override
	public MultiDataSet next() {
		try {
			MultiDataSet ret = read(currIdx);
			if (multiDataSetPreProcessor != null)
				multiDataSetPreProcessor.preProcess(ret);
			currIdx++;

			return ret;
		} catch (IOException e) {
			throw new IllegalStateException("Unable to read dataset");
		}
	}

	private MultiDataSet read(int idx) throws IOException {
		File path = new File(rootDir, String.format(pattern, idx));
		MultiDataSet d = new MultiDataSet();
		d.load(path);
		return d;
	}
}
