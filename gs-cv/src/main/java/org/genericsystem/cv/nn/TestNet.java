package org.genericsystem.cv.nn;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Random;

import org.datavec.api.io.filters.BalancedPathFilter;
import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.split.FileSplit;
import org.datavec.api.split.InputSplit;
import org.datavec.image.loader.BaseImageLoader;
import org.datavec.image.loader.NativeImageLoader;
import org.datavec.image.recordreader.ImageRecordReader;
import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.util.ModelSerializer;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.nd4j.linalg.api.ndarray.INDArray;
import org.nd4j.linalg.dataset.api.preprocessor.DataNormalization;
import org.nd4j.linalg.dataset.api.preprocessor.ImagePreProcessingScaler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestNet {
	private static final Logger log = LoggerFactory.getLogger(TestNet.class);

	static {
		NativeLibraryLoader.load();
	}

	// Predict result for an image.
	// Problem: Output array does not include labels.
	public static void classifyImage(File imageFile, List<String> labels) {
		ComputationGraph graph = getComputationGraph(new File("AdaptedVGG16/AdaptedVGG16-grouped-acc-1.zip"));

		NativeImageLoader loader = new NativeImageLoader(224, 224, 3);
		DataNormalization scaler = new ImagePreProcessingScaler(-1, 1);
		try {
			INDArray image = loader.asMatrix(imageFile);
			scaler.transform(image);
			log.info("Result: {}.", graph.outputSingle(image));
		} catch (IOException e) {
			throw new RuntimeException("Impossible to load image " + imageFile, e);
		}
	}

	public static void main(String[] args) {
		File parentDir = new File(System.getProperty("user.dir"), "data/training-grouped-augmented");
		FileSplit filesInDir = new FileSplit(parentDir, BaseImageLoader.ALLOWED_FORMATS);
		ParentPathLabelGenerator labelMaker = new ParentPathLabelGenerator();
		BalancedPathFilter pathFilter = new BalancedPathFilter(new Random(123), BaseImageLoader.ALLOWED_FORMATS, labelMaker, 0, 0, 100, 0);

		InputSplit[] filesInDirSplit = filesInDir.sample(pathFilter);
		InputSplit testData = filesInDirSplit[0];
		try (ImageRecordReader recordReader = new ImageRecordReader(224, 224, 3, labelMaker)) {
			recordReader.initialize(testData, null);
			classifyImage(new File("data/validation/id-fr-front/dimage-1.png"), recordReader.getLabels());
		} catch (IOException e) {
			log.error("Impossible to load data", e);
			return;
		}
	}

	public static ComputationGraph getComputationGraph(File modelFile) {
		try {
			return ModelSerializer.restoreComputationGraph(modelFile);
		} catch (IOException e) {
			throw new RuntimeException("Impossible to load model from disk.", e);
		}
	}
}
