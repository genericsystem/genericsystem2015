package org.genericsystem.cv;

import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.Random;

import org.apache.commons.io.FilenameUtils;
import org.datavec.api.io.filters.BalancedPathFilter;
import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.split.FileSplit;
import org.datavec.api.split.InputSplit;
import org.datavec.image.loader.BaseImageLoader;
import org.datavec.image.recordreader.ImageRecordReader;
import org.datavec.image.transform.ImageTransform;
import org.deeplearning4j.datasets.datavec.RecordReaderDataSetIterator;
import org.deeplearning4j.datasets.iterator.AsyncDataSetIterator;
import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration;
import org.deeplearning4j.earlystopping.saver.LocalFileGraphSaver;
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculatorCG;
import org.deeplearning4j.earlystopping.termination.MaxEpochsTerminationCondition;
import org.deeplearning4j.earlystopping.termination.ScoreImprovementEpochTerminationCondition;
import org.deeplearning4j.earlystopping.trainer.EarlyStoppingGraphTrainer;
import org.deeplearning4j.eval.Evaluation;
import org.deeplearning4j.nn.conf.GradientNormalization;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.nn.transferlearning.FineTuneConfiguration;
import org.deeplearning4j.nn.transferlearning.TransferLearning;
import org.deeplearning4j.nn.transferlearning.TransferLearningHelper;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.optimize.listeners.ScoreIterationListener;
import org.deeplearning4j.util.ModelSerializer;
import org.deeplearning4j.zoo.PretrainedType;
import org.deeplearning4j.zoo.ZooModel;
import org.deeplearning4j.zoo.model.VGG16;
import org.nd4j.jita.conf.CudaEnvironment;
import org.nd4j.linalg.activations.Activation;
import org.nd4j.linalg.dataset.ExistingMiniBatchDataSetIterator;
import org.nd4j.linalg.dataset.api.DataSet;
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator;
import org.nd4j.linalg.dataset.api.preprocessor.DataNormalization;
import org.nd4j.linalg.dataset.api.preprocessor.ImagePreProcessingScaler;
import org.nd4j.linalg.lossfunctions.LossFunctions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AdaptedVGG16 {
	private static final Logger log = LoggerFactory.getLogger(AdaptedVGG16.class);

	private static String featurizedLayer = "block5_pool";

	public static void main(String[] args) throws Exception {
		System.setProperty("org.bytedeco.javacpp.maxphysicalbytes", "10G");

		CudaEnvironment.getInstance().getConfiguration()
		.setMaximumDeviceCacheableLength(1024 * 1024 * 1024L)
		.setMaximumDeviceCache(6L * 1024 * 1024 * 1024L)
		.setMaximumHostCacheableLength(1024 * 1024 * 1024L)
		.setMaximumHostCache(6L * 1024 * 1024 * 1024L);

		double learningRate = 0.1;
		int batchSize = 2;
		int nEpochs = 30;
		int height = 224;
		int width = 224;
		int channels = 3;
		int seed = 123;
		String[] allowedExtensions = BaseImageLoader.ALLOWED_FORMATS;
		Random randNumGen = new Random(seed);

		File parentDir = new File(System.getProperty("user.dir"), "training");
		FileSplit filesInDir = new FileSplit(parentDir, allowedExtensions, randNumGen);
		ParentPathLabelGenerator labelMaker = new ParentPathLabelGenerator();
		BalancedPathFilter pathFilter = new BalancedPathFilter(randNumGen, allowedExtensions, labelMaker, 0, 0, 10, 0);

		InputSplit[] filesInDirSplit = filesInDir.sample(pathFilter, 70, 15, 15);
		InputSplit trainData = filesInDirSplit[0];
		InputSplit validData = filesInDirSplit[1];
		InputSplit testData = filesInDirSplit[2];

		ImageRecordReader recordReader = new ImageRecordReader(height, width, channels, labelMaker);
		recordReader.initialize(trainData, null);
		List<String> labels = recordReader.getLabels();
		int outputNum = recordReader.numLabels();

		// Until version 0.8.0 of deeplearning4j
		//		TrainedModelHelper modelImportHelper = new TrainedModelHelper(TrainedModels.VGG16);
		//		ComputationGraph vgg16 = modelImportHelper.loadModel();
		// From version 0.8.1-SNAPSHOT
		ZooModel zooModel = new VGG16();
		ComputationGraph vgg16 = (ComputationGraph) zooModel.initPretrained(PretrainedType.IMAGENET);

		FineTuneConfiguration fineTuneConfig = new FineTuneConfiguration.Builder()
				.gradientNormalization(GradientNormalization.RenormalizeL2PerLayer)
				.learningRate(learningRate)
				.build();

		ComputationGraph net = new TransferLearning.GraphBuilder(vgg16)
				.fineTuneConfiguration(fineTuneConfig)
				.setFeatureExtractor(featurizedLayer)
				.removeVertexKeepConnections("predictions")
				//				.addLayer("fc3", new DenseLayer.Builder()
				//						.activation(Activation.RELU)
				//						.weightInit(WeightInit.RELU)
				//						.nIn(4096).nOut(2048).build(), "fc2")
				.addLayer("predictions", 
						new OutputLayer.Builder(LossFunctions.LossFunction.MCXENT)
						.nIn(4096).nOut(outputNum)
						.weightInit(WeightInit.RELU)
						.activation(Activation.SOFTMAX).build(), "fc2")
				.build();

		String tempDir = System.getProperty("java.io.tmpdir");
		String saveDirectory = FilenameUtils.concat(tempDir, "EarlyStoppingIntermediaryResults/");
		Paths.get(saveDirectory).toFile().mkdirs();

		TransferLearningHelper transferLearningHelper = new TransferLearningHelper(net, featurizedLayer);
		ComputationGraph graph = transferLearningHelper.unfrozenGraph();

		// Stats visualisation on http://localhost:9000/train, and print score every 10th iteration.
		//		UIServer uiServer = UIServer.getInstance();
		//		StatsStorage statsStorage = new InMemoryStatsStorage();
		//		uiServer.attach(statsStorage);
		graph.setListeners(/*new StatsListener(statsStorage), */new ScoreIterationListener(10));

		saveFeaturized(getDataSetIterator(recordReader, trainData, null, batchSize, outputNum), transferLearningHelper, "train");
		saveFeaturized(getDataSetIterator(recordReader, validData, null, batchSize, outputNum), transferLearningHelper, "validation");
		saveFeaturized(getDataSetIterator(recordReader, testData, null, batchSize, outputNum), transferLearningHelper, "test");

		EarlyStoppingConfiguration<ComputationGraph> esConf = new EarlyStoppingConfiguration.Builder<ComputationGraph>()
				.epochTerminationConditions(new MaxEpochsTerminationCondition(nEpochs))
				.evaluateEveryNEpochs(1)
				.epochTerminationConditions(new ScoreImprovementEpochTerminationCondition(20))
				.scoreCalculator(new DataSetLossCalculatorCG(getPresavedIterator("validation"), true))
				.modelSaver(new LocalFileGraphSaver(saveDirectory))
				.build();

		EarlyStoppingGraphTrainer trainer = new EarlyStoppingGraphTrainer(esConf, graph, getPresavedIterator("train"));
		trainer.fit();

		//		ImageTransform flipTransform1 = new FlipImageTransform(rng);
		// ImageTransform rotateTransform = new RotateImageTransform(rng, 20, 20, 100, 5);
		//		ImageTransform warpTransform = new WarpImageTransform(rng, 42);
		//		List<ImageTransform> transforms = Arrays.asList(new ImageTransform[] { warpTransform/* , rotateTransform */ });
		//		for (ImageTransform transform : transforms) {
		//			System.out.print("\nTraining on transformation: " + transform.getClass().toString() + "\n\n");
		//			dataIter = getDataSetIterator(recordReader, trainData, transform, batchSize, outputNum);
		//			trainer = new EarlyStoppingGraphTrainer(esConf, result.getBestModel(), dataIter);
		//			result = trainer.fit();
		//		}

		log.info("Model evaluation:");

		Evaluation eval = graph.evaluate(getPresavedIterator("test"), labels);
		log.info(eval.stats(true));

		File modelFile = new File("AdaptedVGG16-" + System.currentTimeMillis() + ".zip");
		ModelSerializer.writeModel(graph, modelFile, true);
		log.info("Model saved to " + modelFile);
	}

	public static DataSetIterator getPresavedIterator(String name) {
		System.out.println("format : " +  "images-" + featurizedLayer + "-" + name + "-%d.bin");
		DataSetIterator existingTestData = new ExistingMiniBatchDataSetIterator(new File(name + "Folder"), "images-" + featurizedLayer + "-" + name + "-%d.bin");
		DataSetIterator asyncTestIter = new AsyncDataSetIterator(existingTestData);
		return asyncTestIter;
	}

	private static void saveFeaturized(DataSetIterator dataIter, TransferLearningHelper transferLearningHelper, String name) {
		int dataSaved = 0;
		while(dataIter.hasNext()) {
			DataSet currentFeaturized = transferLearningHelper.featurize(dataIter.next());
			saveToDisk(currentFeaturized, dataSaved, name);
			dataSaved++;
		}
	}

	public static void saveToDisk(DataSet currentFeaturized, int iterNum, String name) {
		File fileFolder = new File(name + "Folder");
		if (iterNum == 0) {
			fileFolder.mkdirs();
		}
		String fileName = "images-" + featurizedLayer + "-" + name + "-" + iterNum + ".bin";
		currentFeaturized.save(new File(fileFolder, fileName));
		log.info("Saved " + name + "dataset #" + iterNum);
	}

	private static DataSetIterator getDataSetIterator(ImageRecordReader recordReader, InputSplit data, ImageTransform transform, int batchSize, int outputNum) {
		try {
			recordReader.initialize(data, transform);
		} catch (IOException e) {
			log.warn("Impossible to initialize recordReader", e);
		}
		DataSetIterator dataIter = new RecordReaderDataSetIterator(recordReader, batchSize, 1, outputNum);
		DataNormalization scaler = new ImagePreProcessingScaler(-1, 1);
		scaler.fit(dataIter);
		dataIter.setPreProcessor(scaler);
		return dataIter;
	}
}
