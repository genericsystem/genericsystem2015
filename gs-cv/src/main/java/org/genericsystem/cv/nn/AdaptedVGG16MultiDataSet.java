package org.genericsystem.cv.nn;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.datavec.api.io.filters.BalancedPathFilter;
import org.datavec.api.io.labels.ParentPathLabelGenerator;
import org.datavec.api.records.reader.RecordReader;
import org.datavec.api.split.FileSplit;
import org.datavec.api.split.InputSplit;
import org.datavec.image.loader.BaseImageLoader;
import org.datavec.image.recordreader.BaseImageRecordReader;
import org.datavec.image.recordreader.ImageRecordReader;
import org.deeplearning4j.datasets.datavec.RecordReaderMultiDataSetIterator;
import org.deeplearning4j.datasets.iterator.AsyncMultiDataSetIterator;
import org.deeplearning4j.earlystopping.EarlyStoppingConfiguration;
import org.deeplearning4j.earlystopping.saver.LocalFileGraphSaver;
import org.deeplearning4j.earlystopping.scorecalc.DataSetLossCalculatorCG;
import org.deeplearning4j.earlystopping.termination.MaxEpochsTerminationCondition;
import org.deeplearning4j.earlystopping.termination.ScoreImprovementEpochTerminationCondition;
import org.deeplearning4j.eval.Evaluation;
import org.deeplearning4j.nn.conf.GradientNormalization;
import org.deeplearning4j.nn.conf.layers.DenseLayer;
import org.deeplearning4j.nn.conf.layers.OutputLayer;
import org.deeplearning4j.nn.graph.ComputationGraph;
import org.deeplearning4j.nn.transferlearning.FineTuneConfiguration;
import org.deeplearning4j.nn.transferlearning.TransferLearning;
import org.deeplearning4j.nn.transferlearning.TransferLearningHelper;
import org.deeplearning4j.nn.weights.WeightInit;
import org.deeplearning4j.util.ModelSerializer;
import org.deeplearning4j.zoo.PretrainedType;
import org.deeplearning4j.zoo.ZooModel;
import org.deeplearning4j.zoo.model.VGG16;
import org.nd4j.jita.conf.CudaEnvironment;
import org.nd4j.linalg.activations.impl.ActivationLReLU;
import org.nd4j.linalg.activations.impl.ActivationSoftmax;
import org.nd4j.linalg.dataset.MultiDataSet;
import org.nd4j.linalg.dataset.api.iterator.MultiDataSetIterator;
import org.nd4j.linalg.dataset.api.preprocessor.MultiNormalizerMinMaxScaler;
import org.nd4j.linalg.lossfunctions.LossFunctions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class AdaptedVGG16MultiDataSet {
	private static final Logger log = LoggerFactory.getLogger(AdaptedVGG16MultiDataSet.class);
	private static int height = 224;
	private static int width = 224;
	private static int channels = 3;

	private static String featurizedLayer = "block5_pool";

	public static void main(String[] args) throws Exception {
		System.setProperty("org.bytedeco.javacpp.maxphysicalbytes", "8G");

		CudaEnvironment.getInstance().getConfiguration()
		.setMaximumDeviceCacheableLength(1024 * 1024 * 1024L)
		.setMaximumDeviceCache(6L * 1024 * 1024 * 1024L)
		.setMaximumHostCacheableLength(1024 * 1024 * 1024L)
		.setMaximumHostCache(6L * 1024 * 1024 * 1024L);

		double learningRate = 0.005;
		int batchSize = 4;
		int nEpochs = 100;
		int seed = 123;
		String[] allowedExtensions = BaseImageLoader.ALLOWED_FORMATS;
		Random randNumGen = new Random(seed);

		File parentDir = new File(System.getProperty("user.dir"), "training-grouped-augmented2");
		FileSplit filesInDir = new FileSplit(parentDir, allowedExtensions, randNumGen);
		ParentPathLabelGenerator labelMaker = new ParentPathLabelGenerator();
		BalancedPathFilter pathFilter = new BalancedPathFilter(randNumGen, allowedExtensions, null, 0, 0, 1000, 0);

		InputSplit[] filesInDirSplit = filesInDir.sample(pathFilter, .70, .15, .15);
		InputSplit trainData = filesInDirSplit[0];
		InputSplit validData = filesInDirSplit[1];
		InputSplit testData = filesInDirSplit[2];

		log.debug("trainData: {}, validData: {}, testData: {}", trainData.length(), validData.length(), testData.length());

		ImageRecordReader recordReader = new ImageRecordReader(height, width, channels);

		RecordReader featuresReader = new ImageFeaturesRecordReader(height, width, channels, null, null);

		BaseImageRecordReader outputReader = new ImageClassRecordReader(height, width, channels, labelMaker);
		outputReader.initialize(trainData);
		List<String> labels = outputReader.getLabels();
		int outputNum = outputReader.numLabels();

		// Until version 0.8.0 of deeplearning4j
		//		TrainedModelHelper modelImportHelper = new TrainedModelHelper(TrainedModels.VGG16);
		//		ComputationGraph vgg16 = modelImportHelper.loadModel();
		// From version 0.8.1-SNAPSHOT
		ZooModel zooModel = new VGG16();
		ComputationGraph vgg16 = (ComputationGraph) zooModel.initPretrained(PretrainedType.IMAGENET);

		FineTuneConfiguration fineTuneConfig = new FineTuneConfiguration.Builder()
				.gradientNormalization(GradientNormalization.RenormalizeL2PerLayer)
				.learningRate(learningRate)
				.regularization(true)
				.build();

		ComputationGraph net = new TransferLearning.GraphBuilder(vgg16)
				.fineTuneConfiguration(fineTuneConfig)
				.addInputs("features")
				.setFeatureExtractor(featurizedLayer)
				.removeVertexKeepConnections("predictions")
				.addLayer("fc3", new DenseLayer.Builder()
						.activation(new ActivationLReLU(0.33))
						.weightInit(WeightInit.RELU)
						.dropOut(0.5)
						.nIn(4096 + 7744).nOut(2048).build(), "fc2", "features")
				.addLayer("predictions", 
						new OutputLayer.Builder(LossFunctions.LossFunction.MCXENT)
						.nIn(2048).nOut(outputNum)
						.weightInit(WeightInit.RELU)
						.dropOut(0.5)
						.activation(new ActivationSoftmax()).build(), "fc3")
				.setOutputs("predictions")
				.build();

		TransferLearningHelper transferLearningHelper = new TransferLearningHelper(net, featurizedLayer);
		ComputationGraph graph = transferLearningHelper.unfrozenGraph();

		// Stats visualisation on http://localhost:9000/train, and print score every 10th iteration.
		//		UIServer uiServer = UIServer.getInstance();
		//		StatsStorage statsStorage = new InMemoryStatsStorage();
		//		uiServer.attach(statsStorage);
		//		graph.setListeners(/*new StatsListener(statsStorage), */new ScoreIterationListener(10));

		List<RecordReader> readers = Arrays.asList(recordReader, featuresReader, outputReader);
		List<String> names = Arrays.asList("image", "features", "output");
		saveFeaturized(getMultiDataSetIterator(readers, names, trainData, batchSize, outputNum), transferLearningHelper, "train");
		saveFeaturized(getMultiDataSetIterator(readers, names, validData, batchSize, outputNum), transferLearningHelper, "validation");
		saveFeaturized(getMultiDataSetIterator(readers, names, testData, batchSize, outputNum), transferLearningHelper, "test");

		EarlyStoppingConfiguration<ComputationGraph> esConf = new EarlyStoppingConfiguration.Builder<ComputationGraph>()
				.epochTerminationConditions(new ScoreImprovementEpochTerminationCondition(20), new MaxEpochsTerminationCondition(nEpochs))
				.scoreCalculator(new DataSetLossCalculatorCG(getPresavedMultiIterator("validation"), true))
				.modelSaver(new LocalFileGraphSaver("/tmp"))
				.evaluateEveryNEpochs(1)
				.build();

		EarlyStoppingGraphFeaturizedTrainer trainer = new EarlyStoppingGraphFeaturizedTrainer(esConf, transferLearningHelper, getPresavedMultiIterator("train"));
		trainer.fit();

		Evaluation eval = graph.evaluate(getPresavedMultiIterator("test"), labels);
		log.info("Model evaluation:\n{}", eval.stats(true));

		File modelFile = new File("AdaptedVGG16-" + System.currentTimeMillis() + ".zip");
		ModelSerializer.writeModel(net, modelFile, true);
		log.info("Model saved to {}.", modelFile);
	}

	private static MultiDataSetIterator getPresavedMultiIterator(String name) {
		MultiDataSetIterator existingTestData = new ExistingMiniBatchMultiDataSetIterator(new File(name + "Folder"), "images-" + featurizedLayer + "-" + name + "-%d.bin");
		MultiDataSetIterator asyncTestIter = new AsyncMultiDataSetIterator(existingTestData);
		return asyncTestIter;
	}

	private static void saveFeaturized(MultiDataSetIterator dataIter, TransferLearningHelper transferLearningHelper, String name) {
		int[] dataSaved = new int[]{ 0 };
		dataIter.forEachRemaining(mds -> {
			MultiDataSet currentFeaturized = transferLearningHelper.featurize((org.nd4j.linalg.dataset.MultiDataSet) mds);
			saveToDisk(currentFeaturized, dataSaved[0], name);
			dataSaved[0] = dataSaved[0] + 1;
		});
		dataIter.reset();
	}

	private static void saveToDisk(MultiDataSet currentFeaturized, int iterNum, String name) {
		File fileFolder = new File(name + "Folder");
		if (iterNum == 0) {
			fileFolder.mkdirs();
		}
		String fileName = "images-" + featurizedLayer + "-" + name + "-" + iterNum + ".bin";
		try {
			currentFeaturized.save(new File(fileFolder, fileName));
		} catch (IOException e) {
			log.error("Exception while saving file {}.", e, fileName);
		}
		log.info("Saved {} dataset #{}", name, iterNum);
	}

	public static MultiDataSetIterator getMultiDataSetIterator(List<RecordReader> recordReaders, List<String> names, InputSplit data, int batchSize, int outputNum) {
		if (recordReaders.size() != names.size())
			throw new IllegalArgumentException("The lists of recordReaders and of names must have the same size. "
					+ (recordReaders.size() + 1) + " recordReader(s), " + (names.size() + 1) + " names given.");
		RecordReaderMultiDataSetIterator.Builder builder = new RecordReaderMultiDataSetIterator.Builder(batchSize);
		try {
			for (int i = 0; i < recordReaders.size(); i++) {
				recordReaders.get(i).initialize(data);
				String name = names.get(i);
				RecordReader reader = recordReaders.get(i);
				builder.addReader(name, reader);
				if (reader instanceof ImageClassRecordReader)
					builder.addOutputOneHot(name, 0, outputNum);
				else
					builder.addInput(name);
			}
		} catch (IOException e) {
			log.error("Impossible to initialize recordReader.", e);
		} catch (InterruptedException e) {
			log.error("Initialization of recordReader interrupted.", e);
		}
		MultiDataSetIterator iterator = builder.build();
		MultiNormalizerMinMaxScaler scaler = new MultiNormalizerMinMaxScaler(-1, 1);
		scaler.fit(iterator);
		log.debug("Scaler fit");
		iterator.setPreProcessor(scaler);
		iterator.reset();
		return iterator;
	}
}
