package org.genericsystem.watch;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.genericsystem.cv.Classifier;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.ImgClass;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;

public class ClassifierVerticle extends FileCreateEventsHandlerVerticle {

	private static final String LOCK_FILE_NAME = ".lock";

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public ClassifierVerticle() {
		super(VerticleDeployer.PNG_WATCHER_ADDRESS);
	}

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new ClassifierVerticle());
	}

	@Override
	public void handle(Path newFile) {
		Mat img = Imgcodecs.imread(newFile.toString(), Imgcodecs.CV_LOAD_IMAGE_COLOR);
		Path matchingClassDir = null;
		Mat alignedImage = null;
		Path classesDirectory = Paths.get("..", "gs-cv", "classes");
		classesDirectory.toFile().mkdirs();
		try (RandomAccessFile raf = new RandomAccessFile(classesDirectory.resolve(LOCK_FILE_NAME).toString(), "rw"); FileChannel channel = raf.getChannel()) {
			channel.lock();

			try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(classesDirectory, Files::isDirectory)) {
				for (Path path : directoryStream) {
					ImgClass imgClass = new ImgClass(null, path.toString());
					Mat alignedImage_ = Classifier.compareFeature(img, imgClass.getMean().getSrc(), Classifier.MATCHING_THRESHOLD);
					if (alignedImage_ != null) {
						if (matchingClassDir != null)
							throw new IllegalStateException("Two matching classes found");
						matchingClassDir = path;
						alignedImage = alignedImage_;
					}
				}
			} catch (IOException e) {
				throw new IllegalStateException(e);
			}
			if (matchingClassDir == null) {
				matchingClassDir = classesDirectory.resolve(System.nanoTime() + "");
				matchingClassDir.toFile().mkdirs();
				try {
					alignedImage = new Img(img).cropAndDeskew().getSrc();
				} catch (Exception e) {
					throw new IllegalStateException(e);
				}
			}
			String[] fileNameParts = newFile.getFileName().toString().split("\\.(?=[^\\.]+$)");
			File savedFile;
			try {
				savedFile = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], matchingClassDir.toFile());
			} catch (IOException e) {
				throw new IllegalStateException(e);
			}
			Imgcodecs.imwrite(savedFile.toString(), alignedImage);
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}
