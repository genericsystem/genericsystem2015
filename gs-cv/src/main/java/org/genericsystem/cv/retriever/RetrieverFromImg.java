package org.genericsystem.cv.retriever;

import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.imgcodecs.Imgcodecs;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;

public class RetrieverFromImg {

	static {
		NativeLibraryLoader.load();
	}

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final ObjectMapper mapper = new ObjectMapper();
	private final Fields fields = new Fields();

	public static void main(String[] args) {
		new RetrieverFromImg().extractData();
	}

	private static final String sourceDirPath = "classes2";
	private static final String targetDirPath = "../gs-reinforcer/data";

	// Computes the fields for all images in first level subdirectories of sourceDir,
	// converts the data to JSON and stores it in targetDir, preserving the directory structure
	// and adding .json to the file name.
	public void extractData() {
		Path sourceDir = Paths.get(sourceDirPath);
		Path targetDir = Paths.get(targetDirPath);
		try (Stream<Path> stream = Files.list(sourceDir)) {
			stream.filter(path -> Files.isDirectory(path)).forEach(dir -> {
//				logger.debug("Handling dir {}", dir);
				try (DirectoryStream<Path> files = Files.newDirectoryStream(dir, path -> Files.isRegularFile(path))) {
					files.forEach(file -> {
//						logger.debug("Compute fields for file {}", file);
						computeFields(file, sourceDir, targetDir);
					});
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			});
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	private void computeFields(Path imgPath, Path sourceDir, Path targetDir) {
		Img img = new Img(Imgcodecs.imread(imgPath.toAbsolutePath().toString()));
		Img imgSmall = img.resize(1000);
		Mat frame = imgSmall.resize(1000).getSrc();
		ImgDescriptor stabilizedImgDescriptor = null;
		boolean stabilizationHasChanged = true;
		int stabilizationErrors = 0;
		int recoveringCounter = 0;
		Stats.beginTask("deperspectivation");
		//				Mat deperspectivGraphy = computeDeperspectivedHomography(frame, pp, f, mode);
		Mat deperspectivGraphy = Mat.eye(3, 3, CvType.CV_64FC1);
		Stats.endTask("deperspectivation");
		for (int i = 0; i < 20; i++) {
			try {
				if (deperspectivGraphy != null) {
					if (stabilizedImgDescriptor == null) {
						stabilizedImgDescriptor = new ImgDescriptor(frame, deperspectivGraphy);
						continue;
					}
					if (stabilizationHasChanged && stabilizationErrors > 10) {						
						fields.reset();
						stabilizationErrors = 0;
						stabilizedImgDescriptor = new ImgDescriptor(frame, deperspectivGraphy);
						continue;
					}

					Stats.beginTask("get img descriptors");
					ImgDescriptor newImgDescriptor = new ImgDescriptor(frame, deperspectivGraphy);
					Stats.endTask("get img descriptors");
					Stats.beginTask("stabilization homography");
					Mat betweenStabilizedHomography = stabilizedImgDescriptor.computeStabilizationGraphy(newImgDescriptor);
					// displayMat(betweenStabilizedHomography);
					Stats.endTask("stabilization homography");
					if (betweenStabilizedHomography != null) {
						stabilizationErrors = 0;

						Mat stabilizationHomographyFromFrame = new Mat();
						Core.gemm(betweenStabilizedHomography.inv(), deperspectivGraphy, 1, new Mat(), 0, stabilizationHomographyFromFrame);
						Img stabilized = CamLiveRetriever.warpPerspective(frame, stabilizationHomographyFromFrame);
						Img stabilizedDisplay = new Img(stabilized.getSrc(), true);
						if (stabilizationHasChanged && recoveringCounter == 0) {
							Stats.beginTask("stabilizationHasChanged");
							stabilized = newImgDescriptor.getDeperspectivedImg();
							stabilizedDisplay = new Img(stabilized.getSrc(), true);
							Stats.beginTask("restabilizeFields");
							fields.restabilizeFields(betweenStabilizedHomography);
							System.out.println("fields restabilized");
							Stats.endTask("restabilizeFields");
							stabilizedImgDescriptor = newImgDescriptor;
							stabilizationHomographyFromFrame = deperspectivGraphy;
							stabilizationHasChanged = false;
							Stats.endTask("stabilizationHasChanged");
						}
						Stats.beginTask("consolidate fields");
						fields.consolidate(stabilizedDisplay);
						Stats.endTask("consolidate fields");
						Stats.beginTask("performOcr");
						fields.performOcr(stabilized);
						Stats.endTask("performOcr");
					} else {
						stabilizationErrors++;
						logger.warn("Unable to compute a valid stabilization ({} times)", stabilizationErrors);
					}
				}
			} catch (Throwable e) {
				logger.warn("Exception while computing layout.", e);
			} finally {
				Stats.endTask("frame");
			}
		}
		Path file = targetDir.resolve(sourceDir.relativize(imgPath)).resolveSibling(imgPath.getFileName().toString() + ".json");
		logger.debug("Writing to file {}.", file);
		try {
			Files.createDirectories(file.getParent());
			mapper.writerWithDefaultPrettyPrinter().writeValue(file.toFile(), fields);
		} catch (IOException e) {
			throw new RuntimeException(e);
		} finally {
			imgSmall.close();
			img.close();
		}
	}
}
