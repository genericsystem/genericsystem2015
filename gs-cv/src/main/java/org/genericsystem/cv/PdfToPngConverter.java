package org.genericsystem.cv;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;

import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.pdfbox.pdmodel.PDPage;
import org.apache.pdfbox.rendering.ImageType;
import org.apache.pdfbox.rendering.PDFRenderer;
import org.apache.pdfbox.tools.imageio.ImageIOUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PdfToPngConverter {

	private final static Logger logger = LoggerFactory.getLogger(PdfToPngConverter.class);
	private final static String pdfDirectoryPath = "pdf";
	private final static String pngDirectoryPath = "png";

	public static void main(String[] args) {
		convertPdfToPng(pdfDirectoryPath, pngDirectoryPath);
	}

	static void convertPdfToPng(String sourceDirectoryName, String destinationDirectoryName) {
		File destinationDirectory = new File(destinationDirectoryName);
		destinationDirectory.mkdirs();
		for (File image : new File(sourceDirectoryName).listFiles())
			if (image.getName().endsWith(".pdf"))
				convertPdfToImages(image, destinationDirectory);
	}

	public static List<Path> convertPdfToImages(File pdfFile, File destinationDirectory) {
		try {
			destinationDirectory.mkdirs();
			List<Path> results = new ArrayList<>();
			PDDocument document = PDDocument.load(new FileInputStream(pdfFile));
			PDFRenderer pdfRenderer = new PDFRenderer(document);
			String fileName = pdfFile.getName().replace(".pdf", "");

			int pageCounter = 0;
			for (PDPage page : document.getPages()) {
				logger.debug("Extracting an image from file {}.", pdfFile);
				BufferedImage bim = pdfRenderer.renderImageWithDPI(pageCounter, 300, ImageType.RGB);
				Path newFile = destinationDirectory.toPath().resolve(fileName + "-" + pageCounter++ + ".png");
				ImageIOUtil.writeImage(bim, newFile.toString(), 300);
				results.add(newFile);
			}
			document.close();
			return results;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
}