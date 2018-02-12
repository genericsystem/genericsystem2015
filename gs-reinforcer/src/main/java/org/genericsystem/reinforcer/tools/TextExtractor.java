package org.genericsystem.reinforcer.tools;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.lang.invoke.MethodHandles;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.BiConsumer;
import java.util.stream.Stream;

import org.genericsystem.reinforcer.Label;
import org.genericsystem.reinforcer.Labels;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TextExtractor {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private final static String sourceDirPath = "pieces/json";
	private final static String targetDirPath = "pieces/text";
	private final static String textFilePath = "pieces/textData";

	// For each file in a subdirectory of sourceDirPath, creates a text file with the text contained in the 
	// document in a subdirectory of targetDirPath with the same name as the original subdirectory.
	// Creates a text file at textFilePath with the text from all the files.
	public static void main(String[] args) {
		extractText();
		extractTextFiles();
	}

	public static void forEachSubFile(Path sourceDir, BiConsumer<Path, Path> fileAction) {
		try (Stream<Path> stream = Files.list(sourceDir)) {
			stream.filter(path -> Files.isDirectory(path)).forEach(dir -> {
				try (DirectoryStream<Path> files = Files.newDirectoryStream(dir, path -> Files.isRegularFile(path))) {
					files.forEach(file -> fileAction.accept(dir, file));
				} catch (IOException e) {
					throw new RuntimeException(e);
				}
			});
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public static void extractText() {
		Path sourceDir = Paths.get(sourceDirPath);
		StringBuilder data = new StringBuilder();
		forEachSubFile(sourceDir, (dir, file) -> {
			Labels labels = Labels.from(file).sort();
			for (Label label : labels) {
				data.append(label.getText());
				data.append(" ");
			}
		});
		try (BufferedWriter bf = new BufferedWriter(new FileWriter(textFilePath))) {
			bf.write(data.toString());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	public static void extractTextFiles() {
		Path sourceDir = Paths.get(sourceDirPath);
		Path targetDir = Paths.get(targetDirPath);
		forEachSubFile(sourceDir, (dir, file) -> {
			StringBuilder data = new StringBuilder();
			Labels labels = Labels.from(file).sort();
			for (Label label : labels) {
				data.append(label.getText());
				data.append(" ");
			}
			Path saveFile = targetDir.resolve(sourceDir.relativize(file)).resolveSibling(file.getFileName().toString() + ".txt");
			try {
				Files.createDirectories(saveFile.getParent());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
			try (BufferedWriter bf = new BufferedWriter(new FileWriter(saveFile.toFile()))) {
				bf.write(data.toString());
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		});
	}
}
