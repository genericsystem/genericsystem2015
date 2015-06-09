package org.genericsystem.cache;

import java.util.Arrays;

import org.genericsystem.api.core.exceptions.ExistsException;
import org.genericsystem.cache.FileSystem.Directory;
import org.genericsystem.cache.FileSystem.File;
import org.genericsystem.cache.FileSystem.FileType;
import org.genericsystem.defaults.exceptions.InstanceValueClassConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class FileSystemTest extends AbstractTest {

	public void testUpdateRootDirectory() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		assert rootDirectory.isAlive();
		Directory rootDirectory2 = (Directory) rootDirectory.updateValue("rootDirectory2");
		assert rootDirectory2 != rootDirectory;
		assert rootDirectory2.getValue().equals("rootDirectory2");

		assert !rootDirectory.isAlive();
	}

	public void testUpdateRootDirectoryWithFile() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		rootDirectory.addFile("file");
		Directory rootDirectory2 = (Directory) rootDirectory.updateValue("rootDirectory2");
		assert rootDirectory2 != rootDirectory;
		assert !rootDirectory.isAlive();
	}

	//
	public void testUpdateDirectory() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		Directory directory = rootDirectory.addDirectory("directory");
		Directory rootDirectory2 = (Directory) rootDirectory.updateValue("rootDirectory2");
		assert rootDirectory2 != rootDirectory;
		assert !directory.isAlive();
	}

	public void testUpdateDirectoryWithFile() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		Directory directory = rootDirectory.addDirectory("directory");
		directory.addFile("file");
		Directory rootDirectory2 = (Directory) rootDirectory.updateValue("rootDirectory2");
		assert rootDirectory2 != rootDirectory;
		assert !directory.isAlive();
	}

	public void testUpdateFile() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		File file = rootDirectory.addFile("file");
		File file2 = (File) file.updateValue("file2");
		assert file2 != file;
		assert !file.isAlive();
	}

	public void testDirectoryNameNotUniqueInDifferentDirectories() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		Directory directory1 = rootDirectory.addDirectory("directory1");
		final Directory directory2 = rootDirectory.addDirectory("directory2");
		assert !directory2.addDirectory("directory1").equals(directory1); // No Exception
		Cache cache = engine.getCurrentCache();
		engine.getCurrentCache().mount();

		catchAndCheckCause(() -> directory2.addDirectory("directory1"), ExistsException.class); // Exception

		directory1.addFile("fileName", new byte[] { Byte.MAX_VALUE });
		assert Arrays.equals(directory1.getFile("fileName").getContent(), new byte[] { Byte.MAX_VALUE });
		directory1.getFile("fileName").remove();
	}

	public void testFileNameNotUniqueInDifferentDirectories() {
		Engine engine = new Engine(FileSystem.class);

		FileSystem fileSystem = engine.find(FileSystem.class);
		Directory rootDirectory = fileSystem.addRootDirectory("rootDirectory");
		Directory directory1 = rootDirectory.addDirectory("directory1");
		Directory directory2 = rootDirectory.addDirectory("directory2");
		File file1 = directory1.addFile("test.hmtl", "<html/>".getBytes());
		File file2 = directory2.addFile("test.hmtl", "<html/>".getBytes());// No Exception
		assert file1 != file2;
	}

	public void testFileNameValueClassViolation() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem directoryTree = engine.find(FileSystem.class);
		final Directory rootDirectory = directoryTree.addRootDirectory("rootDirectory");
		final FileType fileSystem = engine.find(FileType.class);

		catchAndCheckCause(() -> rootDirectory.addHolder(fileSystem, 2L), InstanceValueClassConstraintViolationException.class); // Exception
	}

	// Modifier par rapport au test d'origine
	public void testGetRootDirectories() {
		Engine engine = new Engine(FileSystem.class);
		FileSystem fileSystem = engine.find(FileSystem.class);
		// System.out.println("fileSystem " + fileSystem.info());
		Directory root = fileSystem.addRootDirectory("root");
		// System.out.println("root " + root.info());
		assert root.equals(fileSystem.getRootDirectory("root")) : fileSystem.getRootDirectories().info();
		assert fileSystem.getRootDirectories().stream().findFirst().get().equals(root);
	}
}
