package org.genericsystem.kernel;

import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.genericsystem.kernel.GenericHandler.SetArchiverHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Nicolas Feybesse
 * @author Michael Ory
 */
public class Archiver {

	public static final Logger log = LoggerFactory.getLogger(Archiver.class);

	private static final long ARCHIVER_COEFF = 5L;

	private static final String PATTERN = "yyyy.MM.dd_HH-mm-ss.SSS";
	private static final String MATCHING_REGEX = "[0-9]{4}.[0-9]{2}.[0-9]{2}_[0-9]{2}-[0-9]{2}-[0-9]{2}.[0-9]{3}---[0-9]+";

	protected static final String GS_EXTENSION = ".gs";
	protected static final String ZIP_EXTENSION = ".zip";
	private static final String PART_EXTENSION = ".part";
	private static final String LOCK_FILE_NAME = ".lock";

	private static final long SNAPSHOTS_PERIOD = 1000L;
	private static final long SNAPSHOTS_INITIAL_DELAY = 1000L;

	private final ScheduledExecutorService scheduler = Executors.newSingleThreadScheduledExecutor();

	protected final Root root;
	private final File directory;
	private FileLock lockFile;

	private final ZipFileManager zipFileManager = new ZipFileManager(new FileManager());

	public static String getFileExtension() {
		return GS_EXTENSION + ZIP_EXTENSION;
	}

	public Archiver(Root root, String directoryPath) {
		this.root = root;
		directory = prepareAndLockDirectory(directoryPath);
		if (directory != null) {
			String snapshotPath = getSnapshotPath(directory);
			if (snapshotPath != null) {
				try {
					getLoader(zipFileManager.getObjectInputStream(snapshotPath + getFileExtension())).loadSnapshot();
				} catch (IOException | ClassNotFoundException e) {
					log.error(e.getMessage(), e);
				}
			}
		}
		startScheduler();
	}

	protected Loader getLoader(ObjectInputStream objectInputStream) {
		return new Loader(objectInputStream);
	}

	protected Saver getSaver(ObjectOutputStream objectOutputStream, long ts) {
		return new Saver(objectOutputStream, ts);
	}

	private Archiver startScheduler() {
		if (directory != null && lockFile != null && SNAPSHOTS_PERIOD > 0L)
			scheduler.scheduleAtFixedRate(() -> {
				try {
					doSnapshot();
				} catch (IOException e) {
					log.error(e.getMessage(), e);
				}
			}, SNAPSHOTS_INITIAL_DELAY, SNAPSHOTS_PERIOD, TimeUnit.MILLISECONDS);
		return this;
	}

	public void close() {
		if (directory != null && lockFile != null) {
			scheduler.shutdown();
			try {
				doSnapshot();
				lockFile.close();
				lockFile = null;
			} catch (IOException e) {
				// TODO rollback here
				throw new IllegalStateException(e);
			}
		}
	}

	private void doSnapshot() throws IOException {
		long ts = root.pickNewTs();
		String fileName = directory.getAbsolutePath() + File.separator + getFilename(ts) + getFileExtension();
		String partFileName = fileName + PART_EXTENSION;
		getSaver(zipFileManager.getObjectOutputStream(partFileName, getFilename(ts) + GS_EXTENSION), ts).saveSnapshot();
		new File(partFileName).renameTo(new File(fileName));
		manageOldSnapshots(directory);
	}

	private void manageOldSnapshots(File directory) {
		NavigableMap<Long, File> snapshotsMap = snapshotsMap(directory, getFileExtension());
		long lastTs = snapshotsMap.lastKey();
		long firstTs = snapshotsMap.firstKey();
		long ts = firstTs;
		for (long snapshotTs : new TreeSet<>(snapshotsMap.keySet()))
			if (snapshotTs != lastTs && snapshotTs != firstTs)
				if ((snapshotTs - ts) < minInterval((lastTs - snapshotTs)))
					removeSnapshot(snapshotsMap, snapshotTs);
				else
					ts = snapshotTs;
	}

	private long minInterval(long periodNumber) {
		return (long) Math.floor(periodNumber / ARCHIVER_COEFF);
	}

	private void removeSnapshot(NavigableMap<Long, File> snapshotsMap, long ts) {
		snapshotsMap.get(ts).delete();
		snapshotsMap.remove(ts);
	}

	private File prepareAndLockDirectory(String directoryPath) {
		if (directoryPath == null)
			return null;
		File directory = new File(directoryPath);
		if (directory.exists()) {
			if (!directory.isDirectory())
				throw new IllegalStateException("Datasource path : " + directoryPath + " is not a directory");
		} else if (!directory.mkdirs())
			throw new IllegalStateException("Can't make directory : " + directoryPath);
		try {
			lockFile = new FileOutputStream(directoryPath + File.separator + LOCK_FILE_NAME).getChannel().tryLock();
			return directory;
		} catch (OverlappingFileLockException | IOException e) {
			throw new IllegalStateException("Locked directory : " + directoryPath);
		}
	}

	private String getSnapshotPath(File directory) {
		NavigableMap<Long, File> snapshotsMap = snapshotsMap(directory, getFileExtension());
		return snapshotsMap.isEmpty() ? null : directory.getAbsolutePath() + File.separator + getFilename(snapshotsMap.lastKey());
	}

	private static NavigableMap<Long, File> snapshotsMap(File directory, String extension) {
		NavigableMap<Long, File> snapshotsMap = new TreeMap<>();
		for (File file : directory.listFiles()) {
			String filename = file.getName();
			if (!file.isDirectory() && filename.endsWith(extension)) {
				filename = filename.substring(0, filename.length() - extension.length());
				if (filename.matches(MATCHING_REGEX))
					try {
						snapshotsMap.put(getTimestamp(filename), file);
					} catch (ParseException pe) {
						throw new IllegalStateException(pe);
					}
			}
		}
		return snapshotsMap;
	}

	private static long getTimestamp(final String filename) throws ParseException {
		return Long.parseLong(filename.substring(filename.lastIndexOf("---") + 3));
	}

	private static String getFilename(final long ts) {
		return new SimpleDateFormat(PATTERN).format(new Date(ts / Statics.MILLI_TO_NANOSECONDS)) + "---" + ts;
	}

	public class Saver {

		protected final ObjectOutputStream objectOutputStream;
		protected final Transaction transaction;

		protected Saver(ObjectOutputStream objectOutputStream, long ts) {
			this.objectOutputStream = objectOutputStream;
			this.transaction = buildTransaction(ts);
		}

		protected Transaction buildTransaction(long ts) {
			return new Transaction(root, ts);
		}

		public Transaction getTransaction() {
			return transaction;
		}

		private void saveSnapshot() throws IOException {
			writeDependencies(transaction.computeDependencies((Generic) root), new HashSet<>());
			objectOutputStream.flush();
			objectOutputStream.close();
		}

		private void writeDependencies(NavigableSet<Generic> dependencies, Set<Generic> vertexSet) throws IOException {
			for (Generic dependency : dependencies)
				if (vertexSet.add(dependency))
					writeDependency(dependency);
		}

		private void writeDependency(Generic dependency) throws IOException {
			writeAncestorId(dependency, dependency);
			writeOtherTs(dependency);
			objectOutputStream.writeObject(dependency.getValue());
			writeAncestorId(dependency, dependency.getMeta());
			writeAncestorsId(dependency, dependency.getSupers());
			writeAncestorsId(dependency, dependency.getComponents());
			// log.info("write dependency : " + dependency.info() + " " + dependency.getTs() + " birthTs : " + dependency.getLifeManager().getBirthTs());
		}

		protected void writeOtherTs(Generic dependency) throws IOException {
			objectOutputStream.writeLong(dependency.getLifeManager().getBirthTs());
			objectOutputStream.writeLong(dependency.getLifeManager().getLastReadTs());
			objectOutputStream.writeLong(dependency.getLifeManager().getDeathTs());
		}

		private void writeAncestorsId(Generic dependency, List<Generic> ancestors) throws IOException {
			objectOutputStream.writeInt(ancestors.size());
			for (Generic ancestor : ancestors)
				writeAncestorId(dependency, ancestor);
		}

		protected void writeAncestorId(Generic dependency, Generic ancestor) throws IOException {
			objectOutputStream.writeLong(ancestor != null ? ancestor.getTs() : dependency.getTs());
		}
	}

	protected class Loader {

		protected final ObjectInputStream objectInputStream;
		protected final Transaction transaction;

		protected Loader(ObjectInputStream objectInputStream) {
			this.objectInputStream = objectInputStream;
			this.transaction = root.newCache();
		}

		public Transaction getTransaction() {
			return transaction;
		}

		private void loadSnapshot() throws ClassNotFoundException, IOException {
			try {
				Map<Long, Generic> vertexMap = new HashMap<>();
				for (;;)
					loadDependency(vertexMap);
			} catch (EOFException ignore) {
			}
		}

		protected long loadTs() throws IOException {
			return objectInputStream.readLong();
		}

		protected long[] loadOtherTs() throws IOException {
			return new long[] { objectInputStream.readLong(), objectInputStream.readLong(), objectInputStream.readLong() };
		}

		protected void loadDependency(Map<Long, Generic> vertexMap) throws IOException, ClassNotFoundException {
			long ts = loadTs();
			long[] otherTs = loadOtherTs();
			if (otherTs[0] == LifeManager.TS_SYSTEM)
				otherTs[0] = LifeManager.TS_OLD_SYSTEM;
			Serializable value = (Serializable) objectInputStream.readObject();
			Generic meta = loadAncestor(ts, vertexMap);
			List<Generic> supers = loadAncestors(ts, vertexMap);
			List<Generic> components = loadAncestors(ts, vertexMap);
			vertexMap.put(ts, new SetArchiverHandler(ts, transaction, meta, supers, value, components, new LifeManager(otherTs)).resolve());
			// log.info("load dependency : " + vertexMap.get(ts).info() + " " + ts + " " + vertexMap.get(ts).getTs() + " birthTs : " + vertexMap.get(ts).getLifeManager().getBirthTs());
			assert getTransaction().isAlive(vertexMap.get(ts)) : vertexMap.get(ts).info();
		}

		protected List<Generic> loadAncestors(long ts, Map<Long, Generic> vertexMap) throws IOException {
			List<Generic> ancestors = new ArrayList<>();
			int sizeComponents = objectInputStream.readInt();
			for (int j = 0; j < sizeComponents; j++)
				ancestors.add(loadAncestor(ts, vertexMap));
			return ancestors;
		}

		protected Generic loadAncestor(long ts, Map<Long, Generic> vertexMap) throws IOException {
			long designTs = objectInputStream.readLong();
			Generic ancestor = vertexMap.get(designTs);
			assert ancestor != null || designTs == ts;
			return ancestor;
		}
	}

	protected static class ZipFileManager {

		private final FileManager fileManager;

		protected ZipFileManager(FileManager fileManager) {
			this.fileManager = fileManager;
		}

		protected ObjectOutputStream getObjectOutputStream(String zipFileName, String fileName) throws IOException {
			ZipOutputStream zipOutput = new ZipOutputStream(fileManager.getFileOutputStream(zipFileName));
			zipOutput.putNextEntry(new ZipEntry(fileName));
			return new ObjectOutputStream(zipOutput);
		}

		protected ObjectInputStream getObjectInputStream(String fileName) throws IOException {
			ZipInputStream inputStream = new ZipInputStream(fileManager.getFileInputStream(fileName));
			inputStream.getNextEntry();
			return new ObjectInputStream(inputStream);
		}
	}

	protected static class FileManager {

		protected FileOutputStream getFileOutputStream(String fileName) throws IOException {
			return new FileOutputStream(fileName);
		}

		protected FileInputStream getFileInputStream(String fileName) throws IOException {
			return new FileInputStream(new File(fileName));
		}
	}

}
