package org.genericsystem.ir;

import java.io.File;
import java.lang.invoke.MethodHandles;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

import javax.activation.DataSource;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.Session;
import javax.mail.Store;
import javax.mail.URLName;
import javax.mail.event.MessageCountAdapter;
import javax.mail.event.MessageCountEvent;
import javax.mail.internet.MimeMessage;
import javax.mail.search.FlagTerm;

import org.apache.commons.mail.util.MimeMessageParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.mail.imap.IMAPFolder;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

/**
 * The MailWatcherVerticle connects to an email account. For each unread mail each new mail received afterwards, the PDF attachments are extracted and stored locally. A message is then sent to the event bus to the {@link PdfConverterVerticle} to extract
 * each page of these files as PNG files.
 */
public class MailWatcherVerticle extends AbstractVerticle {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final String ip = LocalNet.getIpAddress();

	@Override
	public void start() throws Exception {
		vertx.executeBlocking(fut -> checkMail(fut), res -> {
			if (res.failed())
				vertx.eventBus().publish(Dispatcher.ADDRESS + ":watchMail", null);
		});
	}

	/**
	 * Check for new emails arriving in the mailbox defined by the configuration file src/main/conf/MailWatcherVerticle.json.
	 * 
	 * @param future - A future that is failed if there is an exception.
	 */
	private void checkMail(Future<Object> future) {
		Properties props = System.getProperties();
		Session session = Session.getInstance(props, null);
		JsonObject config = vertx.getOrCreateContext().config();
		URLName url = new URLName(config.getString("protocol"), config.getString("host"), config.getInteger("port"), config.getString("file"), config.getString("username"), config.getString("password"));
		Store store;
		IMAPFolder inbox = null;
		try {
			store = session.getStore(url);
			store.connect();
			inbox = (IMAPFolder) store.getFolder(url);
			inbox.open(Folder.READ_WRITE); // Folder.READ_WRITE to mark the emails as read.

			int start = 1;
			int end = inbox.getMessageCount();
			// Process unseen messages.
			while (start <= end) {
				Message[] msgs = inbox.search(new FlagTerm(new Flags(Flags.Flag.SEEN), false));
				for (Message msg : msgs)
					processMessage((MimeMessage) msg);
				// New messages that have arrived during processing.
				start = end + 1;
				end = inbox.getMessageCount();
			}

			// Listen for new messages.
			inbox.addMessageCountListener(new MessageCountAdapter() {
				@Override
				public void messagesAdded(MessageCountEvent ev) {
					Message[] msgs = ev.getMessages();
					for (Message msg : msgs)
						processMessage((MimeMessage) msg);
				}
			});
			for (;;)
				inbox.idle();
		} catch (Exception e) {
			future.fail(e);
		}
	}

	/**
	 * Process a message. Any pdf attachment is extracted in the folder specified in {@link #DistributedVerticle.BASE_PATH} + "downloaded-pdf/" ({@value #DistributedVerticle.BASE_PATH + "pdf/"}), and a message is published to the event bus.
	 * 
	 * @param msg - the message to process
	 */
	private void processMessage(MimeMessage msg) {
		try {
			MimeMessageParser parser = new MimeMessageParser(msg).parse();
			logger.info("> New email: {}.", parser.getSubject());
			for (DataSource attachment : parser.getAttachmentList()) {
				String contentType = attachment.getContentType().toLowerCase();
				if (contentType.contains("application/pdf") || contentType.contains("application/x-pdf")) {
					String fileName = attachment.getName();
					Path folder = Paths.get(DistributedVerticle.BASE_PATH + "downloaded-pdf/");
					folder.toFile().mkdirs();
					Path newFile = folder.resolve(fileName);
					synchronized (MailWatcherVerticle.class) {
						if (newFile.toFile().exists()) {
							String[] fileNameParts = fileName.split("\\.(?=[^\\.]+$)");
							newFile = File.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], folder.toFile()).toPath();
							newFile.toFile().delete(); // We only want the file name…
						}
						Files.copy(attachment.getInputStream(), newFile);
					}
					JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO).put(DistributedVerticle.IP, ip).put(DistributedVerticle.FILENAME, newFile.toString().replaceFirst(DistributedVerticle.BASE_PATH, ""))
							.put(DistributedVerticle.TYPE, PdfConverterVerticle.ACTION);
					logger.info("Saved attachment: {}", task.getString(DistributedVerticle.FILENAME));
					vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
				}
			}
		} catch (Exception e) {
			logger.error("Error while processing mail.", e);
		}
	}
}
