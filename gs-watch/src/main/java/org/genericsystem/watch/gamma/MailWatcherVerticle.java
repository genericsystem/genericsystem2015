package org.genericsystem.watch.gamma;

import java.io.File;
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

import com.sun.mail.imap.IMAPFolder;

import io.vertx.core.AbstractVerticle;
import io.vertx.core.Future;
import io.vertx.core.json.JsonObject;

public class MailWatcherVerticle extends AbstractVerticle {

	@Override
	public void start() throws Exception {
		vertx.executeBlocking(fut -> checkMail(fut), res -> {
			if (res.failed())
				vertx.eventBus().publish(Dispatcher.ADDRESS + ":watchMail", null);
		});
	}

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
			for(;;)
				inbox.idle();
		} catch (Exception e) {
			future.fail(e);
		}
	}

	private void processMessage(MimeMessage msg) {
		try {
			MimeMessageParser parser = new MimeMessageParser(msg).parse();
			System.out.println("> New email: " + parser.getSubject());
			for (DataSource attachment : parser.getAttachmentList()) {
				String contentType = attachment.getContentType().toLowerCase();
				if (contentType.contains("application/pdf") || contentType.contains("application/x-pdf")) {
					String fileName = attachment.getName();
					Path folder = Paths.get(DistributedVerticle.BASE_PATH + "pdf/");
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
					JsonObject task = new JsonObject().put(Dispatcher.STATE, Dispatcher.TODO)
							.put(DistributedVerticle.IP, LocalNet.getIpAddress())
							.put(DistributedVerticle.FILENAME, newFile.toString().replaceFirst(DistributedVerticle.BASE_PATH, ""))
							.put(DistributedVerticle.TYPE, PdfConverterVerticle.ACTION);
					vertx.eventBus().publish(Dispatcher.ADDRESS + ":add", task.encodePrettily());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
