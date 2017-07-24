package org.genericsystem.watch;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

import javax.activation.DataSource;
import javax.mail.Flags;
import javax.mail.Folder;
import javax.mail.Message;
import javax.mail.MessagingException;
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

public class MailWatcherVerticle extends AbstractVerticle {

	// TODO: Store config in a config file.
	private static final String protocol = "imaps";
	private static final String host = "imap.gmail.com";
	private static final String file = "INBOX";
	private static final String username = "watchtestmwf";
	private static final String password = "WatchTestMWF4";
	private static final String pdfDir = "../gs-cv/pdf";

	private static final Long PERIODC_DELAY = 5000l;

	public static void main(String[] args) {
		VerticleDeployer.deployVerticle(new MailWatcherVerticle());
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public void start() throws Exception {
		vertx.executeBlocking(fut -> connectToInbox((Future) fut), res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
			else
				vertx.setPeriodic(PERIODC_DELAY, handler -> {
					// System.out.println("Periodic call");
					checkMail((IMAPFolder) res.result());
				});
		});
	}

	private void connectToInbox(Future<IMAPFolder> future) {
		Properties props = System.getProperties();
		Session session = Session.getInstance(props, null);
		URLName url = new URLName(protocol, host, 993, file, username, password);
		Store store;
		IMAPFolder inbox = null;
		try {
			store = session.getStore(url);
			store.connect();
			inbox = (IMAPFolder) store.getFolder(url);
		} catch (MessagingException e) {
			future.fail(e);
		}
		future.complete(inbox);
	}

	private void checkMail(IMAPFolder inbox) {
		// System.out.println(">>> mail watcher (check): " + Thread.currentThread().getName());

		if (inbox == null)
			return;
		try {
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
			inbox.close(false); // Close without purging the deleted messages on exit
		} catch (MessagingException e) {
			e.printStackTrace();
		}
	}

	private void processMessage(MimeMessage msg) {
		System.out.println(">>> mail watcher (process): " + Thread.currentThread().getName());
		try {
			MimeMessageParser parser = new MimeMessageParser(msg).parse();
			System.out.println("> New email: " + parser.getSubject());
			for (DataSource attachment : parser.getAttachmentList()) {
				String contentType = attachment.getContentType().toLowerCase();
				if (contentType.contains("application/pdf") || contentType.contains("application/x-pdf")) {
					String fileName = attachment.getName();
					Path folder = Paths.get(pdfDir);
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
					vertx.eventBus().publish(VerticleDeployer.PDF_WATCHER_ADDRESS, newFile.toString());
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
