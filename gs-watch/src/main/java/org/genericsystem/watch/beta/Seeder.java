package org.genericsystem.watch.beta;

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
import org.genericsystem.watch.MailWatcherVerticle;

import com.sun.mail.imap.IMAPFolder;

import io.vertx.core.Vertx;
import io.vertx.core.VertxOptions;
import io.vertx.core.eventbus.EventBusOptions;
import io.vertx.core.spi.cluster.ClusterManager;
import io.vertx.spi.cluster.hazelcast.HazelcastClusterManager;

public class Seeder extends DistributedVerticle {

	private static final String protocol = "imaps";
	private static final String host = "imap.gmail.com";
	private static final String file = "INBOX";
	private static final String username = "watchtestmwf";
	private static final String password = "WatchTestMWF4";
	private static final String pdfDir = System.getenv("HOME") + "/git/genericsystem2015/gs-cv/pdf";
	private static final String IP_ADDRESS = "192.168.1.11";

	public static void main(String[] args) {

		ClusterManager mgr = new HazelcastClusterManager();

		VertxOptions vertxOptions = new VertxOptions().setClustered(true).setClusterManager(mgr);
		vertxOptions.setEventBusOptions(new EventBusOptions()).setClustered(true);
		vertxOptions.setClusterHost(IP_ADDRESS);
		vertxOptions.setMaxWorkerExecuteTime(Long.MAX_VALUE);

		Vertx.clusteredVertx(vertxOptions, res -> {
			if (res.succeeded()) {
				Vertx vertx = res.result();
				vertx.deployVerticle(new Seeder(), result -> {
					System.out.println(result.result());

				});
			} else {
				throw new IllegalStateException(res.cause());
			}
		});
	}

	@Override
	public void start() throws Exception {

		System.out.println("start seeder");
		super.start();

		vertx.executeBlocking(future -> {
			System.out.println("checking mailbox");

			Properties props = System.getProperties();
			Session session = Session.getInstance(props, null);
			URLName url = new URLName(protocol, host, 993, file, username, password);
			Store store;
			try {
				store = session.getStore(url);
				store.connect();
				IMAPFolder inbox = (IMAPFolder) store.getFolder(url);
				inbox.open(Folder.READ_WRITE); // Folder.READ_WRITE to mark the
												// emails as read.

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
			} catch (MessagingException e) {
				e.printStackTrace();
			}

		}, res -> {
			if (res.failed())
				throw new IllegalStateException(res.cause());
		});

	}

	private void processMessage(MimeMessage msg) {
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
							newFile = File
									.createTempFile(fileNameParts[0] + "-", "." + fileNameParts[1], folder.toFile())
									.toPath();
							newFile.toFile().delete(); // We only want the
														// filename

						}
						Files.copy(attachment.getInputStream(), newFile);
					}
					System.gc();
					System.runFinalization();

					cache.safeConsum(nothing -> {
						addMessage(Paths.get(fileName), 1, System.currentTimeMillis(), TODO, 5);
					});

				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
