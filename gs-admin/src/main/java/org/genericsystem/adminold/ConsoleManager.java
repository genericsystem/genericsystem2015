package org.genericsystem.adminold;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Vector;

import javax.swing.JScrollPane;
import javax.swing.JTextPane;
import javax.swing.event.DocumentEvent;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;

public class ConsoleManager extends javax.swing.JPanel implements javax.swing.event.DocumentListener {
	private static final long serialVersionUID = -170238346811873779L;

	private final JTextPane myTextPane;
	private final ConsoleWriter myWriter;

	public ConsoleManager() {
		setLayout(new BorderLayout());
		setPreferredSize(new Dimension(500, 500));
		myTextPane = new JTextPane();
		add(new JScrollPane(myTextPane));
		myTextPane.getDocument().addDocumentListener(this);
		myWriter = new ConsoleWriter(myTextPane.getDocument());
		System.setOut(new PrintStream(myWriter.asStream()));
		System.setErr(new PrintStream(myWriter.asStream()));
	}

	@Override
	public void insertUpdate(final DocumentEvent de) {
		final Document doc = de.getDocument();
		final int i1, i2;
		final int l;
		i1 = de.getOffset();
		i2 = de.getLength();
		l = doc.getLength();
		if (l == i1 + i2) {
			Element e1 = doc.getDefaultRootElement();
			Element e2 = e1.getElement(e1.getElementIndex(i1));
			while (!e2.isLeaf())
				e2 = e2.getElement(e2.getElementIndex(i1));
			final Element e3 = e2;
			AttributeSet as = e3.getAttributes();
			if (as.isDefined(myWriter))
				if (myTextPane.getCaretPosition() != l)
					myTextPane.setCaretPosition(l);
		}
	}

	@Override
	public void changedUpdate(DocumentEvent de) {
	}

	@Override
	public void removeUpdate(DocumentEvent de) {
	}

	static class ConsoleWriter extends java.io.Writer {
		private final Document document;
		private final Vector<String> buffer;

		public ConsoleWriter(Document document) {
			this.document = document;
			buffer = new Vector<>();
		}

		@Override
		public void write(char cbuf[], int off, int len) throws IOException {
			byte b[] = new byte[cbuf.length];
			for (int i = off; i < off + len; i++)
				b[i] = (byte) cbuf[i];
			buffer.addElement(new String(b, off, len, "utf8"));
		}

		@SuppressWarnings("unchecked")
		@Override
		public void flush() throws IOException {
			try {
				Vector<String> v;
				String ss[];
				final StringBuffer sb;

				v = (Vector<String>) buffer.clone();
				buffer.removeAllElements();
				ss = new String[v.size()];
				v.copyInto(ss);
				sb = new StringBuffer();
				for (int i = 0; i < ss.length; i++)
					sb.append(ss[i]);
				document.insertString(document.getLength(), sb.toString(), null);
			} catch (BadLocationException ble) {
				throw new IOException(ble.toString());
			} catch (Exception e) {
			}
		}

		@Override
		public void close() {
		}

		public OutputStream asStream() {
			return new Stream();
		}

		class Stream extends java.io.OutputStream {
			char buf[] = new char[1];

			@Override
			public void write(int b) throws IOException {
				buf[0] = (char) b;
				ConsoleWriter.this.write(buf, 0, 1);
				flush();
			}

			@Override
			public void write(byte b[], int off, int len) throws IOException {
				char c[];
				c = new char[b.length];
				for (int i = off; i < off + len; i++)
					c[i] = (char) b[i];
				ConsoleWriter.this.write(c, off, len);
				flush();
			}

			@Override
			public void flush() throws IOException {
				ConsoleWriter.this.flush();
			}
		}
	}
}
