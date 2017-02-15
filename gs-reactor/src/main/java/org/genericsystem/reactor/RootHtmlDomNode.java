package org.genericsystem.reactor;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

public class RootHtmlDomNode extends HtmlDomNode {
	private final Map<String, HtmlDomNode> nodeById = new HashMap<>();
	private final Sender send;
	private final String rootId;

	public RootHtmlDomNode(Context rootModelContext, RootTag rootTag, String rootId, Sender send) {
		super(null, rootModelContext, rootTag);
		this.rootId = rootId;
		this.send = send;
		sendAdd(0);
		init(0);
	}

	@Override
	public Sender getSender() {
		return send;
	}

	@Override
	protected RootHtmlDomNode getRootHtmlDomNode() {
		return this;
	}

	@Override
	public String getParentId() {
		return rootId;
	}

	private Map<String, HtmlDomNode> getMap() {
		return nodeById;
	}

	public HtmlDomNode getNodeById(String id) {
		return getMap().get(id);
	}

	public void add(String id, HtmlDomNode domNode) {
		getMap().put(id, domNode);
	}

	public void remove(String id) {
		getMap().remove(id);
	}

	public String header() {
		String header = "";
		String appName = getTag().getClass().getSimpleName().toLowerCase();
		header = "<!DOCTYPE html>\n";
		header += "<html>\n";
		header += "<head>\n";
		header += "<meta charset=\"UTF-8\" name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n";
		header += "<LINK rel=stylesheet type=\"text/css\" href=\"" + appName + ".css\"/>\n";
		header += "<LINK rel=stylesheet type=\"text/css\" href=\"reactor.css\"/>\n";
		header += "<script>\n";
		header += "var serviceLocation = \"ws://\" + document.location.host + \"" + "\";\n";
		header += "</script>\n";
		header += "<script type=\"text/javascript\" src=\"" + appName + ".js\"></script>\n";
		header += "</head>\n";
		header += "<body onload=\"connect();\" id=\"root\">\n";
		return header;
	}

	public String footer() {
		return "</body>\n</html>\n";
	}

	public void toHtmlFile(String sourceCode, String extension, String path) {
		BufferedWriter writer = null;
		try {
			writer = new BufferedWriter(new FileWriter(path + "index." + extension));
			writer.write(sourceCode);
			writer.close();
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}
}