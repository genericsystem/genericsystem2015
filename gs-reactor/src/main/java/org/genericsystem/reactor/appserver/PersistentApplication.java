package org.genericsystem.reactor.appserver;

import java.lang.reflect.InvocationTargetException;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Root;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.HtmlDomNode.Sender;
import org.genericsystem.reactor.RootTag;
import org.genericsystem.reactor.annotations.RunScript;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PersistentApplication {

	private final Class<? extends RootTag> htmlAppClass;
	private RootTag tagTree;
	private final Root engine;
	private final Class<Context> modelClass;
	private final String rootId;
	private String indexHtml = "";
	protected static Logger log = LoggerFactory.getLogger(PersistentApplication.class);

	public PersistentApplication(Class<? extends RootTag> htmlAppClass, Class<Context> modelClass, Root engine, String rootId) {
		this.htmlAppClass = htmlAppClass;
		this.modelClass = modelClass;
		this.engine = engine;
		this.rootId = rootId;
		RunScript script = getApplicationClass().getAnnotation(RunScript.class);
		if (script != null) {
			AbstractCache cache = engine.newCache();
			log.info("Script will run ...");
			Script runner;
			try {
				runner = script.value().newInstance();
			} catch (InstantiationException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
			cache.safeConsum(unused -> runner.run(getEngine()));
			log.info("Script has run");
		}
		try {
			tagTree = getApplicationClass().newInstance();
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | SecurityException e) {
			try {
				engine.newCache().start();
				tagTree = getApplicationClass().getConstructor(Root.class).newInstance(engine);
				engine.getCurrentCache().flush();
			} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | SecurityException | InvocationTargetException | NoSuchMethodException ex) {
				throw new IllegalStateException(ex);
			}
		}
	}

	public String getRootId() {
		return rootId;
	}

	public Root getEngine() {
		return engine;
	}

	public Class<? extends RootTag> getApplicationClass() {
		return htmlAppClass;
	}

	public void close() {
		engine.close();
	}

	public String getIndexHtml() {
		return indexHtml;
	}

	public void setIndexHtml(String indexHtml) {
		this.indexHtml = indexHtml;
	}

	public RootHtmlDomNode init(Sender send) {
		try {
			return tagTree.init(modelClass.getConstructor(Root.class).newInstance(engine), rootId, send);
		} catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}
}
