package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.modelproperties.SelectionDefaults;

import java.lang.annotation.Annotation;

import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.DownloaderAnnotation;
import org.genericsystem.reactor.HtmlDomNode.RootHtmlDomNode;
import org.genericsystem.reactor.InfoAnnotation;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.annotations.AddCustomAnnotation;
import org.genericsystem.reactor.annotations.AddProcessAnnotation;

import io.vertx.core.http.ServerWebSocket;

public class GSApp extends GSDiv implements RootTag, SelectionDefaults {

	public GSApp() {
		super((Tag) null);
		createSelectionProperty();
	}

	@Override
	public void beforeProcessAnnotations() {
		Annotation annotations = getClass().getAnnotation(AddCustomAnnotation.class);
		if (annotations != null)
			for (Class<? extends Annotation> annotation : ((AddCustomAnnotation) annotations).value()) {
				Annotation annotationProccess = annotation.getAnnotation(AddProcessAnnotation.class);
				if (annotationProccess != null) {
					try {
						DownloaderAnnotation.getInstance().getInfoAnnotations().add(new InfoAnnotation(annotation, ((AddProcessAnnotation) annotationProccess).value().newInstance(), ((AddProcessAnnotation) annotationProccess).isRepeatable()));
					} catch (IllegalAccessException | InstantiationException e) {
						throw new IllegalStateException(e);
					}
				}
			}
	}

	@Override
	public RootHtmlDomNode init(Context rootModelContext, String rootId, ServerWebSocket webSocket) {
		return new RootHtmlDomNode(rootModelContext, this, rootId, webSocket);
	}

}
