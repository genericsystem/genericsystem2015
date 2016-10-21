package org.genericsystem.reactor.model;

import java.lang.annotation.Annotation;
import java.util.function.BiConsumer;

import org.genericsystem.reactor.Tag;

public interface ProcessAnnotation extends BiConsumer<Annotation, Tag> {

}
