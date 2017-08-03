package org.genericsystem.cv.comparator;

import java.io.Serializable;
import java.util.function.Function;

import org.genericsystem.cv.Img;

@FunctionalInterface
public interface ImgFunction extends Function<Img, Img>, Serializable {

}
