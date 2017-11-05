package org.genericsystem.cv.utils;

import java.io.Serializable;
import java.util.function.Function;

import org.genericsystem.cv.Img;

/**
 * This functional interface extends {@link Function}. It takes an {@link Img} as an input, apply a given filter (defined with a lambda) and outputs a transformed {@link Img}.
 * 
 * @author Pierrik Lassalas
 */
@FunctionalInterface
public interface ImgFunction extends Function<Img, Img>, Serializable {

}
