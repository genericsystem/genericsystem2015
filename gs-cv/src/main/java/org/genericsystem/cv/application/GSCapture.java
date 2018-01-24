package org.genericsystem.cv.application;

interface GSCapture {

	SuperFrameImg read();

	void release();

}