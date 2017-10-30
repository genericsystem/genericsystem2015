package org.genericsystem.cv;


import static org.testng.Assert.assertTrue;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.testng.annotations.Test;

import javafx.scene.layout.GridPane;

public class MatrixTest{

	private static final double[] vector = {0,1,2};
	private static final double[] vector2 = {3,4,5};
	private static final double[] vector3 = {1,1,1};
	private static final double[][] mat_2_3 = {vector, vector2};
	private static final double[][] mat_3_3 = {vector, vector2, vector3};
	
	
	static {
		NativeLibraryLoader.load();
	}

	
	@Test
	public void matConvertTest(){
				
		Mat opencvMat = new Mat(3,1,CvType.CV_32F);
		opencvMat.put(0, 0, 0d);
		opencvMat.put(1, 0, 1d);
		opencvMat.put(2, 0, 2d);
		
		Matrix convertedMatrix = Matrix.convert(opencvMat);
				
		Matrix m = new Matrix(vector);		
		
		assertTrue(m.equals(convertedMatrix));
	}
	
	
	@Test
	public void matrixConvertTest(){
				
		Mat opencvMat = new Mat(3,1,CvType.CV_32F);
		opencvMat.put(0, 0, 1d);
		opencvMat.put(1, 0, 2d);
		opencvMat.put(2, 0, 0d);
		
		Matrix matrix = new Matrix(3,1);
		matrix.set(0, 0, 1d);
		matrix.set(1, 0, 2d);
		matrix.set(2, 0, 0d);
		
		Mat convertedMatrix = matrix.convert();		
		
		//TODO Pas bien. A changer		
		assertTrue(Matrix.convert(opencvMat).equals(Matrix.convert(convertedMatrix)));
	}


	
	@Test
	public void normTest(){
		
		double[] array = {1,2,2};
		Matrix m = new Matrix(array);

		assertTrue(m.norm()==3);
		
	}
	
	@Test
	public void normalizeTest(){
		
		double[] array = {1,2,2};
		Matrix m = new Matrix(array);		
		
		Matrix normalized = new Matrix(3,1);
		normalized.set(0, 0, 1d/3);
		normalized.set(1, 0, 2d/3);
		normalized.set(2, 0, 2d/3);		
		
		assertTrue(m.normalize().equals(normalized));
		
	}
	
	@Test
	public void tTest(){
		
		Matrix matrix = new Matrix(mat_2_3);
		
		assertTrue(matrix.getm()==2);
		assertTrue(matrix.getn()==3);
		
		Matrix transposedMatrix = matrix.t();
		
		assertTrue(transposedMatrix.getm()==3);
		assertTrue(transposedMatrix.getn()==2);
		for(int i=0;i<matrix.getm();i++){
			for(int j=0;j<matrix.getn();j++){
				assertTrue(matrix.get(i, j) == transposedMatrix.get(j, i));
			}
		}				
	}
	
	@Test
	public void dotTest() {
		
		Matrix m1 = new Matrix(3,1);
		m1.set(0, 0, 1);
		m1.set(1, 0, 2);
		m1.set(2, 0, 3);
		
		Matrix m2 = new Matrix(3,1);
		m2.set(0, 0, 4);
		m2.set(1, 0, -5);
		m2.set(2, 0, 6);
		
		Matrix m3 = new Matrix(3,1);
		m3.set(0, 0, 1);
		m3.set(1, 0, 2);
		m3.set(2, 0, 1);
		
			
		assertTrue(m1.dot(m2)==12);
		assertTrue(m2.dot(m3)==0);
				
	}
	
	@Test
	public void timesTest(){
		
		Matrix m1 = new Matrix(mat_3_3);
		Matrix m2 = new Matrix(3,1);
		
		m2.set(0, 0, 1d);
		m2.set(1, 0, 1d);
		m2.set(2, 0, 1d);
		
		assertTrue(m1.times(m2, 1).getm()==m1.getm());
		assertTrue(m1.times(m2, 1).getn()==m2.getn());
		
		Matrix m3 = 	new Matrix(3,1);
		
		m3.set(0, 0, 3d);
		m3.set(1, 0, 12d);
		m3.set(2, 0, 3d);	
		
		assertTrue(m1.times(m2, 1).equals(m3));
				
	}
	
	@Test
	public void crossProduct(){
		
		Matrix m1 = new Matrix(3,1);
		m1.set(0, 0, 1);
		m1.set(1, 0, 2);
		m1.set(2, 0, 3);
		
		Matrix m2 = new Matrix(3,1);
		m2.set(0, 0, 4);
		m2.set(1, 0, -5);
		m2.set(2, 0, 6);
		
		Matrix m3 = new Matrix(3,1);
		m3.set(0, 0, 27d);
		m3.set(1, 0, 6d);
		m3.set(2, 0, -13d);		
		
		assertTrue(Matrix.crossProduct(m1,m2).equals(m3));
		
		
	}
	
}
