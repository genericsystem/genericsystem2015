var video = document.querySelector('video');
var canvas = document.querySelector('canvas');
var context = canvas.getContext('2d');
var img = document.querySelector('img');
var streaming = false;
let vc = null;
let width = 320;
let height = 240;


var url = "ws://192.168.1.10:8080/WScams/wsServer";
var socket = new WebSocket(url);

function onOpen(event){
}

function opencvIsReady() {

    socket.onopen=onOpen;
    socket.addEventListener('message',function(event){
        img.src=window.URL.createObjectURL(event.data);
        requestAnimationFrame(processVideo);
    });

	video.width=width;
	video.height=height;
    canvas.width=width;
    canvas.height=height;

    let mediaStream = new MediaStream();


    const constraints = { 
    		video:{
        width : {exact: 320},
        height : {exact: 240} }
    }


    if (streaming) return;
    navigator.mediaDevices.getUserMedia(constraints)
    .then(function(s) {
        stream = s;
        video.srcObject = s;
        video.play();
    })
    .catch(function(err) {
        console.log("An error occured! " + err);
    });

    video.addEventListener("canplay", function(ev){
        if (!streaming) {

            video.setAttribute("width", width);
            video.setAttribute("height", height);
            streaming = true;
            vc = new cv.VideoCapture(video);
        }
        startVideoProcessing();
    }, false);




}

let lastFilter = '';
let src = null;
let dstC1 = null;


function startVideoProcessing() {
    if (!streaming) { console.warn("Please startup your webcam"); return; }
    stopVideoProcessing();
    src = new cv.Mat(height, width, cv.CV_8UC4);
    dstC1 = new cv.Mat(height, width, cv.CV_8UC1);
    requestAnimationFrame(processVideo);
}

function stopVideoProcessing(){
    if (src != null && !src.isDeleted()) src.delete();
    if (dstC1 != null && !dstC1.isDeleted()) dstC1.delete();
}

//Afficher la webcam dans le canvas

function processVideo(){
    console.log('draw Image');
    vc.read(src);
    //let mat = new cv.Mat(height, width, cv.CV_8U);
    //cv.cvtColor(src, mat, cv.COLOR_RGBA2GRAY);
    //cv.adaptiveThreshold(mat, dstC1, 255, cv.ADAPTIVE_THRESH_GAUSSIAN_C, cv.THRESH_BINARY_INV, 9, 5);
    //mat.delete();
    cv.imshow("canvas", src);
    var canvasData = canvas.toDataURL('image/png',1);
    var decodeAstring = atob(canvasData.split(',')[1]);
    var charArray =[];
    for(var i=0; i<decodeAstring.length;i++){
        charArray.push(decodeAstring.charCodeAt(i));
    }
    socket.send( new Blob([new Uint8Array(charArray)],{
        type:'image/png'
    }));   

}