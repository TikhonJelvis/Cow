$(function () {
  $("span.tagged").mouseover(function () {
    var classNumber = $(this).attr('class').match(/n\d+/)[0];
    
    $("." + classNumber).css('background', '#99D'); 
  });

  $("span.tagged").mouseout(function () {
    var classNumber = $(this).attr('class').match(/n\d+/)[0];
    
    $("." + classNumber).css('background', ''); 
  });
  
  $("span.from").mouseover(function () {
    var idNumber = $(this).attr('class').match(/id\d+/)[0];
    
    $("." + idNumber + ".to").css("box-shadow", "5px 5px 5px #AAA");
  });
  $("span.from").mouseout(function () {
    var idNumber = $(this).attr('class').match(/id\d+/)[0];
    
    $("." + idNumber + ".to").css("box-shadow", "");
  });
  $("span.to").mouseover(function () {
    var idNumber = $(this).attr('class').match(/id\d+/)[0];
    
    console.log($("." + idNumber + ".from").css("box-shadow", "5px 5px 5px #AAA"));
    $("." + idNumber + ".from").css("box-shadow", "5px 5px 5px #AAA");
  });
  $("span.to").mouseover(function () {
    var idNumber = $(this).attr('class').match(/id\d+/)[0];
    
    $("." + idNumber + ".from").css("box-shadow", "");
  });
});