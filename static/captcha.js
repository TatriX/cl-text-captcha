var result = document.getElementById("result");
var answer = document.getElementById("answer");
var form = document.getElementById("captcha-form");
form.onsubmit = function() {
    try {
        var xhr = new XMLHttpRequest();
        xhr.open("PUT", "answer", true);
        xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
        xhr.onload = function(e) {
            if (this.status == 200) {
                result.className = "correct";
                result.textContent = "Correct";
                loadCaptcha();
                answer.value = "";
                answer.focus();
            } else {
                result.className = "incorrect";
                result.textContent = "Try again";
            }
        };
        xhr.send("answer=" + answer.value);
    } catch (e) {
        console.error(e);
    }
    return false;
};

var captcha = document.getElementById("captcha");

function loadCaptcha() {
    try {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", "question", true);
        xhr.onload = function(e) {
            if (this.status == 200) {
                captcha.className = "";
                captcha.value = this.responseText;
            } else {
                captcha.className = "incorrect";
                captcha.value = "Cannot get captcha";
            }
        };
        xhr.send(null);
    } catch (e) {
        console.error(e);
    }
}

var refresh = document.getElementById("refresh");
refresh.onmousedown = function(e) {
    e.preventDefault();
    loadCaptcha();
    return false;
};

loadCaptcha();
answer.focus();
