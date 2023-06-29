var Spaces = function(){
    var self = this;
    var injectionArea = document.querySelector("#spaces-injection");
    var form = injectionArea.querySelector("form.editor");
    var init = false;
    self.path = window.location.pathname;
    self.editor = null;

    if(document.querySelector("meta[name=path]"))
        self.path = document.querySelector("meta[name=path]").getAttribute("content");

    var initEdit = function(){
        if(!form){
            form = document.createElement("form");
            form.classList.add("editor");
            form.setAttribute("method", "post");
            form.setAttribute("action", "/api/spaces/save");
            form.innerHTML = `\
<textarea name="content"></textarea>
<input type="hidden" name="path" value="index.html">
<input type="hidden" name="browser" value="true">
<input type="submit" value="Save">`;
            injectionArea.appendChild(form);

            var uploader = document.createElement("form");
            uploader.classList.add("uploader");
            uploader.setAttribute("method", "post");
            uploader.setAttribute("action", "/api/spaces/upload");
            uploader.innerHTML = `\
<input type="file" name="files[]" multiple required>
<input type="hidden" name="browser" value="true">
<input type="submit" value="Upload">`;
            injectionArea.appendChild(uploader);
        }
        form.querySelector("input[name=path]").value = self.path;
    };

    var loadPage = function(url, data){
        return new Promise(function(ok, fail){
            var request = new XMLHttpRequest();
            request.addEventListener("error", fail);
            request.addEventListener("load", function(ev){
                if(request.status == 200)
                    ok(request.response);
                else
                    fail(ev);
            });
            request.open("POST", url);
            request.send(data);
        });
    };

    var loadTag = function(tag, attrs){
        return new Promise(function(ok, fail){
            var el = document.createElement(tag);
            el.setAttribute("crossorigin", "anonymous");
            el.setAttribute("referrerpolicy", "no-referrer");
            el.addEventListener("error", fail);
            el.addEventListener("load", ok);
            for (const [key, value] of Object.entries(attrs)) {
                el.setAttribute(key, value);
            }
            injectionArea.appendChild(el);
        });
    };

    var initDependencies = function(){
        init = true;
        loadTag('link', {href: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/codemirror.min.css", rel: "stylesheet", integrity:"sha512-uf06llspW44/LZpHzHT6qBOIVODjWtv4MxCricRxkzvopAlSWnTf6hpZTFxuuZcuNE9CBQhqE0Seu1CoRk84nQ=="});
        loadTag('link', {href: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/theme/monokai.min.css", rel: "stylesheet", integrity:"sha512-R6PH4vSzF2Yxjdvb2p2FA06yWul+U0PDDav4b/od/oXf9Iw37zl10plvwOXelrjV2Ai7Eo3vyHeyFUjhXdBCVQ=="});
        return loadTag('script', {src: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/codemirror.min.js", integrity: "sha512-8RnEqURPUc5aqFEN04aQEiPlSAdE0jlFS/9iGgUyNtwFnSKCXhmB6ZTNl7LnDtDWKabJIASzXrzD0K+LYexU9g=="})
            .then(function(){return Promise.all([
                loadTag('script', {src: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/xml/xml.min.js", integrity: "sha512-LarNmzVokUmcA7aUDtqZ6oTS+YXmUKzpGdm8DxC46A6AHu+PQiYCUlwEGWidjVYMo/QXZMFMIadZtrkfApYp/g=="}),
                loadTag('script', {src: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/css/css.min.js", integrity: "sha512-rQImvJlBa8MV1Tl1SXR5zD2bWfmgCEIzTieFegGg89AAt7j/NBEe50M5CqYQJnRwtkjKMmuYgHBqtD1Ubbk5ww=="}),
                loadTag('script', {src: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/javascript/javascript.min.js", integrity: "sha512-I6CdJdruzGtvDyvdO4YsiAq+pkWf2efgd1ZUSK2FnM/u2VuRASPC7GowWQrWyjxCZn6CT89s3ddGI+be0Ak9Fg=="}),
                loadTag('script', {src: "https://cdnjs.cloudflare.com/ajax/libs/codemirror/6.65.7/mode/htmlmixed/htmlmixed.min.js", integrity: "sha512-HN6cn6mIWeFJFwRN9yetDAMSh+AK9myHF1X9GlSlKmThaat65342Yw8wL7ITuaJnPioG0SYG09gy0qd5+s777w=="}),
            ]);});
    };

    var initEditor = function(){
        injectionArea.classList.add("open");
        if(self.editor == null){
            self.editor = CodeMirror.fromTextArea(form.querySelector("textarea"), {
                lineNumbers: true,
                lineWrapping: true,
                autofocus: true,
                mode: "htmlmixed",
                theme: "monokai"
            });
            form.addEventListener("submit", function(){
                form.querySelector("textarea").value = self.editor.getValue();
            });
            return loadPage(window.location.href)
                .then(function(v){self.editor.setValue(v); return self.editor;});
        }
        return Promise.resolve(self.editor);
    };

    initEdit();
    injectionArea.addEventListener("click", function(ev){ ev.stopPropagation(); });
    injectionArea.querySelector("a.edit").addEventListener("click", function(ev){
        ev.preventDefault();
        if(!init)
            initDependencies().then(initEditor);
        else
            initEditor();

        var listen;
        listen = function(){
            document.removeEventListener("click", listen);
            injectionArea.classList.remove("open");
        };
        document.addEventListener("click", listen);
    });

    return self;
};
window.addEventListener('DOMContentLoaded', function(){
    window.spaces = new Spaces();
});
