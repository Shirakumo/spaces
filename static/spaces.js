var Spaces = function(){
    var self = this;
    var injectionArea;
    var form;
    var init;
    var setupPage;
    self.path = null;
    self.editor = null;

    if(document.querySelector("meta[name=path]"))
        self.path = document.querySelector("meta[name=path]").getAttribute("content");
    
    var loadPage = function(url, data, options){
        return new Promise(function(ok, fail){
            var request = new XMLHttpRequest();
            for(const [key, value] of Object.entries(options || {})){
                request[key] = value;
            }
            request.onerror = function(ev){
                if(console) console.log("Spaces request failed", request, ev);
                fail(request, ev);
            };
            request.onload = function(ev){
                if(request.status == 200){
                    if(console) console.log("Spaces request succeeded", request);
                    ok(request.response);
                }else{
                    if(console) console.log("Spaces request failed", request, ev);
                    fail(request, ev);
                }
            };
            request.open("POST", url);
            request.send(data);
        });
    };

    var ajaxForm = function(form){
        return new Promise(function(ok, fail){
            form.addEventListener("submit", function(ev){
                ev.preventDefault();
                [].forEach.call(form.querySelectorAll("[type=submit]"), function(el){
                    el.disabled = true;
                });
                var data = new FormData(form);
                data.delete("browser");
                
                loadPage(form.getAttribute("action"), data, {responseType: 'json'})
                    .then(function(response){
                        [].forEach.call(form.querySelectorAll("[type=submit]"), function(el){
                            el.removeAttribute("disabled");
                        });
                        ok(response);
                    })
                    .catch(fail);
            });
        });
    };

    var standardAjaxForm = function(form){
        return ajaxForm(form)
            .catch(function(request){
                alert(request.response["message"]);
            });
    };

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
<input type="submit" value="Save" title="Saves the content on the server and refreshes the display.">`;
            injectionArea.appendChild(form);

            var uploader = document.createElement("form");
            uploader.classList.add("uploader");
            uploader.setAttribute("method", "post");
            uploader.setAttribute("action", "/api/spaces/upload");
            uploader.innerHTML = `\
<input type="file" name="files[]" multiple required>
<input type="hidden" name="browser" value="true">
<input type="submit" value="Upload" title="Uploads the file under its filename.">`;
            injectionArea.appendChild(uploader);
            ajaxForm(uploader);
        }
        form.querySelector("input[name=path]").value = self.path;
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

    var replacePageContent = function(content){
        if(console) console.log("Replacing page content...");
        // parse content
        let newDocument = document.implementation.createHTMLDocument();
        newDocument.open();
        newDocument.write(content);
        newDocument.close();
        
        // clear out all current page content **except** the injection.
        var prune;
        prune = function(root){
            // Have to copy children first as we'll be modifying during iteration.
            var children = [].slice.call(root.children);
            [].forEach.call(children, function(el){
                if(el === injectionArea){
                }else if(!el.contains(injectionArea)){
                    el.remove();
                }else{
                    prune(el);
                }
            });
        };
        prune(document);

        // reconstruct anxilliary page content around the injection.
        var transfer;
        var srcInjectionArea = newDocument.querySelector("#spaces-injection");
        transfer = function(src, dst){
            [].forEach.call(src.children, function(el){
                if(el === srcInjectionArea){
                }else if(!el.querySelector("#spaces-injection")){
                    dst.appendChild(el.cloneNode(true));
                }else{
                    var dstElement = el.cloneNode(false);
                    dst.appendChild(dstElement);
                    transfer(el, dstElement);
                }
            });
        };
        transfer(newDocument.children[0], document.children[0]);
        
        // Done.
    };

    var refreshEditor = function(){
        return loadPage(window.location.href).then(function(v){
            self.editor.setValue(v);
            return self.editor;
        });
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
                replacePageContent(self.editor.getValue());
            });
            ajaxForm(form);
            return refreshEditor();
        }
        return Promise.resolve(self.editor);
    };

    var initSidebar = function(){
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
    };

    setupPage = function(){
        injectionArea = document.querySelector("#spaces-injection");
        form = injectionArea.querySelector("form.editor");
        init = false;
        self.path = window.location.pathname;
        self.editor = null;

        if(document.querySelector("meta[name=path]"))
            self.path = document.querySelector("meta[name=path]").getAttribute("content");
        
        initEdit();
        initSidebar();
    };

    setupPage();
    return self;
};
window.addEventListener('DOMContentLoaded', function(){
    window.spaces = new Spaces();
});
