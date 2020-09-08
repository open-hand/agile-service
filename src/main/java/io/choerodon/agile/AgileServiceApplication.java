package io.choerodon.agile;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.ruiyun.jvppeteer.core.Puppeteer;
import com.ruiyun.jvppeteer.core.browser.Browser;
import com.ruiyun.jvppeteer.core.browser.BrowserFetcher;
import com.ruiyun.jvppeteer.core.page.ElementHandle;
import com.ruiyun.jvppeteer.core.page.JSHandle;
import com.ruiyun.jvppeteer.core.page.Page;
import com.ruiyun.jvppeteer.core.page.Response;
import com.ruiyun.jvppeteer.events.BrowserListener;
import com.ruiyun.jvppeteer.events.DefaultBrowserListener;
import com.ruiyun.jvppeteer.events.EventHandler;
import com.ruiyun.jvppeteer.events.Events;
import com.ruiyun.jvppeteer.options.*;
import io.choerodon.resource.annoation.EnableChoerodonResourceServer;
import org.hzero.boot.file.FileClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cloud.netflix.eureka.EnableEurekaClient;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;

@EnableChoerodonResourceServer
@EnableAsync
@EnableEurekaClient
@SpringBootApplication
@EnableCaching
@EnableScheduling
public class AgileServiceApplication implements CommandLineRunner {

    @Autowired
    private FileClient fileClient;

    public static void main(String[] args) {
        try {
            SpringApplication.run(AgileServiceApplication.class);
        }catch (Exception e){
            e.printStackTrace();
        }
    }


    @Override
    public void run(String... args) throws Exception {
        BrowserFetcher.downloadIfNotExist(null);
        ArrayList<String> argList = new ArrayList<>();
        LaunchOptions options = new LaunchOptionsBuilder().withArgs(argList).withHeadless(true).build();
        argList.add("--no-sandbox");
        argList.add("--disable-setuid-sandbox");
        Browser browser = Puppeteer.launch(options);
        Page page = browser.newPage();
        DefaultBrowserListener<Object> listener = new DefaultBrowserListener<>();
        listener.setMothod(Events.PAGE_DOMContentLoaded.getName());
        listener.setHandler(event -> {
            ScreenshotOptions options1 = new ScreenshotOptions();
            options1.setPath("contents.png");
            PDFOptions pdfOptions = new PDFOptions();
            pdfOptions.setPath("test.pdf");
//            options1.setFullPage(true);

//            ele.$$("img").stream().map(e ->{
//                e.
//            })

            try {
                Thread.sleep(2000);
                ElementHandle ele = page.$(".tui-editor-contents");
                ele.$$("img").forEach(e -> {
                    try {
                        JSHandle src = e.getProperty("src");
                        String url = (String) src.jsonValue();
                        System.out.println(url);
//                        src
//                        fileClient.downloadFile
                    } catch (JsonProcessingException jsonProcessingException) {
                        jsonProcessingException.printStackTrace();
                    }
                });
                page.pdf(pdfOptions);
                ele.screenshot(options1);
                System.out.println(111);
            } catch (Exception e) {
                e.printStackTrace();
            } finally {
                try {
                    page.close();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });
        listener.setIsSync(true);
        page.once(listener.getMothod(), listener);

        page.goTo("https://choerodon.com.cn/#/knowledge/share/8d56c3bd35de82ae");
        //设置截图范围

    }
}
