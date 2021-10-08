package io.choerodon.agile;

import io.choerodon.resource.annoation.EnableChoerodonResourceServer;
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
public class AgileServiceApplication {
    
    public static void main(String[] args) {
        SpringApplication.run(AgileServiceApplication.class);
    }
    
    
    
}
