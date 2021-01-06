package io.choerodon.agile.infra.utils;


import com.netflix.discovery.EurekaClient;
import com.netflix.discovery.shared.Application;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

/**
 * @author zhaotianxin
 * @date 2021-01-05 15:34
 */
@Component
public class FeignUtil {

    public final static String TEST_MANAGER_SERVICE = "test-manager-service";
    public final static String KNOWLEDGEBASE_SERVICE = "knowledgebase-service";

    @Autowired
    private EurekaClient eurekaClient;

    public Boolean isExist(String serviceName) {
        Application application = eurekaClient.getApplication(serviceName);
        Boolean exist = false;
        if (!ObjectUtils.isEmpty(application) && !CollectionUtils.isEmpty(application.getInstances())) {
            exist = true;
        }
        return exist;
    }
}
