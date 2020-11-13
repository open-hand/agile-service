package io.choerodon.agile.infra.utils;

import org.springframework.beans.factory.NoSuchBeanDefinitionException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

/**
 * @author dinghuang123@gmail.com
 * @since 2018/10/31
 */
@Component
public class SpringBeanUtil implements ApplicationContextAware {

    private static ApplicationContext applicationContext;

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) {
        SpringBeanUtil.applicationContext = applicationContext;
    }

    public static <T> T getBean(Class<T> clazz) {
        return applicationContext.getBean(clazz);
    }

    public static <T> T getExpandBean(Class<T> clazz) {
        T t = null;
        try {
            t = applicationContext.getBean(clazz);
        }
        catch (NoSuchBeanDefinitionException e){
            t = null;
        }
        catch (Exception e){
            e.printStackTrace();
        }
        return t;
    }
}
