package io.choerodon.agile.infra.utils;

import java.lang.reflect.Field;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author shinan.chen
 * @date 2018/9/28
 */
public class EnumUtil {

    private static final  Logger LOGGER = LoggerFactory.getLogger(EnumUtil.class);

    private EnumUtil(){
    }
    /**
     * 枚举类通用校验
     * @param cls cls
     * @param statusType statusType
     * @return result
     */
    public static Boolean contain(Class cls,String statusType){
        Field[] fields = cls.getDeclaredFields();
        for(Field field:fields){
            try {
                String type = String.valueOf(field.get(cls));
                if(type.equals(statusType)){
                    return true;
                }
            } catch (IllegalAccessException e) {
                LOGGER.error("IllegalAccessException", e);
            }
        }
        return false;
    }
}
